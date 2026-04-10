# ============================================================================
# VECTORIZED PREVENTR PACKAGE FUNCTIONS
# ============================================================================
# High-performance vectorized implementation of the AHA PREVENT
# cardiovascular disease risk calculator.
#
# Supports all PREVENT model types:
#   - base:  standard 23-predictor model (24 with age_squared for 30yr)
#   - hba1c: base + HbA1c (dm / no dm / missing)
#   - uacr:  base + ln(UACR) + missing UACR indicator
#   - sdi:   base + SDI decile categories + missing SDI indicator
#   - full:  base + HbA1c + UACR + SDI (all enhanced predictors)
#
# Each model × sex × time horizon estimates 5 outcomes:
#   total_cvd, ascvd, heart_failure, chd, stroke
#
# Coefficients are loaded from prevent_coefficients.csv (long format).
# SDI decile lookup is loaded from sdi_decile_dat.csv.
#
# MAIN DEPENDENCIES: dplyr, purrr, rlang, tibble
#
# USAGE:
# source("functions/scores/preventr_vectorized.R")
# result <- estimate_risk_fast(data = df, time = "10yr", model = "base")
# ============================================================================

library(dplyr)

# --- UTILITY FUNCTIONS ---

convert_chol_to_mmol <- function(chol_mg_dl) {
  chol_mg_dl / 38.67
}

standardize_sex <- function(sex) {
  dplyr::case_when(
    sex %in% c("male", "m", "M", 0)   ~ "male",
    sex %in% c("female", "f", "F", 1) ~ "female",
    TRUE ~ as.character(sex)
  )
}

# --- SDI LOOKUP ---

#' Look up SDI decile from ZIP code (vectorised)
#' @param zip Character vector of 5-digit ZIP codes
#' @param sdi_dat Optional pre-loaded data frame with `zip` and `sdi_decile`
#' @return Numeric vector of SDI deciles (1-10); NA where ZIP is unknown
get_sdi <- function(zip, sdi_dat = NULL) {
  if (is.null(sdi_dat)) {
    sdi_dat <- .load_sdi_data()
  }
  if (is.null(sdi_dat)) {
    return(rep(NA_real_, length(zip)))
  }
  # Zero-pad ZIP codes to 5 digits (e.g. "1518" → "01518")
  zip_padded <- dplyr::if_else(
    !is.na(zip),
    sprintf("%05d", as.integer(zip)),
    NA_character_
  )
  lookup <- tibble::tibble(zip = zip_padded) |>
    dplyr::left_join(sdi_dat, by = "zip")
  lookup$sdi_decile
}

.load_sdi_data <- function() {
  candidates <- c(
    "sdi_decile_dat.csv",
    "scores/sdi_decile_dat.csv",
    file.path("functions", "scores", "sdi_decile_dat.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(readr::read_csv(p))
  }
  if (requireNamespace("preventr", quietly = TRUE)) {
    return(preventr:::sdi_decile_dat)
  }
  warning("SDI decile lookup data not found. SDI-based models will use NA.")
  NULL
}

#' Select the appropriate PREVENT model based on available inputs
#' Mirrors preventr:::select_model logic.
#' @keywords internal
select_prevent_model <- function(hba1c = NULL, uacr = NULL, zip = NULL) {
  has_hba1c <- !is.null(hba1c) && !all(is.na(hba1c))
  has_uacr  <- !is.null(uacr)  && !all(is.na(uacr))
  has_zip   <- !is.null(zip)   && !all(is.na(zip))

  if (!has_hba1c && !has_uacr && !has_zip) return("base")
  if (!has_hba1c &&  has_uacr && !has_zip) return("uacr")
  if ( has_hba1c && !has_uacr && !has_zip) return("hba1c")
  if (!has_hba1c && !has_uacr &&  has_zip) return("sdi")
  "full"
}

# --- COEFFICIENT TABLE ---

#' Load the long-format PREVENT coefficient CSV (cached)
#' Columns: model, time, sex, outcome, term, coefficient
#' @keywords internal
.prevent_coef_env <- new.env(parent = emptyenv())

get_prevent_coef_table <- function() {
  if (is.null(.prevent_coef_env$coefs)) {
    candidates <- c(
      "prevent_coefficients.csv",
      "scores/prevent_coefficients.csv",
      file.path("functions", "scores", "prevent_coefficients.csv")
    )
    loaded <- FALSE
    for (p in candidates) {
      if (file.exists(p)) {
        .prevent_coef_env$coefs <- read.csv(p, stringsAsFactors = FALSE)
        loaded <- TRUE
        break
      }
    }
    if (!loaded) stop("prevent_coefficients.csv not found. Place it beside this script.")
  }
  .prevent_coef_env$coefs
}

# --- VECTORIZED RISK CALCULATION ---

#' Calculate PREVENT CVD Risk (Vectorized, All Models)
#'
#' @param data A data frame with patient data
#' @param age_var,sex_var,sbp_var,bp_tx_var,total_c_var,hdl_c_var,statin_var,dm_var,smoking_var,egfr_var,bmi_var
#'   Column names (unquoted) for the 11 required base predictors.
#' @param hba1c_var Column name for HbA1c (optional; unquoted)
#' @param uacr_var  Column name for UACR mg/g (optional; unquoted)
#' @param zip_var   Column name for 5-digit ZIP code (optional; unquoted)
#' @param time Time horizon: "10yr", "30yr", or "both"
#' @param chol_unit Cholesterol units: "mg/dL" or "mmol/L"
#' @param model "base", "hba1c", "uacr", "sdi", "full", or NULL (auto-select)
#' @param quiet Suppress messages
#' @return A data frame with risk estimates appended (5 outcomes + model info)
#' @export
estimate_risk_fast <- function(data,
                               age_var     = age,
                               sex_var     = sex,
                               sbp_var     = sbp,
                               bp_tx_var   = bp_tx,
                               total_c_var = total_c,
                               hdl_c_var   = hdl_c,
                               statin_var  = statin,
                               dm_var      = dm,
                               smoking_var = smoking,
                               egfr_var    = egfr,
                               bmi_var     = bmi,
                               hba1c_var   = NULL,
                               uacr_var    = NULL,
                               zip_var     = NULL,
                               time        = "10yr",
                               chol_unit   = "mg/dL",
                               model       = NULL,
                               quiet       = FALSE) {

  # Capture variable names ----
  age_col     <- rlang::enquo(age_var)
  sex_col     <- rlang::enquo(sex_var)
  sbp_col     <- rlang::enquo(sbp_var)
  bp_tx_col   <- rlang::enquo(bp_tx_var)
  total_c_col <- rlang::enquo(total_c_var)
  hdl_c_col   <- rlang::enquo(hdl_c_var)
  statin_col  <- rlang::enquo(statin_var)
  dm_col      <- rlang::enquo(dm_var)
  smoking_col <- rlang::enquo(smoking_var)
  egfr_col    <- rlang::enquo(egfr_var)
  bmi_col     <- rlang::enquo(bmi_var)
  hba1c_col   <- rlang::enquo(hba1c_var)
  uacr_col_q  <- rlang::enquo(uacr_var)
  zip_col     <- rlang::enquo(zip_var)

  # Validate time ----
  stopifnot(time %in% c("10yr", "30yr", "both"))
  times_to_calc <- if (time == "both") c("10yr", "30yr") else time

  # Prepare data with internal column names ----
  prep_data <- data |>
    dplyr::mutate(
      .prev_age     = !!age_col,
      .prev_sex     = standardize_sex(!!sex_col),
      .prev_sbp     = !!sbp_col,
      .prev_bp_tx   = as.numeric(!!bp_tx_col),
      .prev_total_c = !!total_c_col,
      .prev_hdl_c   = !!hdl_c_col,
      .prev_statin  = as.numeric(!!statin_col),
      .prev_dm      = as.numeric(!!dm_col),
      .prev_smoking = as.numeric(!!smoking_col),
      .prev_egfr    = !!egfr_col,
      .prev_bmi     = !!bmi_col,
      .row_id       = dplyr::row_number()
    )

  # Optional enhanced variables ----
  if (!rlang::quo_is_null(hba1c_col)) {
    prep_data <- prep_data |> dplyr::mutate(.prev_hba1c = !!hba1c_col)
  } else {
    prep_data$.prev_hba1c <- NA_real_
  }
  if (!rlang::quo_is_null(uacr_col_q)) {
    prep_data <- prep_data |> dplyr::mutate(.prev_uacr = !!uacr_col_q)
  } else {
    prep_data$.prev_uacr <- NA_real_
  }
  if (!rlang::quo_is_null(zip_col)) {
    prep_data <- prep_data |> dplyr::mutate(.prev_zip = as.character(!!zip_col))
  } else {
    prep_data$.prev_zip <- NA_character_
  }

  # Convert cholesterol if supplied in mmol/L ----
  if (tolower(chol_unit) %in% c("mmol/l", "mmoll", "mmol_l")) {
    prep_data <- prep_data |>
      dplyr::mutate(
        .prev_total_c = .prev_total_c * 38.67,
        .prev_hdl_c   = .prev_hdl_c   * 38.67
      )
  }

  # Auto-select model if NULL ----
  if (is.null(model)) {
    model <- select_prevent_model(
      hba1c = prep_data$.prev_hba1c,
      uacr  = prep_data$.prev_uacr,
      zip   = prep_data$.prev_zip
    )
  }
  stopifnot(model %in% c("base", "hba1c", "uacr", "sdi", "full"))
  if (!quiet) message("PREVENT model: ", model)

  # Calculate risk for each time horizon ----
  results_list <- purrr::map(times_to_calc, function(th) {
    .calculate_prevent_risk(prep_data, time = th, model = model, quiet = quiet)
  })

  if (time == "both") {
    names(results_list) <- paste0("risk_est_", times_to_calc)
    return(results_list)
  }
  results_list[[1]]
}

#' Internal vectorized risk calculation (all models, all outcomes)
#' @keywords internal
.calculate_prevent_risk <- function(data, time, model, quiet = FALSE) {

  all_coefs <- get_prevent_coef_table()

  # Filter coefficients to this model + time
  coefs_long <- all_coefs |>
    dplyr::filter(.data$model == .env$model, .data$time == .env$time)

  outcomes <- unique(coefs_long$outcome)

  # Build predictor columns (vectorised) ----
  pred_data <- data |>
    dplyr::mutate(
      # Cholesterol in mmol/L
      .non_hdl_mmol = convert_chol_to_mmol(.prev_total_c - .prev_hdl_c),
      .hdl_mmol     = convert_chol_to_mmol(.prev_hdl_c),

      # Base predictors (centred/scaled per preventr:::prep_terms)
      age           = (.prev_age - 55) / 10,
      age_squared   = age^2,
      non_hdl_c     = .non_hdl_mmol - 3.5,
      hdl_c         = (.hdl_mmol - 1.3) / 0.3,
      sbp_lt_110    = (pmin(.prev_sbp, 110) - 110) / 20,
      sbp_gte_110   = (pmax(.prev_sbp, 110) - 130) / 20,
      dm            = .prev_dm,
      smoking       = .prev_smoking,
      bmi_lt_30     = (pmin(.prev_bmi, 30) - 25) / 5,
      bmi_gte_30    = (pmax(.prev_bmi, 30) - 30) / 5,
      egfr_lt_60    = (pmin(.prev_egfr, 60) - 60) / -15,
      egfr_gte_60   = (pmax(.prev_egfr, 60) - 90) / -15,
      bp_tx         = .prev_bp_tx,
      statin        = .prev_statin,

      # Interaction terms
      bp_tx_sbp_gte_110 = bp_tx * sbp_gte_110,
      statin_non_hdl_c  = statin * non_hdl_c,
      age_non_hdl_c     = age * non_hdl_c,
      age_hdl_c         = age * hdl_c,
      age_sbp_gte_110   = age * sbp_gte_110,
      age_dm            = age * dm,
      age_smoking       = age * smoking,
      age_bmi_gte_30    = age * bmi_gte_30,
      age_egfr_lt_60    = age * egfr_lt_60,

      # Constant
      constant = 1
    )

  # Enhanced predictor columns (only computed for non-base models) ----
  if (model != "base") {
    sdi_decile <- get_sdi(pred_data$.prev_zip)

    pred_data <- pred_data |>
      dplyr::mutate(
        # SDI indicators
        .sdi_decile  = sdi_decile,
        sdi_4_to_6   = as.numeric(!is.na(.sdi_decile) & .sdi_decile >= 4 & .sdi_decile <= 6),
        sdi_7_to_10  = as.numeric(!is.na(.sdi_decile) & .sdi_decile >= 7 & .sdi_decile <= 10),
        missing_sdi  = as.numeric(is.na(.sdi_decile)),

        # UACR
        ln_uacr      = dplyr::if_else(!is.na(.prev_uacr), log(.prev_uacr), 0),
        missing_uacr = as.numeric(is.na(.prev_uacr)),

        # HbA1c (centred at 5.3, split by DM status)
        hba1c_dm     = dplyr::if_else(!is.na(.prev_hba1c) & dm == 1,
                                      .prev_hba1c - 5.3, 0),
        hba1c_no_dm  = dplyr::if_else(!is.na(.prev_hba1c) & dm == 0,
                                      .prev_hba1c - 5.3, 0),
        missing_hba1c = as.numeric(is.na(.prev_hba1c))
      )
  }

  # Remove age_squared for 10yr models (matches preventr:::run_models) ----
  if (time == "10yr") {
    pred_data$age_squared <- NULL
  }

  # The set of terms this model actually uses
  model_terms <- unique(coefs_long$term)

  # Compute risk per sex × outcome via matrix multiply ----
  results <- pred_data |>
    dplyr::group_by(.prev_sex) |>
    dplyr::group_modify(~ {
      current_sex <- unique(.y$.prev_sex)
      sex_coefs <- coefs_long |> dplyr::filter(sex == current_sex)

      # Build predictor matrix once (rows × terms)
      pred_matrix <- as.matrix(.x[, model_terms, drop = FALSE])

      # Compute risk for each outcome
      risk_cols <- purrr::map_dfc(outcomes, function(out) {
        cv <- sex_coefs |>
          dplyr::filter(outcome == out) |>
          dplyr::arrange(match(term, model_terms))
        coef_vec <- cv$coefficient
        names(coef_vec) <- cv$term
        coef_vec <- coef_vec[model_terms]

        log_odds <- pred_matrix %*% coef_vec
        risk     <- exp(log_odds) / (1 + exp(log_odds))
        tibble::tibble(!!out := round(as.numeric(risk), 3))
      })

      dplyr::bind_cols(.x, risk_cols) |>
        dplyr::mutate(
          model      = .env$model,
          over_years = as.integer(gsub("yr", "", .env$time)),
          input_problems = NA_character_
        )
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(.row_id)

  # Drop internal working columns ----
  internal_cols <- c(
    ".prev_age", ".prev_sex", ".prev_sbp", ".prev_bp_tx", ".prev_total_c",
    ".prev_hdl_c", ".prev_statin", ".prev_dm", ".prev_smoking", ".prev_egfr",
    ".prev_bmi", ".prev_hba1c", ".prev_uacr", ".prev_zip",
    ".non_hdl_mmol", ".hdl_mmol", ".sdi_decile", ".row_id",
    model_terms
  )
  results <- results |>
    dplyr::select(-dplyr::any_of(internal_cols))

  if (!quiet) {
    message("PREVENT ", model, " model: ", time, " risk for ", nrow(results), " rows.")
  }
  results
}

#' Simplified wrapper with auto-detection of common variable names
#'
#' @param data Data frame with patient data
#' @param time Time horizon: "10yr", "30yr", or "both"
#' @param model Model type or NULL for auto-select
#' @param quiet Suppress messages
#' @return Data frame with original data plus risk estimates
#' @export
estimate_prevent_risk <- function(data, time = "10yr", model = NULL, quiet = FALSE) {

  # Variable name candidates (first match wins)
  var_map <- list(
    age     = c("age_prev", "age"),
    sex     = c("sex_prev", "sex", "gender"),
    sbp     = c("sbp_b", "sbp", "systolic"),
    bp_tx   = c("bp_meds_prev", "bp_tx", "bp_med", "htn_tx"),
    total_c = c("tc_b", "total_c", "total_chol", "tchol"),
    hdl_c   = c("hdl_b", "hdl_c", "hdl"),
    statin  = c("statin_prev", "statin"),
    dm      = c("diabetes_prev", "dm", "diabetes"),
    smoking = c("smoking_prev", "smoking", "smoker"),
    egfr    = c("egfr_b", "egfr_prev", "egfr"),
    bmi     = c("bmi_prev", "bmi")
  )
  opt_map <- list(
    hba1c = c("hba1c_prev", "hba1c", "a1c"),
    uacr  = c("uacr_prev", "uacr", "acr"),
    zip   = c("zip_prev", "zip", "zipcode")
  )

  detect <- function(candidates, required = TRUE) {
    found <- intersect(candidates, names(data))
    if (length(found) > 0) return(found[1])
    if (required) stop("Could not find column. Expected one of: ",
                       paste(candidates, collapse = ", "))
    NULL
  }

  detected     <- lapply(var_map, detect, required = TRUE)
  opt_detected <- lapply(opt_map, detect, required = FALSE)

  call_args <- list(
    data        = data,
    age_var     = rlang::sym(detected$age),
    sex_var     = rlang::sym(detected$sex),
    sbp_var     = rlang::sym(detected$sbp),
    bp_tx_var   = rlang::sym(detected$bp_tx),
    total_c_var = rlang::sym(detected$total_c),
    hdl_c_var   = rlang::sym(detected$hdl_c),
    statin_var  = rlang::sym(detected$statin),
    dm_var      = rlang::sym(detected$dm),
    smoking_var = rlang::sym(detected$smoking),
    egfr_var    = rlang::sym(detected$egfr),
    bmi_var     = rlang::sym(detected$bmi),
    time        = time,
    model       = model,
    quiet       = quiet
  )

  if (!is.null(opt_detected$hba1c)) call_args$hba1c_var <- rlang::sym(opt_detected$hba1c)
  if (!is.null(opt_detected$uacr))  call_args$uacr_var  <- rlang::sym(opt_detected$uacr)
  if (!is.null(opt_detected$zip))   call_args$zip_var   <- rlang::sym(opt_detected$zip)

  rlang::exec(estimate_risk_fast, !!!call_args)
}

# --- EXAMPLE USAGE ---
#
# # Base model (auto-detected variable names)
# result <- estimate_prevent_risk(df, time = "10yr")
#
# # Explicit variable mapping, all 5 models
# result <- estimate_risk_fast(
#   data = my_data,
#   age_var = patient_age, sex_var = patient_sex,
#   sbp_var = systolic_bp, bp_tx_var = on_bp_meds,
#   total_c_var = total_cholesterol, hdl_c_var = hdl_cholesterol,
#   statin_var = on_statin, dm_var = has_diabetes,
#   smoking_var = is_smoker, egfr_var = egfr_ckdepi, bmi_var = body_mass_index,
#   hba1c_var = hba1c, uacr_var = uacr, zip_var = zip,
#   time = "both", model = NULL   # NULL = auto-select
# )
#
# # Force a specific model
# result <- estimate_risk_fast(df, time = "10yr", model = "sdi", zip_var = zip)

