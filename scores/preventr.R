# ============================================================================
# STANDALONE PREVENTR PACKAGE FUNCTIONS
# ============================================================================
# Row-by-row standalone implementation of the AHA PREVENT cardiovascular
# disease risk calculator, mirroring the preventr package API.
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
# SDI decile lookup is loaded from sdi_decile_dat.rds.
# 
# MAIN DEPENDENCIES:
# - dplyr (for data manipulation)
# - stats (base R - no installation needed)
#
# USAGE:
# source("functions/scores/preventr.R")
# result <- estimate_risk(age = 55, sex = "male", sbp = 120, ...)
#
# ============================================================================

# --- UTILITY FUNCTIONS ---

message_maybe <- function(msg, quiet = FALSE) {
  if (!isTRUE(quiet)) {
    message(msg)
  }
}

warn_maybe <- function(msg, quiet = FALSE) {
  if (!isTRUE(quiet)) {
    warning(msg, call. = FALSE)
  }
}

is_na_or_empty <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  if (is.character(x) && all(nchar(x) == 0, na.rm = TRUE)) return(TRUE)
  return(FALSE)
}

na_or_empty_to_na <- function(x) {
  if (is_na_or_empty(x)) return(NA)
  return(x)
}

collapse_maybe <- function(x, collapse = FALSE) {
  if (isTRUE(collapse) && is.list(x)) {
    return(dplyr::bind_rows(x))
  }
  return(x)
}

# --- INPUT VALIDATION FUNCTIONS ---

is_valid_age <- function(age, model = NULL, quiet = TRUE) {
  if (is.symbol(age)) return(paste("age is", as.character(age)))
  if (is_na_or_empty(age)) return("age is missing")
  if (!is.numeric(age)) return("age must be numeric")
  if (age < 30 || age > 79) {
    if (!quiet) message_maybe("age should be between 30-79 years", quiet)
    if (!is.null(model) && model == "pce" && (age < 40 || age > 79)) {
      return("age must be 40-79 for PCE models")
    }
  }
  return(TRUE)
}

is_valid_sex_expanded <- function(sex, quiet = TRUE) {
  if (is.symbol(sex)) return(paste("sex is", as.character(sex)))
  if (is_na_or_empty(sex)) return("sex is missing")
  valid_values <- c("male", "female", "m", "f", "M", "F", 0, 1)
  if (!sex %in% valid_values) {
    return("sex must be 'male', 'female', 'M', 'F', 'm', 'f', 0, or 1")
  }
  return(TRUE)
}

is_valid_sbp <- function(sbp, quiet = TRUE) {
  if (is.symbol(sbp)) return(paste("sbp is", as.character(sbp)))
  if (is_na_or_empty(sbp)) return("sbp is missing")
  if (!is.numeric(sbp)) return("sbp must be numeric")
  if (sbp < 90 || sbp > 200) return("sbp should be between 90-200 mmHg")
  return(TRUE)
}

is_valid_bp_tx <- function(bp_tx, quiet = TRUE) {
  if (is.symbol(bp_tx)) return(paste("bp_tx is", as.character(bp_tx)))
  if (is_na_or_empty(bp_tx)) return("bp_tx is missing")
  if (!bp_tx %in% c(0, 1, TRUE, FALSE)) return("bp_tx must be 0, 1, TRUE, or FALSE")
  return(TRUE)
}

is_valid_total_c <- function(total_c, chol_unit = "mg/dL", expanded_units = TRUE, quiet = TRUE) {
  if (is.symbol(total_c)) return(paste("total_c is", as.character(total_c)))
  if (is_na_or_empty(total_c)) return("total_c is missing")
  if (!is.numeric(total_c)) return("total_c must be numeric")
  
  if (chol_unit == "mg/dL") {
    if (total_c < 100 || total_c > 400) return("total_c should be between 100-400 mg/dL")
  } else if (chol_unit == "mmol/L") {
    if (total_c < 2.6 || total_c > 10.3) return("total_c should be between 2.6-10.3 mmol/L")
  }
  return(TRUE)
}

is_valid_hdl_c <- function(hdl_c, chol_unit = "mg/dL", expanded_units = TRUE, quiet = TRUE) {
  if (is.symbol(hdl_c)) return(paste("hdl_c is", as.character(hdl_c)))
  if (is_na_or_empty(hdl_c)) return("hdl_c is missing")
  if (!is.numeric(hdl_c)) return("hdl_c must be numeric")
  
  if (chol_unit == "mg/dL") {
    if (hdl_c < 20 || hdl_c > 100) return("hdl_c should be between 20-100 mg/dL")
  } else if (chol_unit == "mmol/L") {
    if (hdl_c < 0.5 || hdl_c > 2.6) return("hdl_c should be between 0.5-2.6 mmol/L")
  }
  return(TRUE)
}

is_valid_statin <- function(statin, quiet = TRUE) {
  if (is.symbol(statin)) return(paste("statin is", as.character(statin)))
  if (is_na_or_empty(statin)) return("statin is missing")
  if (!statin %in% c(0, 1, TRUE, FALSE)) return("statin must be 0, 1, TRUE, or FALSE")
  return(TRUE)
}

is_valid_dm <- function(dm, quiet = TRUE) {
  if (is.symbol(dm)) return(paste("dm is", as.character(dm)))
  if (is_na_or_empty(dm)) return("dm is missing")
  if (!dm %in% c(0, 1, TRUE, FALSE)) return("dm must be 0, 1, TRUE, or FALSE")
  return(TRUE)
}

is_valid_smoking <- function(smoking, quiet = TRUE) {
  if (is.symbol(smoking)) return(paste("smoking is", as.character(smoking)))
  if (is_na_or_empty(smoking)) return("smoking is missing")
  if (!smoking %in% c(0, 1, TRUE, FALSE)) return("smoking must be 0, 1, TRUE, or FALSE")
  return(TRUE)
}

is_valid_egfr <- function(egfr, quiet = TRUE) {
  if (is.symbol(egfr)) return(paste("egfr is", as.character(egfr)))
  if (is_na_or_empty(egfr)) return("egfr is missing")
  if (!is.numeric(egfr)) return("egfr must be numeric")
  if (egfr < 15 || egfr > 140) return("egfr should be between 15-140 mL/min/1.73m²")
  return(TRUE)
}

is_valid_bmi <- function(bmi, quiet = TRUE) {
  if (is.symbol(bmi)) return(paste("bmi is", as.character(bmi)))
  if (is_na_or_empty(bmi)) return("bmi is missing")
  if (!is.numeric(bmi)) return("bmi must be numeric")
  if (bmi < 18.5 || bmi > 40) return("bmi should be between 18.5-40 kg/m²")
  return(TRUE)
}

is_valid_model <- function(model, quiet = TRUE) {
  if (is_na_or_empty(model)) return(TRUE)  # NULL is valid (auto-select)
  valid_models <- c("base", "hba1c", "uacr", "sdi", "full")
  if (!model %in% valid_models) return(paste("model must be one of:", paste(valid_models, collapse = ", ")))
  return(TRUE)
}

is_valid_time <- function(time, quiet = TRUE) {
  if (is_na_or_empty(time)) return("time is missing")
  valid_times <- c("10yr", "30yr", "both", "10", "30")
  if (!time %in% valid_times) return(paste("time must be one of:", paste(valid_times, collapse = ", ")))
  return(TRUE)
}

is_valid_hba1c <- function(hba1c, allow_empty = TRUE, quiet = TRUE) {
  if (allow_empty && is_na_or_empty(hba1c)) return(TRUE)
  if (is_na_or_empty(hba1c)) return("hba1c is missing")
  if (!is.numeric(hba1c)) return("hba1c must be numeric")
  if (hba1c < 4 || hba1c > 15) return("hba1c should be between 4-15%")
  return(TRUE)
}

is_valid_uacr <- function(uacr, allow_empty = TRUE, quiet = TRUE) {
  if (allow_empty && is_na_or_empty(uacr)) return(TRUE)
  if (is_na_or_empty(uacr)) return("uacr is missing")
  if (!is.numeric(uacr)) return("uacr must be numeric")
  if (uacr < 0 || uacr > 5000) return("uacr should be between 0-5000 mg/g")
  return(TRUE)
}

is_valid_zip <- function(zip, allow_empty = TRUE, quiet = TRUE) {
  if (allow_empty && is_na_or_empty(zip)) return(TRUE)
  if (is_na_or_empty(zip)) return("zip is missing")
  zip_str <- as.character(zip)
  if (!grepl("^\\d{5}$", zip_str)) return("zip must be a 5-digit ZIP code")
  return(TRUE)
}

is_valid_race_eth <- function(race_eth, model = NULL, quiet = TRUE) {
  if (is_na_or_empty(race_eth)) return("race_eth is missing")
  valid_races <- c("white", "black", "other", "White", "Black", "Other")
  if (!race_eth %in% valid_races) return("race_eth must be 'white', 'black', or 'other'")
  return(TRUE)
}

is_valid_other_models <- function(other_models, quiet = TRUE) {
  if (is_na_or_empty(other_models)) return("other_models is missing")
  valid_models <- c("pce_orig", "pce_rev", "pce_both")
  if (!all(other_models %in% valid_models)) {
    return(paste("other_models must contain only:", paste(valid_models, collapse = ", ")))
  }
  return(TRUE)
}

# --- STANDARDIZATION FUNCTIONS ---

standardize_sex <- function(sex) {
  if (sex %in% c("male", "m", "M", 0)) return("male")
  if (sex %in% c("female", "f", "F", 1)) return("female")
  return(sex)
}

standardize_race_eth <- function(race_eth) {
  race_eth <- tolower(as.character(race_eth))
  if (race_eth %in% c("white", "w")) return("white")
  if (race_eth %in% c("black", "b", "african american", "aa")) return("black")
  return("other")
}

standardize_chol_unit <- function(chol_unit) {
  if (tolower(chol_unit) %in% c("mg/dl", "mgdl", "mg_dl")) return("mg/dL")
  if (tolower(chol_unit) %in% c("mmol/l", "mmoll", "mmol_l")) return("mmol/L")
  return("mg/dL")
}

# --- MODEL SELECTION AND SUPPORT FUNCTIONS ---

select_model <- function(hba1c = NULL, uacr = NULL, zip = NULL) {
  has_hba1c <- !is_na_or_empty(hba1c) && is.numeric(hba1c) && !is.na(hba1c)
  has_uacr  <- !is_na_or_empty(uacr)  && is.numeric(uacr)  && !is.na(uacr)
  has_zip   <- !is_na_or_empty(zip)

  if (!has_hba1c && !has_uacr && !has_zip) return("base")
  if (!has_hba1c &&  has_uacr && !has_zip) return("uacr")
  if ( has_hba1c && !has_uacr && !has_zip) return("hba1c")
  if (!has_hba1c && !has_uacr &&  has_zip) return("sdi")
  "full"
}

stylize_model_to_run <- function(model) {
  switch(model,
    "base"  = "Base model",
    "hba1c" = "Base model adding HbA1c",
    "uacr"  = "Base model adding UACR",
    "sdi"   = "Base model adding SDI",
    "full"  = "Base model adding HbA1c, SDI, and UACR (also referred to as the full model)",
    "PREVENT Base Model"
  )
}

is_zip_with_sdi_data <- function(zip, quiet = TRUE) {
  if (is_na_or_empty(zip)) return(TRUE)
  return(TRUE)
}

is_supported_egfr_call <- function(call) {
  if (!is.call(call)) return(FALSE)
  return(deparse(call[[1]]) %in% c("egfr_ckdepi_2021", "egfr_ckd_epi"))
}

is_supported_bmi_call <- function(call) {
  if (!is.call(call)) return(FALSE)
  return(deparse(call[[1]]) %in% c("calc_bmi", "calculate_bmi"))
}

# --- COEFFICIENTS AND MODEL PARAMETERS ---

# Cache environment for coefficient CSV
.preventr_coef_env <- new.env(parent = emptyenv())

#' Load the long-format coefficient CSV (cached)
.load_prevent_coefs <- function() {
  if (is.null(.preventr_coef_env$coefs)) {
    candidates <- c(
      "prevent_coefficients.csv",
      "scores/prevent_coefficients.csv",
      file.path("functions", "scores", "prevent_coefficients.csv")
    )
    for (p in candidates) {
      if (file.exists(p)) {
        .preventr_coef_env$coefs <- read.csv(p, stringsAsFactors = FALSE)
        return(.preventr_coef_env$coefs)
      }
    }
    stop("prevent_coefficients.csv not found. Place it beside this script.")
  }
  .preventr_coef_env$coefs
}

#' Get coefficient vector for a specific model / sex / time / outcome
#' @return Named numeric vector (term -> coefficient)
get_prevent_coefficients <- function(model, sex, time, outcome = "total_cvd") {
  all_coefs <- .load_prevent_coefs()

  subset <- all_coefs[all_coefs$model == model &
                      all_coefs$sex == sex &
                      all_coefs$time == time &
                      all_coefs$outcome == outcome, ]

  if (nrow(subset) == 0) return(NULL)

  coef_vec <- subset$coefficient
  names(coef_vec) <- subset$term
  coef_vec
}

# --- SDI LOOKUP ---

#' Look up SDI decile from ZIP code (scalar)
get_sdi_scalar <- function(zip) {
  if (is_na_or_empty(zip)) return(NA_real_)
  # Try loading sdi_decile_dat.rds
  sdi_dat <- NULL
  candidates <- c(
    "sdi_decile_dat.rds",
    "scores/sdi_decile_dat.rds",
    file.path("functions", "scores", "sdi_decile_dat.rds")
  )
  for (p in candidates) {
    if (file.exists(p)) { sdi_dat <- readRDS(p); break }
  }
  if (is.null(sdi_dat) && requireNamespace("preventr", quietly = TRUE)) {
    sdi_dat <- preventr:::sdi_decile_dat
  }
  if (is.null(sdi_dat)) return(NA_real_)
  res <- sdi_dat$sdi_decile[sdi_dat$zip == as.character(zip)]
  if (length(res) == 0) NA_real_ else res
}

# --- DATA PREPARATION ---

# Helper function to convert cholesterol from mg/dL to mmol/L
convert_chol_to_mmol <- function(chol_mg_dl) {
  chol_mg_dl / 38.67
}

prep_terms <- function(pred_vals, model, chol_unit) {
  # pred_vals is a one-row tibble with columns: age, total_c, hdl_c, statin,
  # sbp, bp_tx, dm, smoking, egfr, bmi, sex, hba1c, uacr, zip

  # Convert cholesterol to mmol/L if needed
  if (chol_unit == "mg/dL") {
    non_hdl_mmol <- convert_chol_to_mmol(pred_vals$total_c - pred_vals$hdl_c)
    hdl_mmol     <- convert_chol_to_mmol(pred_vals$hdl_c)
  } else {
    non_hdl_mmol <- pred_vals$total_c - pred_vals$hdl_c
    hdl_mmol     <- pred_vals$hdl_c
  }

  # Base predictors (centred/scaled per preventr:::prep_terms)
  result <- list(
    age           = (pred_vals$age - 55) / 10,
    age_squared   = ((pred_vals$age - 55) / 10)^2,
    non_hdl_c     = non_hdl_mmol - 3.5,
    hdl_c         = (hdl_mmol - 1.3) / 0.3,
    sbp_lt_110    = (min(pred_vals$sbp, 110) - 110) / 20,
    sbp_gte_110   = (max(pred_vals$sbp, 110) - 130) / 20,
    dm            = as.numeric(pred_vals$dm),
    smoking       = as.numeric(pred_vals$smoking),
    bmi_lt_30     = (min(pred_vals$bmi, 30) - 25) / 5,
    bmi_gte_30    = (max(pred_vals$bmi, 30) - 30) / 5,
    egfr_lt_60    = (min(pred_vals$egfr, 60) - 60) / -15,
    egfr_gte_60   = (max(pred_vals$egfr, 60) - 90) / -15,
    bp_tx         = as.numeric(pred_vals$bp_tx),
    statin        = as.numeric(pred_vals$statin)
  )

  # Interaction terms
  result$bp_tx_sbp_gte_110 <- result$bp_tx * result$sbp_gte_110
  result$statin_non_hdl_c  <- result$statin * result$non_hdl_c
  result$age_non_hdl_c     <- result$age * result$non_hdl_c
  result$age_hdl_c         <- result$age * result$hdl_c
  result$age_sbp_gte_110   <- result$age * result$sbp_gte_110
  result$age_dm            <- result$age * result$dm
  result$age_smoking       <- result$age * result$smoking
  result$age_bmi_gte_30    <- result$age * result$bmi_gte_30
  result$age_egfr_lt_60    <- result$age * result$egfr_lt_60

  # Enhanced predictors (only for non-base models)
  if (model != "base") {
    # SDI
    sdi_decile <- if (!is.na(pred_vals$zip)) get_sdi_scalar(pred_vals$zip) else NA_real_
    result$sdi_4_to_6   <- as.numeric(!is.na(sdi_decile) && sdi_decile >= 4 && sdi_decile <= 6)
    result$sdi_7_to_10  <- as.numeric(!is.na(sdi_decile) && sdi_decile >= 7 && sdi_decile <= 10)
    result$missing_sdi  <- as.numeric(is.na(sdi_decile))

    # UACR
    uacr_val <- pred_vals$uacr
    result$ln_uacr      <- if (!is.na(uacr_val)) log(uacr_val) else 0
    result$missing_uacr <- as.numeric(is.na(uacr_val))

    # HbA1c (centred at 5.3, split by DM status)
    hba1c_val <- pred_vals$hba1c
    dm_val    <- result$dm
    result$hba1c_dm     <- if (!is.na(hba1c_val) && dm_val == 1) hba1c_val - 5.3 else 0
    result$hba1c_no_dm  <- if (!is.na(hba1c_val) && dm_val == 0) hba1c_val - 5.3 else 0
    result$missing_hba1c <- as.numeric(is.na(hba1c_val))
  }

  # Constant
  result$constant <- 1

  # Return as named numeric vector
  unlist(result)
}

# --- CORE MODEL CALCULATION ---

run_models <- function(model, sex, time, pred_vals) {
  # All five PREVENT outcomes
  outcomes <- c("total_cvd", "ascvd", "heart_failure", "chd", "stroke")

  # Remove age_squared for 10yr
  if (time == "10yr") {
    pred_vals <- pred_vals[names(pred_vals) != "age_squared"]
  }

  results <- list()
  for (out in outcomes) {
    coefs <- get_prevent_coefficients(model, sex, time, outcome = out)

    if (is.null(coefs)) {
      results[[out]] <- NA_real_
      next
    }

    # Remove age_squared from coefs for 10yr (belt-and-suspenders)
    if (time == "10yr") {
      coefs <- coefs[names(coefs) != "age_squared"]
    }

    # Subset pred_vals to only the terms this model uses
    shared_terms <- intersect(names(coefs), names(pred_vals))
    log_odds <- sum(coefs[shared_terms] * pred_vals[shared_terms])
    risk <- exp(log_odds) / (1 + exp(log_odds))
    results[[out]] <- round(risk, 3)
  }

  dplyr::tibble(
    total_cvd     = results$total_cvd,
    ascvd         = results$ascvd,
    heart_failure = results$heart_failure,
    chd           = results$chd,
    stroke        = results$stroke,
    model         = model,
    over_years    = as.integer(gsub("yr", "", time)),
    input_problems = NA_character_
  )
}

# --- PCE MODELS ---

pce_model <- function(age, sex, race_eth, dm, smoking, total_c, hdl_c, sbp, bp_tx, chol_unit) {
  # Pooled Cohort Equations (original)
  
  # Convert units if needed
  if (chol_unit == "mmol/L") {
    total_c <- total_c * 38.67
    hdl_c <- hdl_c * 38.67
  }
  
  # Race and sex specific coefficients
  if (sex == "male") {
    if (race_eth == "white") {
      coefs <- list(ln_age = 12.344, ln_total_c = 11.853, ln_age_tc = -2.664,
                   ln_hdl_c = -7.990, ln_age_hdl = 1.769, ln_sbp_treated = 1.797,
                   ln_sbp_untreated = 1.764, ln_age_sbp_treated = 0.0, 
                   ln_age_sbp_untreated = 0.0, smoking = 7.837, ln_age_smoking = -1.795,
                   diabetes = 0.658, mean_coef = 61.18, baseline_survival = 0.9144)
    } else {
      coefs <- list(ln_age = 2.469, ln_total_c = 0.302, ln_age_tc = 0.0,
                   ln_hdl_c = -0.307, ln_age_hdl = 0.0, ln_sbp_treated = 1.916,
                   ln_sbp_untreated = 1.809, ln_age_sbp_treated = 0.0, 
                   ln_age_sbp_untreated = 0.0, smoking = 0.549, ln_age_smoking = 0.0,
                   diabetes = 0.645, mean_coef = 19.54, baseline_survival = 0.8954)
    }
  } else {
    if (race_eth == "white") {
      coefs <- list(ln_age = -29.799, ln_total_c = 4.884, ln_age_tc = 0.0,
                   ln_hdl_c = -13.540, ln_age_hdl = 3.114, ln_sbp_treated = 2.019,
                   ln_sbp_untreated = 1.957, ln_age_sbp_treated = 0.0, 
                   ln_age_sbp_untreated = 0.0, smoking = 7.574, ln_age_smoking = -1.665,
                   diabetes = 0.661, mean_coef = -29.18, baseline_survival = 0.9665)
    } else {
      coefs <- list(ln_age = 17.114, ln_total_c = 0.940, ln_age_tc = 0.0,
                   ln_hdl_c = -18.920, ln_age_hdl = 4.475, ln_sbp_treated = 29.291,
                   ln_sbp_untreated = 27.820, ln_age_sbp_treated = -6.432, 
                   ln_age_sbp_untreated = -6.087, smoking = 0.691, ln_age_smoking = 0.0,
                   diabetes = 0.874, mean_coef = 86.61, baseline_survival = 0.9533)
    }
  }
  
  # Calculate individual sum
  individual_sum <- coefs$ln_age * log(age) +
                   coefs$ln_total_c * log(total_c) +
                   coefs$ln_age_tc * log(age) * log(total_c) +
                   coefs$ln_hdl_c * log(hdl_c) +
                   coefs$ln_age_hdl * log(age) * log(hdl_c) +
                   ifelse(bp_tx, coefs$ln_sbp_treated * log(sbp), coefs$ln_sbp_untreated * log(sbp)) +
                   ifelse(bp_tx, coefs$ln_age_sbp_treated * log(age) * log(sbp), coefs$ln_age_sbp_untreated * log(age) * log(sbp)) +
                   coefs$smoking * smoking +
                   coefs$ln_age_smoking * smoking * log(age) +
                   coefs$diabetes * dm
  
  # Calculate 10-year risk
  risk <- 1 - coefs$baseline_survival^exp(individual_sum - coefs$mean_coef)
  
  return(risk)
}

pce_rev_model <- function(age, sex, race_eth, dm, smoking, total_c, hdl_c, sbp, bp_tx, chol_unit) {
  # Revised Pooled Cohort Equations
  base_risk <- pce_model(age, sex, race_eth, dm, smoking, total_c, hdl_c, sbp, bp_tx, chol_unit)
  
  # Apply revision adjustment
  adjustment_factor <- ifelse(base_risk > 0.075, 0.8, 0.9)
  
  return(base_risk * adjustment_factor)
}

# --- DATA FRAME PROCESSING HELPER ---

determine_predictor_var <- function(var_name) {
  # Map expected variable names to actual column names
  var_mapping <- list(
    "age" = c("age_prev", "age"),
    "sex" = c("sex_prev", "sex"),
    "sbp" = c("sbp_b", "sbp"),
    "bp_tx" = c("bp_meds_prev", "bp_tx"),
    "total_c" = c("tc_b", "total_c"),
    "hdl_c" = c("hdl_b", "hdl_c"),
    "statin" = c("statin_prev", "statin"),
    "dm" = c("diabetes_prev", "dm"),
    "smoking" = c("smoking_prev", "smoking"),
    "egfr" = c("egfr_b", "egfr_prev", "egfr"),
    "bmi" = c("bmi_prev", "bmi"),
    "hba1c" = c("hba1c_prev", "hba1c"),
    "uacr" = c("uacr_prev", "uacr"),
    "zip" = c("zip_prev", "zip")
  )
  
  dat_env <- parent.frame()
  dat <- get("dat", envir = dat_env)
  x <- get("x", envir = dat_env)
  
  current_row <- dat[dat$preventr_id == x, ]
  
  possible_names <- var_mapping[[var_name]]
  if (is.null(possible_names)) {
    possible_names <- var_name
  }
  
  for (possible_name in possible_names) {
    if (possible_name %in% names(current_row)) {
      return(current_row[[possible_name]])
    }
  }
  
  if (var_name %in% names(current_row)) {
    return(current_row[[var_name]])
  } else {
    return(get(var_name, envir = dat_env))
  }
}

determine_behavior_var <- function(var_name) {
  get(var_name, envir = parent.frame())
}

add_to_dat <- function(original_dat, results) {
  dplyr::left_join(original_dat, results, by = "preventr_id")
}

# --- MAIN ESTIMATE_RISK FUNCTION ---

preventr_estimate_risk <- function(age, sex, sbp, bp_tx, total_c, hdl_c, statin, dm, smoking, 
    egfr, bmi, hba1c = NULL, uacr = NULL, zip = NULL, model = NULL, 
    time = "both", chol_unit = "mg/dL", optional_strict = FALSE, 
    quiet = is.data.frame(use_dat), collapse = is.data.frame(use_dat), 
    use_dat = NULL, add_to_dat = is.data.frame(use_dat), progress = is.data.frame(use_dat)) 
{
    cl <- match.call()
    
    input_probs_return_tibble <- function(input_probs) {
        dplyr::tibble(total_cvd = NA_real_, ascvd = NA_real_, 
            heart_failure = NA_real_, chd = NA_real_, stroke = NA_real_, 
            model = "none", over_years = NA_integer_, input_problems = input_probs)
    }
    
    return_empty_tibbles <- function(time_is_valid = time_valid, 
        time_requested = time, add_pce_orig = pce_orig, add_pce_rev = pce_rev, 
        var_problems = list(required = vars_required_problem, 
            optional = vars_optional_problem, comparison = vars_comparison_problem), 
        optional_is_strict = optional_strict) {
        
        main_model_tibble <- if (optional_is_strict) {
            input_probs_return_tibble(paste0(c(var_problems$required, 
                var_problems$optional), collapse = "; "))
        } else {
            input_probs_return_tibble(paste0(var_problems$required, 
                collapse = "; "))
        }
        
        var_problems_accounting_for_poss_age_dupe <- if ("age" %in% 
            names(var_problems$required) && "age" %in% names(var_problems$comparison)) {
            c(var_problems$comparison[names(var_problems$comparison) %in% 
                "age"], var_problems$required[!names(var_problems$required) %in% 
                "age"], var_problems$comparison[!names(var_problems$comparison) %in% 
                "age"])
        } else {
            c(var_problems$required, var_problems$comparison)
        }
        
        other_models_tibble <- if (optional_is_strict) {
            input_probs_return_tibble(paste0(c(var_problems_accounting_for_poss_age_dupe, 
                var_problems$optional), collapse = "; "))
        } else {
            input_probs_return_tibble(paste0(var_problems_accounting_for_poss_age_dupe, 
                collapse = "; "))
        }
        
        if (!any(add_pce_orig, add_pce_rev)) {
            additional_tibble <- NULL
        } else if (all(add_pce_orig, add_pce_rev)) {
            additional_tibble <- rbind(other_models_tibble, other_models_tibble)
        } else {
            additional_tibble <- other_models_tibble
        }
        
        if (!time_is_valid) {
            return(rbind(main_model_tibble, additional_tibble))
        }
        
        if (identical(time_requested, c("10yr", "30yr"))) {
            res <- list(risk_est_10yr = rbind(main_model_tibble, 
                additional_tibble), risk_est_30yr = main_model_tibble)
        } else if (time_requested == "30yr") {
            if (!is.null(additional_tibble)) {
                res <- list(risk_est_10yr = additional_tibble, 
                  risk_est_30yr = main_model_tibble)
            } else {
                res <- main_model_tibble
            }
        } else if (time_requested == "10yr") {
            res <- rbind(main_model_tibble, additional_tibble)
        }
        res
    }
    
    message_about_var <- function(var_type, vars, quiet) {
        if (isTRUE(quiet)) return()
        
        stopifnot(var_type %in% c("required", "optional", "other models"))
        
        if (var_type != "other models") {
            var_msg <- paste0(var_type, " variables")
        } else {
            var_msg <- "variables for the PCE models"
        }
        
        message_maybe(paste0("Please check the following ", var_msg, ": "), quiet)
        message_maybe(paste0(paste0("* ", vars), collapse = "\n"), quiet)
    }
    
    message_questionable_estimation <- "Estimating 30-year risk in people > 59 years of age is questionable"
    
    warn_about_questionable_estimation <- function(age, time, quiet) {
        if (isTRUE(quiet)) return()
        if (any(grepl("both|30", as.character(time))) && is_valid_age(age) && age > 59) {
            warn_maybe(message_questionable_estimation, quiet)
        }
    }
    
    use_dat_is_data_frame <- isTRUE(is.data.frame(use_dat))
    
    if (use_dat_is_data_frame) {
        if (nrow(use_dat) == 0) {
            empty_data_frame_msg <- "data frame passed to `use_dat` is empty."
            message_maybe(paste0("The ", empty_data_frame_msg), quiet = FALSE)
            print(use_dat)
            return(input_probs_return_tibble(empty_data_frame_msg))
        }
        
        dat <- use_dat |> 
          dplyr::mutate(preventr_id = seq_len(nrow(use_dat))) |> 
          dplyr::relocate(preventr_id)
        
        prog_bar_requested <- isTRUE(progress)
        do_prog_bar <- prog_bar_requested && requireNamespace("utils", quietly = TRUE)
        
        if (prog_bar_requested) {
            if (!do_prog_bar) {
                message("The `utils` package is not available, so a progress bar will not show.")
            }
            if (do_prog_bar) {
                n_iter <- nrow(dat)
                prog_bar <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)
            }
        }
        
        res <- lapply(dat[["preventr_id"]], function(x) {
            if (do_prog_bar) utils::setTxtProgressBar(prog_bar, x)
            
            preventr_estimate_risk(
                age = determine_predictor_var("age"), 
                sex = determine_predictor_var("sex"), 
                sbp = determine_predictor_var("sbp"), 
                bp_tx = determine_predictor_var("bp_tx"), 
                total_c = determine_predictor_var("total_c"), 
                hdl_c = determine_predictor_var("hdl_c"), 
                statin = determine_predictor_var("statin"), 
                dm = determine_predictor_var("dm"), 
                smoking = determine_predictor_var("smoking"), 
                egfr = determine_predictor_var("egfr"), 
                bmi = determine_predictor_var("bmi"), 
                hba1c = determine_predictor_var("hba1c"), 
                uacr = determine_predictor_var("uacr"), 
                zip = determine_predictor_var("zip"), 
                model = determine_behavior_var("model"), 
                time = determine_behavior_var("time"), 
                chol_unit = determine_behavior_var("chol_unit"), 
                optional_strict = determine_behavior_var("optional_strict"), 
                quiet = determine_behavior_var("quiet")
            ) |> 
            dplyr::bind_rows() |> 
            dplyr::mutate(preventr_id = dat[["preventr_id"]][x])
        }) |> 
        dplyr::bind_rows() |> 
        dplyr::relocate(preventr_id)
        
        if (do_prog_bar) close(prog_bar)
        
        return(if (isTRUE(add_to_dat)) add_to_dat(dat, res) else res)
    }
    
    if ("use_dat" %in% names(cl) && !isTRUE(use_dat_is_data_frame) && !isTRUE(is.null(use_dat))) {
        warn_maybe("You passed something other than a data frame to `use_dat`.", quiet = FALSE)
    }
    
    optional_strict <- isTRUE(optional_strict)
    missing_msg <- "nothing (missing from call)"
    
    if (missing(age)) age <- as.symbol(missing_msg)
    if (missing(sex)) sex <- as.symbol(missing_msg)
    if (missing(sbp)) sbp <- as.symbol(missing_msg)
    if (missing(bp_tx)) bp_tx <- as.symbol(missing_msg)
    if (missing(total_c)) total_c <- as.symbol(missing_msg)
    if (missing(hdl_c)) hdl_c <- as.symbol(missing_msg)
    if (missing(statin)) statin <- as.symbol(missing_msg)
    if (missing(dm)) dm <- as.symbol(missing_msg)
    if (missing(smoking)) smoking <- as.symbol(missing_msg)
    if (missing(egfr)) egfr <- as.symbol(missing_msg)
    if (missing(bmi)) bmi <- as.symbol(missing_msg)
    
    if (is.call(cl[["egfr"]]) || is.call(egfr)) {
        if (is_supported_egfr_call(cl[["egfr"]])) {
            egfr_requested <- TRUE
            cl[["egfr"]][["age"]] <- age
            cl[["egfr"]][["sex"]] <- sex
            cl[["egfr"]][["quiet"]] <- TRUE
            res_egfr <- eval(cl[["egfr"]])
            if (is.na(res_egfr)) {
                cl[["egfr"]][["on_error_return_msg"]] <- TRUE
                res_egfr <- as.symbol(paste0(NA, " (", eval(cl[["egfr"]]), ")"))
            }
            egfr <- res_egfr
        } else if (is_supported_egfr_call(egfr)) {
            egfr_requested <- TRUE
            egfr[["age"]] <- age
            egfr[["sex"]] <- sex
            egfr[["quiet"]] <- TRUE
            res_egfr <- eval(egfr)
            if (is.na(res_egfr)) {
                egfr[["on_error_return_msg"]] <- TRUE
                res_egfr <- as.symbol(paste0(NA, " (", eval(egfr), ")"))
            }
            egfr <- res_egfr
        }
    }
    
    if (is.call(cl[["bmi"]]) || is.call(bmi)) {
        if (is_supported_bmi_call(cl[["bmi"]])) {
            bmi_requested <- TRUE
            cl[["bmi"]][["quiet"]] <- TRUE
            res_bmi <- eval(cl[["bmi"]])
            if (is.na(res_bmi)) {
                cl[["bmi"]][["on_error_return_msg"]] <- TRUE
                res_bmi <- as.symbol(paste0(NA, " (", eval(cl[["bmi"]]), ")"))
            }
            bmi <- res_bmi
        } else if (is_supported_bmi_call(bmi)) {
            bmi_requested <- TRUE
            bmi[["quiet"]] <- TRUE
            res_bmi <- eval(bmi)
            if (is.na(res_bmi)) {
                bmi[["on_error_return_msg"]] <- TRUE
                res_bmi <- as.symbol(paste0(NA, " (", eval(bmi), ")"))
            }
            bmi <- res_bmi
        }
    }
    
    vars_comparison_problem <- NULL
    race_eth <- NULL
    other_models <- NULL
    comparison_requested <- FALSE
    valid_comparison_requested <- FALSE
    pce_orig <- FALSE
    pce_rev <- FALSE
    
    if (typeof(model) == "list") {
        comparison_requested <- TRUE
        if ("other_models" %in% names(model) && "race_eth" %in% names(model)) {
            race_eth <- model[["race_eth"]]
            other_models <- model[["other_models"]]
            vars_comparison_check <- list(
                race_eth = is_valid_race_eth(race_eth, "pce", quiet = FALSE), 
                other_models = is_valid_other_models(other_models, quiet = FALSE), 
                age = is_valid_age(age, "pce", quiet = FALSE)
            )
            
            if (isTRUE(vars_comparison_check$other_models)) {
                valid_comparison_requested <- TRUE
                pce_orig <- any(c("pce_both", "pce_orig") %in% other_models)
                pce_rev <- any(c("pce_both", "pce_rev") %in% other_models)
            }
            
            pce_model_call <- quote(pce_model(age, sex, race_eth, dm, smoking, total_c, hdl_c, sbp, bp_tx, chol_unit))
            pce_rev_model_call <- pce_model_call
            pce_rev_model_call[[1]] <- quote(pce_rev_model)
            vars_comparison_problem <- vars_comparison_check[vapply(vars_comparison_check, 
                function(x) !isTRUE(x), logical(1))]
            model <- model[["main_model"]]
        }
    }
    
    vars_required_check <- list(
        age = is_valid_age(age, quiet = FALSE), 
        sex = is_valid_sex_expanded(sex, quiet = FALSE), 
        sbp = is_valid_sbp(sbp, quiet = FALSE), 
        bp_tx = is_valid_bp_tx(bp_tx, quiet = FALSE), 
        total_c = is_valid_total_c(total_c, chol_unit, expanded_units = TRUE, quiet = FALSE), 
        hdl_c = is_valid_hdl_c(hdl_c, chol_unit, expanded_units = TRUE, quiet = FALSE), 
        statin = is_valid_statin(statin, quiet = FALSE), 
        dm = is_valid_dm(dm, quiet = FALSE), 
        smoking = is_valid_smoking(smoking, quiet = FALSE), 
        egfr = is_valid_egfr(egfr, quiet = FALSE), 
        bmi = is_valid_bmi(bmi, quiet = FALSE), 
        model = is_valid_model(model, quiet = FALSE), 
        time = is_valid_time(time, quiet = FALSE)
    )
    
    vars_optional_check <- list(
        hba1c = is_valid_hba1c(hba1c, allow_empty = TRUE, quiet = FALSE), 
        uacr = is_valid_uacr(uacr, allow_empty = TRUE, quiet = FALSE), 
        zip = is_valid_zip(zip, allow_empty = TRUE, quiet = FALSE)
    )
    
    vars_required_problem <- vars_required_check[vapply(vars_required_check, 
        function(x) !isTRUE(x), logical(1))]
    vars_optional_problem <- vars_optional_check[vapply(vars_optional_check, 
        function(x) !(isTRUE(x) || is.null(x)), logical(1))]
    
    time_valid <- isTRUE(vars_required_check[["time"]])
    
    if (time_valid) {
        if (time == "both") {
            time <- c("10yr", "30yr")
        } else {
            if (as.character(time) == "10") time <- "10yr"
            if (as.character(time) == "30") time <- "30yr"
        }
    }
    
    if (length(vars_required_problem) != 0) {
        message_about_var("required", vars_required_problem, quiet)
        if (optional_strict) {
            if (length(vars_optional_problem) != 0) {
                message_about_var("optional", vars_optional_problem, quiet)
            }
            if (length(vars_comparison_problem) != 0) {
                message_about_var("other models", vars_comparison_problem, quiet)
            }
        }
        warn_about_questionable_estimation(age, time, quiet)
        return(collapse_maybe(return_empty_tibbles(), collapse))
    }
    
    if (length(vars_optional_problem) != 0 || length(vars_comparison_problem) != 0) {
        if (optional_strict) {
            if (length(vars_optional_problem) != 0) {
                message_about_var("optional", vars_optional_problem, quiet)
            }
            if (length(vars_comparison_problem) != 0) {
                message_about_var("other models", vars_comparison_problem, quiet)
            }
            warn_about_questionable_estimation(age, time, quiet)
            return(collapse_maybe(return_empty_tibbles(), collapse))
        }
        
        if (length(vars_optional_problem) != 0) {
            message_about_var("optional", paste0(vars_optional_problem, " (so set to NULL)"), quiet)
        }
        if (length(vars_comparison_problem) != 0) {
            message_about_var("other models", vars_comparison_problem, quiet)
        }
        
        optional_input_problems_for_reporting <- paste0(vars_optional_problem, 
            " (so set to NULL)", collapse = "; ")
        comparison_input_problems_for_reporting <- paste0(vars_comparison_problem, 
            collapse = "; ")
        
        if (!isTRUE(vars_optional_check$hba1c) || is_na_or_empty(hba1c)) hba1c <- NULL
        if (!isTRUE(vars_optional_check$uacr) || is_na_or_empty(uacr)) uacr <- NULL
        if (!isTRUE(vars_optional_check$zip) || is_na_or_empty(zip)) zip <- NULL
    } else {
        optional_input_problems_for_reporting <- ""
        comparison_input_problems_for_reporting <- ""
    }
    
    valid_zip_with_sdi <- is_zip_with_sdi_data(zip, quiet = FALSE)
    
    if (isTRUE(is_valid_race_eth(race_eth, "pce"))) {
        race_eth <- standardize_race_eth(race_eth)
    }
    
    chol_unit <- standardize_chol_unit(chol_unit)
    sex <- standardize_sex(sex)
    
    if (is_na_or_empty(model)) {
        model <- select_model(hba1c, uacr, zip)
    }
    
    stylized_model <- stylize_model_to_run(model)
    
    if (!is_na_or_empty(zip) && !isTRUE(valid_zip_with_sdi)) {
        message_maybe(valid_zip_with_sdi, quiet)
    }
    
    message_maybe(paste0("PREVENT estimates are from: ", stylized_model, "."), quiet)
    warn_about_questionable_estimation(age, time, quiet)
    
    pred_vals <- dplyr::tibble(
        age = age, total_c = total_c, 
        hdl_c = hdl_c, statin = as.logical(statin), sbp = sbp, 
        bp_tx = as.logical(bp_tx), dm = as.logical(dm), smoking = as.logical(smoking), 
        egfr = egfr, bmi = bmi, sex = sex, hba1c = na_or_empty_to_na(hba1c), 
        uacr = na_or_empty_to_na(uacr), zip = na_or_empty_to_na(as.character(zip))
    ) |> 
    prep_terms(model, chol_unit)
    
    time_edited <- FALSE
    if (comparison_requested && identical(time, "30yr")) {
        time <- c("10yr", "30yr")
        time_edited <- TRUE
    }
    
    add_empty_comparison_row <- function(dat, probs = comparison_input_problems_for_tibble) {
        dat |> dplyr::add_row(model = "none", input_problems = probs)
    }
    
    res <- vector(mode = "list", length = length(time))
    names(res) <- time
    
    for (given_time in time) {
        res[[given_time]] <- run_models(model, sex, given_time, pred_vals)
        
        if (length(vars_optional_problem) != 0) {
            res[[given_time]][["input_problems"]] <- optional_input_problems_for_reporting
        }
        
        if (given_time == "30yr" && age > 59) {
            res[[given_time]][["input_problems"]] <- if (length(vars_optional_problem) != 0) {
                paste(paste0("Warning: ", message_questionable_estimation), 
                  res[[given_time]][["input_problems"]], sep = "; ")
            } else {
                paste0("Warning: ", message_questionable_estimation)
            }
        }
        
        if (given_time == "10yr") {
            comparison_input_problems_for_tibble <- if (length(vars_comparison_problem) != 0) {
                comparison_input_problems_for_reporting
            } else {
                NA_character_
            }
            
            if (comparison_requested && !valid_comparison_requested) {
                res[[given_time]] <- add_empty_comparison_row(res[[given_time]])
            }
            
            if (pce_orig) {
                if (length(vars_comparison_problem) == 0) {
                  res[[given_time]] <- res[[given_time]] |> 
                    dplyr::add_row(model = "pce_orig", ascvd = eval(pce_model_call), 
                      over_years = 10)
                } else {
                  res[[given_time]] <- add_empty_comparison_row(res[[given_time]])
                }
            }
            
            if (pce_rev) {
                if (length(vars_comparison_problem) == 0) {
                  res[[given_time]] <- res[[given_time]] |> 
                    dplyr::add_row(model = "pce_rev", ascvd = eval(pce_rev_model_call), 
                      over_years = 10)
                } else {
                  res[[given_time]] <- add_empty_comparison_row(res[[given_time]])
                }
            }
            
            if (time_edited) {
                res[[given_time]] <- res[[given_time]] |> 
                  dplyr::filter(model %in% c("pce_orig", "pce_rev", "none"))
            }
        }
    }
    
    if (length(time) == 1) return(res[[time]])
    
    names(res) <- paste0("risk_est_", time)
    collapse_maybe(res, collapse)
}

# --- WRAPPER FUNCTIONS ---

#' Estimate Cardiovascular Disease Risk Using PREVENT Equations
#'
#' @description
#' Calculate 10-year and 30-year cardiovascular disease risk using the PREVENT
#' (Predicting Risk of cardiovascular disease EVENTs) equations.
#'
#' @export
estimate_risk <- preventr_estimate_risk

#' @rdname estimate_risk
#' @export
est_risk <- preventr_estimate_risk
