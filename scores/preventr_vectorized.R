# ============================================================================
# VECTORIZED PREVENTR PACKAGE FUNCTIONS
# ============================================================================
# This is a high-performance vectorized implementation of the PREVENT
# cardiovascular disease risk calculator.
# 
# MAIN DEPENDENCIES:
# - dplyr (for data manipulation)
# - tidyr (for data reshaping)
#
# USAGE:
# source("functions/preventr_vectorized.R")
# result <- estimate_risk_fast(data = df, time = "10yr")
#
# PERFORMANCE:
# This vectorized version is significantly faster than the row-by-row
# implementation, especially for large datasets.
# ============================================================================

library(dplyr)
library(tidyr)

# --- UTILITY FUNCTIONS ---

convert_chol_to_mmol <- function(chol_mg_dl) {
  chol_mg_dl / 38.67
}

standardize_sex <- function(sex) {
  dplyr::case_when(
    sex %in% c("male", "m", "M", 0) ~ "male",
    sex %in% c("female", "f", "F", 1) ~ "female",
    TRUE ~ as.character(sex)
  )
}

# --- COEFFICIENT DATA ---

# Store coefficients as a tibble for easy lookup
get_prevent_coef_table <- function() {
  tibble::tribble(
    ~term,                  ~female_10yr, ~male_10yr, ~female_30yr, ~male_30yr,
    "age",                  0.794,        0.769,      0.550,        0.463,
    "age_squared",          0,            0,          -0.0928,      -0.0984,
    "non_hdl_c",            0.0305,       0.0736,     0.0410,       0.0836,
    "hdl_c",                -0.161,       -0.0954,    -0.166,       -0.103,
    "sbp_lt_110",           -0.239,       -0.435,     -0.163,       -0.214,
    "sbp_gte_110",          0.360,        0.336,      0.330,        0.290,
    "dm",                   0.867,        0.769,      0.679,        0.533,
    "smoking",              0.536,        0.439,      0.320,        0.214,
    "bmi_lt_30",            0,            0,          0,            0,
    "bmi_gte_30",           0,            0,          0,            0,
    "egfr_lt_60",           0.605,        0.538,      0.186,        0.116,
    "egfr_gte_60",          0.0434,       0.0165,     0.0554,       0.0604,
    "bp_tx",                0.315,        0.289,      0.289,        0.233,
    "statin",               -0.148,       -0.134,     -0.0757,      -0.0272,
    "bp_tx_sbp_gte_110",    -0.0664,      -0.0476,    -0.0564,      -0.0384,
    "statin_non_hdl_c",     0.120,        0.150,      0.107,        0.134,
    "age_non_hdl_c",        -0.0820,      -0.0518,    -0.0751,      -0.0512,
    "age_hdl_c",            0.0307,       0.0191,     0.0302,       0.0166,
    "age_sbp_gte_110",      -0.0946,      -0.105,     -0.0999,      -0.110,
    "age_dm",               -0.271,       -0.225,     -0.321,       -0.259,
    "age_smoking",          -0.0787,      -0.0895,    -0.161,       -0.157,
    "age_bmi_gte_30",       0,            0,          0,            0,
    "age_egfr_lt_60",       -0.164,       -0.154,     -0.145,       -0.117,
    "constant",             -3.31,        -3.03,      -1.32,        -1.15
  )
}

# --- VECTORIZED RISK CALCULATION ---

#' Calculate PREVENT CVD Risk (Vectorized)
#'
#' @param data A data frame with patient data
#' @param age_var Column name for age (unquoted)
#' @param sex_var Column name for sex (unquoted)
#' @param sbp_var Column name for systolic blood pressure (unquoted)
#' @param bp_tx_var Column name for BP medication (unquoted)
#' @param total_c_var Column name for total cholesterol (unquoted)
#' @param hdl_c_var Column name for HDL cholesterol (unquoted)
#' @param statin_var Column name for statin use (unquoted)
#' @param dm_var Column name for diabetes (unquoted)
#' @param smoking_var Column name for smoking status (unquoted)
#' @param egfr_var Column name for eGFR (unquoted)
#' @param bmi_var Column name for BMI (unquoted)
#' @param time Time horizon: "10yr", "30yr", or "both"
#' @param chol_unit Cholesterol units: "mg/dL" or "mmol/L"
#' @param model Model type: currently only "base" is supported
#' @param quiet Suppress messages
#' @return A data frame with risk estimates
#' @export
estimate_risk_fast <- function(data,
                                age_var = age,
                                sex_var = sex,
                                sbp_var = sbp,
                                bp_tx_var = bp_tx,
                                total_c_var = total_c,
                                hdl_c_var = hdl_c,
                                statin_var = statin,
                                dm_var = dm,
                                smoking_var = smoking,
                                egfr_var = egfr,
                                bmi_var = bmi,
                                time = "10yr",
                                chol_unit = "mg/dL",
                                model = "base",
                                quiet = FALSE) {
  
  # Capture variable names
  age_col <- rlang::enquo(age_var)
  sex_col <- rlang::enquo(sex_var)
  sbp_col <- rlang::enquo(sbp_var)
  bp_tx_col <- rlang::enquo(bp_tx_var)
  total_c_col <- rlang::enquo(total_c_var)
  hdl_c_col <- rlang::enquo(hdl_c_var)
  statin_col <- rlang::enquo(statin_var)
  dm_col <- rlang::enquo(dm_var)
  smoking_col <- rlang::enquo(smoking_var)
  egfr_col <- rlang::enquo(egfr_var)
  bmi_col <- rlang::enquo(bmi_var)
  
  # Validate time parameter
  if (!time %in% c("10yr", "30yr", "both")) {
    stop("time must be '10yr', '30yr', or 'both'")
  }
  
  # Determine which time horizons to calculate
  times_to_calc <- if (time == "both") c("10yr", "30yr") else time
  
  # Prepare data with standardized variable names
  prep_data <- data |>
    dplyr::mutate(
      age = !!age_col,
      sex = standardize_sex(!!sex_col),
      sbp = !!sbp_col,
      bp_tx = as.numeric(!!bp_tx_col),
      total_c = !!total_c_col,
      hdl_c = !!hdl_c_col,
      statin = as.numeric(!!statin_col),
      dm = as.numeric(!!dm_col),
      smoking = as.numeric(!!smoking_col),
      egfr = !!egfr_col,
      bmi = !!bmi_col,
      .row_id = dplyr::row_number()
    )
  
  # Convert cholesterol units if needed
  if (chol_unit == "mmol/L") {
    prep_data <- prep_data |>
      dplyr::mutate(
        total_c = total_c * 38.67,
        hdl_c = hdl_c * 38.67
      )
  }
  
  # Calculate risk for each time horizon
  results_list <- purrr::map(times_to_calc, function(time_horizon) {
    calculate_prevent_risk_vectorized(prep_data, time_horizon, quiet)
  })
  
  # Combine results
  if (time == "both") {
    names(results_list) <- paste0("risk_est_", times_to_calc)
    return(results_list)
  } else {
    return(results_list[[1]])
  }
}

#' Internal vectorized risk calculation
#' @keywords internal
calculate_prevent_risk_vectorized <- function(data, time, quiet = FALSE) {
  
  # Get coefficient table
  coef_table <- get_prevent_coef_table()
  
  # Determine which coefficient columns to use based on time
  coef_suffix <- if (time == "10yr") "_10yr" else "_30yr"
  
  # Calculate centered and scaled predictors (vectorized)
  pred_data <- data |>
    dplyr::mutate(
      # Convert cholesterol to mmol/L
      non_hdl_mmol = convert_chol_to_mmol(total_c - hdl_c),
      hdl_mmol = convert_chol_to_mmol(hdl_c),
      
      # Center and scale variables
      age_centered = (age - 55) / 10,
      age_squared = if (time == "30yr") age_centered^2 else 0,
      non_hdl_c = non_hdl_mmol - 3.5,
      hdl_c_scaled = (hdl_mmol - 1.3) / 0.3,
      sbp_lt_110 = (pmin(sbp, 110) - 110) / 20,
      sbp_gte_110 = (pmax(sbp, 110) - 130) / 20,
      bmi_lt_30 = (pmin(bmi, 30) - 25) / 5,
      bmi_gte_30 = (pmax(bmi, 30) - 30) / 5,
      egfr_lt_60 = (pmin(egfr, 60) - 60) / -15,
      egfr_gte_60 = (pmax(egfr, 60) - 90) / -15,
      
      # Interaction terms
      bp_tx_sbp_gte_110 = bp_tx * sbp_gte_110,
      statin_non_hdl_c = statin * non_hdl_c,
      age_non_hdl_c = age_centered * non_hdl_c,
      age_hdl_c = age_centered * hdl_c_scaled,
      age_sbp_gte_110 = age_centered * sbp_gte_110,
      age_dm = age_centered * dm,
      age_smoking = age_centered * smoking,
      age_bmi_gte_30 = age_centered * bmi_gte_30,
      age_egfr_lt_60 = age_centered * egfr_lt_60,
      constant = 1
    )
  
  # Calculate risk separately for males and females
  results <- pred_data |>
    dplyr::group_by(sex) |>
    dplyr::group_modify(~ {
      
      current_sex <- unique(.y$sex)
      coef_col <- paste0(current_sex, coef_suffix)
      
      # Get coefficients for this sex and time
      coefs <- coef_table |>
        dplyr::select(term, coef = !!rlang::sym(coef_col))
      
      # Prepare predictor matrix
      predictor_cols <- c(
        "age_centered", "age_squared", "non_hdl_c", "hdl_c_scaled",
        "sbp_lt_110", "sbp_gte_110", "dm", "smoking",
        "bmi_lt_30", "bmi_gte_30", "egfr_lt_60", "egfr_gte_60",
        "bp_tx", "statin", "bp_tx_sbp_gte_110", "statin_non_hdl_c",
        "age_non_hdl_c", "age_hdl_c", "age_sbp_gte_110", "age_dm",
        "age_smoking", "age_bmi_gte_30", "age_egfr_lt_60", "constant"
      )
      
      # Rename age_centered to age for coefficient matching
      pred_matrix <- .x |>
        dplyr::select(dplyr::all_of(predictor_cols)) |>
        dplyr::rename(age = age_centered, hdl_c = hdl_c_scaled)
      
      # Remove age_squared for 10yr model
      if (time == "10yr") {
        pred_matrix <- pred_matrix |>
          dplyr::select(-age_squared)
        coefs <- coefs |>
          dplyr::filter(term != "age_squared")
      }
      
      # Calculate linear predictor (matrix multiplication approach)
      # This is the vectorized magic - one calculation for all rows
      coef_vec <- coefs$coef
      names(coef_vec) <- coefs$term
      
      # Ensure column order matches coefficient order
      pred_matrix <- pred_matrix |>
        dplyr::select(dplyr::all_of(coefs$term))
      
      # Calculate log odds for all rows at once
      log_odds <- as.matrix(pred_matrix) %*% coef_vec
      
      # Convert to probability
      risk <- exp(log_odds) / (1 + exp(log_odds))
      
      # Add to data
      .x |>
        dplyr::mutate(
          total_cvd = round(as.numeric(risk), 3),
          ascvd = NA_real_,
          heart_failure = NA_real_,
          chd = NA_real_,
          stroke = NA_real_,
          model = "base",
          over_years = as.integer(gsub("yr", "", time)),
          input_problems = NA_character_
        )
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(.row_id) |>
    dplyr::select(-c(age_centered, age_squared, non_hdl_mmol, hdl_mmol,
                     non_hdl_c, hdl_c_scaled, sbp_lt_110, sbp_gte_110,
                     bmi_lt_30, bmi_gte_30, egfr_lt_60, egfr_gte_60,
                     bp_tx_sbp_gte_110, statin_non_hdl_c,
                     age_non_hdl_c, age_hdl_c, age_sbp_gte_110,
                     age_dm, age_smoking, age_bmi_gte_30, age_egfr_lt_60,
                     constant, .row_id))
  
  if (!quiet) {
    message("PREVENT estimates calculated using Base Model for ", time, " risk.")
  }
  
  return(results)
}

#' Simplified wrapper for common use case
#' 
#' @description
#' A simplified interface that assumes standard NHANES-style variable names.
#' 
#' @param data Data frame with patient data
#' @param time Time horizon: "10yr", "30yr", or "both"
#' @param quiet Suppress messages
#' @return Data frame with original data plus risk estimates
#' @export
estimate_prevent_risk <- function(data, time = "10yr", quiet = FALSE) {
  
  # Detect common variable naming patterns
  var_map <- list(
    age = c("age_prev", "age"),
    sex = c("sex_prev", "sex", "gender"),
    sbp = c("sbp_b", "sbp", "systolic"),
    bp_tx = c("bp_meds_prev", "bp_tx", "bp_med", "htn_tx"),
    total_c = c("tc_b", "total_c", "total_chol", "tchol"),
    hdl_c = c("hdl_b", "hdl_c", "hdl"),
    statin = c("statin_prev", "statin"),
    dm = c("diabetes_prev", "dm", "diabetes"),
    smoking = c("smoking_prev", "smoking", "smoker"),
    egfr = c("egfr_b", "egfr_prev", "egfr"),
    bmi = c("bmi_prev", "bmi")
  )
  
  # Find which variables exist in the data
  detected_vars <- list()
  for (var_name in names(var_map)) {
    possible_names <- var_map[[var_name]]
    found <- intersect(possible_names, names(data))
    if (length(found) > 0) {
      detected_vars[[var_name]] <- found[1]
    } else {
      stop("Could not find variable for ", var_name, 
           ". Expected one of: ", paste(possible_names, collapse = ", "))
    }
  }
  
  # Build the call dynamically
  estimate_risk_fast(
    data = data,
    age_var = !!rlang::sym(detected_vars$age),
    sex_var = !!rlang::sym(detected_vars$sex),
    sbp_var = !!rlang::sym(detected_vars$sbp),
    bp_tx_var = !!rlang::sym(detected_vars$bp_tx),
    total_c_var = !!rlang::sym(detected_vars$total_c),
    hdl_c_var = !!rlang::sym(detected_vars$hdl_c),
    statin_var = !!rlang::sym(detected_vars$statin),
    dm_var = !!rlang::sym(detected_vars$dm),
    smoking_var = !!rlang::sym(detected_vars$smoking),
    egfr_var = !!rlang::sym(detected_vars$egfr),
    bmi_var = !!rlang::sym(detected_vars$bmi),
    time = time,
    quiet = quiet
  )
}

# --- EXAMPLE USAGE ---
# 
# # Basic usage with auto-detection of variable names
# result <- estimate_prevent_risk(df_prevent, time = "10yr")
# 
# # Explicit variable specification
# result <- estimate_risk_fast(
#   data = my_data,
#   age_var = patient_age,
#   sex_var = patient_sex,
#   sbp_var = systolic_bp,
#   bp_tx_var = on_bp_meds,
#   total_c_var = total_cholesterol,
#   hdl_c_var = hdl_cholesterol,
#   statin_var = on_statin,
#   dm_var = has_diabetes,
#   smoking_var = is_smoker,
#   egfr_var = egfr_ckdepi,
#   bmi_var = body_mass_index,
#   time = "both"
# )
#
# # Calculate both 10-year and 30-year risk
# results_both <- estimate_prevent_risk(df_prevent, time = "both")
# risk_10yr <- results_both$risk_est_10yr
# risk_30yr <- results_both$risk_est_30yr
