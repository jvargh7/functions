# ============================================================================
# STANDALONE NEPHRO PACKAGE FUNCTIONS
# ============================================================================
# This file contains a standalone implementation of key nephro package functions
# for calculating eGFR using CKD-EPI equations without external package dependencies.
# 
# MAIN DEPENDENCIES:
# - Base R only (no external packages required)
#
# FUNCTIONS INCLUDED:
# - egfr_ckdepi_2021_creat() - CKD-EPI 2021 creatinine equation (race-free)
# - egfr_ckdepi_2021_cys() - CKD-EPI 2021 cystatin C equation
# - egfr_ckdepi_2021_creat_cys() - CKD-EPI 2021 combined creatinine-cystatin C equation
# - egfr_ckdepi_2009() - Original CKD-EPI 2009 creatinine equation (with race)
# - Helper functions for input validation and unit conversion
#
# REFERENCE:
# Inker LA, et al. New Creatinine- and Cystatin C-Based Equations to Estimate GFR 
# without Race. N Engl J Med. 2021;385(19):1737-1749.
#
# ============================================================================

# --- INPUT VALIDATION AND HELPER FUNCTIONS ---

#' Check if value is within valid range
#' @param x numeric vector
#' @param min_val minimum valid value
#' @param max_val maximum valid value
#' @param var_name variable name for error messages
is_valid_range <- function(x, min_val, max_val, var_name = "value") {
  if (any(!is.na(x) & (x < min_val | x > max_val), na.rm = TRUE)) {
    warning(paste0("Some ", var_name, " values are outside expected range (", 
                   min_val, "-", max_val, ")"))
  }
  return(TRUE)
}

#' Validate age input
#' @param age numeric vector of ages in years
is_valid_age_nephro <- function(age) {
  if (any(!is.na(age) & (age < 18 | age > 120), na.rm = TRUE)) {
    warning("Some age values are outside expected range (18-120 years)")
  }
  return(TRUE)
}

#' Standardize sex input
#' @param sex character vector or numeric (0/1) for sex
standardize_sex_nephro <- function(sex) {
  if (is.numeric(sex)) {
    # Assume 0 = male, 1 = female
    return(ifelse(sex == 1, "female", "male"))
  } else if (is.character(sex)) {
    sex_lower <- tolower(trimws(sex))
    sex_std <- ifelse(sex_lower %in% c("f", "female", "woman", "w"), "female",
               ifelse(sex_lower %in% c("m", "male", "man"), "male", NA_character_))
    if (any(is.na(sex_std) & !is.na(sex))) {
      warning("Some sex values could not be standardized")
    }
    return(sex_std)
  }
  return(sex)
}

#' Convert creatinine units
#' @param scr serum creatinine value
#' @param from_unit current unit ("mg/dL" or "umol/L")
#' @param to_unit target unit ("mg/dL" or "umol/L") 
convert_creatinine_units <- function(scr, from_unit = "mg/dL", to_unit = "mg/dL") {
  if (from_unit == to_unit) return(scr)
  
  if (from_unit == "mg/dL" && to_unit == "umol/L") {
    return(scr * 88.42)
  } else if (from_unit == "umol/L" && to_unit == "mg/dL") {
    return(scr / 88.42)
  } else {
    stop("Unsupported unit conversion. Use 'mg/dL' or 'umol/L'")
  }
}

#' Convert cystatin C units
#' @param cys cystatin C value
#' @param from_unit current unit ("mg/L" or "mg/dL")
#' @param to_unit target unit ("mg/L" or "mg/dL")
convert_cystatin_units <- function(cys, from_unit = "mg/L", to_unit = "mg/L") {
  if (from_unit == to_unit) return(cys)
  
  if (from_unit == "mg/L" && to_unit == "mg/dL") {
    return(cys / 10)
  } else if (from_unit == "mg/dL" && to_unit == "mg/L") {
    return(cys * 10)
  } else {
    stop("Unsupported unit conversion. Use 'mg/L' or 'mg/dL'")
  }
}

# --- MAIN eGFR CALCULATION FUNCTIONS ---

#' CKD-EPI 2021 eGFR equation using creatinine (race-free)
#' @param scr serum creatinine in mg/dL
#' @param age age in years  
#' @param sex sex ("male"/"female" or 0/1 where 1=female)
#' @param scr_unit unit for creatinine ("mg/dL" or "umol/L")
#' @return estimated GFR in mL/min/1.73m²
#' @export
egfr_ckdepi_2021_creat <- function(scr, age, sex, scr_unit = "mg/dL") {
  
  # Input validation
  if (missing(scr) || missing(age) || missing(sex)) {
    stop("scr, age, and sex are required")
  }
  
  # Convert units if needed
  if (scr_unit == "umol/L") {
    scr <- convert_creatinine_units(scr, "umol/L", "mg/dL")
  }
  
  # Validate inputs
  is_valid_range(scr, 0.1, 15, "serum creatinine")
  is_valid_age_nephro(age)
  
  # Standardize sex
  sex <- standardize_sex_nephro(sex)
  
  # Calculate eGFR using CKD-EPI 2021 equation (race-free)
  # Female: eGFR = 142 × min(Scr/0.7, 1)^(-0.241) × max(Scr/0.7, 1)^(-1.200) × 0.9938^age [× 1.012 if female]
  # Male:   eGFR = 142 × min(Scr/0.9, 1)^(-0.302) × max(Scr/0.9, 1)^(-1.200) × 0.9938^age
  
  kappa <- ifelse(sex == "female", 0.7, 0.9)
  alpha <- ifelse(sex == "female", -0.241, -0.302)
  sex_factor <- ifelse(sex == "female", 1.012, 1.0)
  
  # Calculate components
  scr_kappa <- scr / kappa
  min_term <- pmin(scr_kappa, 1)^alpha
  max_term <- pmax(scr_kappa, 1)^(-1.200)
  age_term <- 0.9938^age
  
  # Final calculation
  egfr <- 142 * min_term * max_term * age_term * sex_factor
  
  return(egfr)
}

#' CKD-EPI 2021 eGFR equation using cystatin C
#' @param cys serum cystatin C in mg/L
#' @param age age in years
#' @param sex sex ("male"/"female" or 0/1 where 1=female)  
#' @param cys_unit unit for cystatin C ("mg/L" or "mg/dL")
#' @return estimated GFR in mL/min/1.73m²
#' @export
egfr_ckdepi_2021_cys <- function(cys, age, sex, cys_unit = "mg/L") {
  
  # Input validation
  if (missing(cys) || missing(age) || missing(sex)) {
    stop("cys, age, and sex are required")
  }
  
  # Convert units if needed
  if (cys_unit == "mg/dL") {
    cys <- convert_cystatin_units(cys, "mg/dL", "mg/L")
  }
  
  # Validate inputs
  is_valid_range(cys, 0.1, 8, "serum cystatin C")
  is_valid_age_nephro(age)
  
  # Standardize sex
  sex <- standardize_sex_nephro(sex)
  
  # Calculate eGFR using CKD-EPI 2021 cystatin C equation
  # eGFR = 133 × min(Scys/0.8, 1)^(-0.499) × max(Scys/0.8, 1)^(-1.328) × 0.996^age [× 0.932 if female]
  
  sex_factor <- ifelse(sex == "female", 0.932, 1.0)
  
  # Calculate components
  cys_ratio <- cys / 0.8
  min_term <- pmin(cys_ratio, 1)^(-0.499)
  max_term <- pmax(cys_ratio, 1)^(-1.328)
  age_term <- 0.996^age
  
  # Final calculation
  egfr <- 133 * min_term * max_term * age_term * sex_factor
  
  return(egfr)
}

#' CKD-EPI 2021 eGFR equation using both creatinine and cystatin C
#' @param scr serum creatinine in mg/dL
#' @param cys serum cystatin C in mg/L
#' @param age age in years
#' @param sex sex ("male"/"female" or 0/1 where 1=female)
#' @param scr_unit unit for creatinine ("mg/dL" or "umol/L")
#' @param cys_unit unit for cystatin C ("mg/L" or "mg/dL")
#' @return estimated GFR in mL/min/1.73m²
#' @export
egfr_ckdepi_2021_creat_cys <- function(scr, cys, age, sex, 
                                       scr_unit = "mg/dL", cys_unit = "mg/L") {
  
  # Input validation
  if (missing(scr) || missing(cys) || missing(age) || missing(sex)) {
    stop("scr, cys, age, and sex are required")
  }
  
  # Convert units if needed
  if (scr_unit == "umol/L") {
    scr <- convert_creatinine_units(scr, "umol/L", "mg/dL")
  }
  if (cys_unit == "mg/dL") {
    cys <- convert_cystatin_units(cys, "mg/dL", "mg/L")
  }
  
  # Validate inputs
  is_valid_range(scr, 0.1, 15, "serum creatinine")
  is_valid_range(cys, 0.1, 8, "serum cystatin C")
  is_valid_age_nephro(age)
  
  # Standardize sex
  sex <- standardize_sex_nephro(sex)
  
  # Calculate eGFR using CKD-EPI 2021 combined equation
  # eGFR = 135 × min(Scr/κ, 1)^α × max(Scr/κ, 1)^(-0.544) × min(Scys/0.8, 1)^(-0.323) × 
  #        max(Scys/0.8, 1)^(-0.778) × 0.9961^age [× 0.963 if female]
  
  # Creatinine parameters
  kappa <- ifelse(sex == "female", 0.7, 0.9)
  alpha <- ifelse(sex == "female", -0.219, -0.144)
  sex_factor <- ifelse(sex == "female", 0.963, 1.0)
  
  # Calculate creatinine components
  scr_kappa <- scr / kappa
  scr_min_term <- pmin(scr_kappa, 1)^alpha
  scr_max_term <- pmax(scr_kappa, 1)^(-0.544)
  
  # Calculate cystatin C components
  cys_ratio <- cys / 0.8
  cys_min_term <- pmin(cys_ratio, 1)^(-0.323)
  cys_max_term <- pmax(cys_ratio, 1)^(-0.778)
  
  # Age component
  age_term <- 0.9961^age
  
  # Final calculation
  egfr <- 135 * scr_min_term * scr_max_term * cys_min_term * cys_max_term * age_term * sex_factor
  
  return(egfr)
}

#' CKD-EPI 2009 eGFR equation using creatinine (includes race)
#' @param scr serum creatinine in mg/dL
#' @param age age in years
#' @param sex sex ("male"/"female" or 0/1 where 1=female)
#' @param race race ("black"/"african american" vs other)
#' @param scr_unit unit for creatinine ("mg/dL" or "umol/L")
#' @return estimated GFR in mL/min/1.73m²
#' @export
egfr_ckdepi_2009 <- function(scr, age, sex, race = "other", scr_unit = "mg/dL") {
  
  # Input validation
  if (missing(scr) || missing(age) || missing(sex)) {
    stop("scr, age, and sex are required")
  }
  
  # Convert units if needed
  if (scr_unit == "umol/L") {
    scr <- convert_creatinine_units(scr, "umol/L", "mg/dL")
  }
  
  # Validate inputs
  is_valid_range(scr, 0.1, 15, "serum creatinine")
  is_valid_age_nephro(age)
  
  # Standardize sex and race
  sex <- standardize_sex_nephro(sex)
  race_lower <- tolower(trimws(race))
  is_black <- race_lower %in% c("black", "african american", "aa", "b")
  
  # Calculate eGFR using CKD-EPI 2009 equation
  kappa <- ifelse(sex == "female", 0.7, 0.9)
  alpha <- ifelse(sex == "female", -0.329, -0.411)
  sex_factor <- ifelse(sex == "female", 1.018, 1.0)
  race_factor <- ifelse(is_black, 1.159, 1.0)
  
  # Calculate components
  scr_kappa <- scr / kappa
  min_term <- pmin(scr_kappa, 1)^alpha
  max_term <- pmax(scr_kappa, 1)^(-1.209)
  age_term <- 0.993^age
  
  # Final calculation
  egfr <- 141 * min_term * max_term * age_term * sex_factor * race_factor
  
  return(egfr)
}

# --- WRAPPER FUNCTIONS FOR COMPATIBILITY ---

# Alias for the main function used in your code
egfr_ckdepi_2021 <- egfr_ckdepi_2021_creat

# Additional aliases for different naming conventions
eGFR_ckdepi_2021_creat <- egfr_ckdepi_2021_creat
eGFR_ckdepi_2021_cys <- egfr_ckdepi_2021_cys
eGFR_ckdepi_2021_creat_cys <- egfr_ckdepi_2021_creat_cys

# --- PACKAGE SIMULATION FOR NEPHRO NAMESPACE ---

# Create a pseudo-namespace for nephro package calls
nephro <- list(
  egfr_ckdepi_2021_creat = egfr_ckdepi_2021_creat,
  egfr_ckdepi_2021_cys = egfr_ckdepi_2021_cys,
  egfr_ckdepi_2021_creat_cys = egfr_ckdepi_2021_creat_cys,
  egfr_ckdepi_2009 = egfr_ckdepi_2009
)

# --- EXAMPLE USAGE ---

# Examples of how to use these functions:
# 
# # Basic usage with CKD-EPI 2021 creatinine equation
# egfr1 <- egfr_ckdepi_2021_creat(scr = 1.2, age = 65, sex = "male")
# 
# # With different units
# egfr2 <- egfr_ckdepi_2021_creat(scr = 106, age = 65, sex = "female", scr_unit = "umol/L")
# 
# # Using cystatin C
# egfr3 <- egfr_ckdepi_2021_cys(cys = 1.1, age = 65, sex = "male")
# 
# # Combined creatinine and cystatin C (most accurate)
# egfr4 <- egfr_ckdepi_2021_creat_cys(scr = 1.2, cys = 1.1, age = 65, sex = "male")
# 
# # Original 2009 equation (with race)
# egfr5 <- egfr_ckdepi_2009(scr = 1.2, age = 65, sex = "male", race = "black")
# 
# # Using with vectors (multiple patients)
# patients <- data.frame(
#   scr = c(0.9, 1.2, 1.5, 2.1),
#   age = c(45, 65, 70, 80), 
#   sex = c("female", "male", "female", "male")
# )
# 
# patients$egfr <- with(patients, egfr_ckdepi_2021_creat(scr, age, sex))