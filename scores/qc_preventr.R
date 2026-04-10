# ============================================================================
# QC SCRIPT: Validate preventr_vectorized.R against preventr package
# ============================================================================
# Correct answers are in "qc_preventr dat.xlsx" sheet "Sheet2"
# Reference: https://cran.r-project.org/web/packages/preventr/vignettes/using-data-frame.html
#
# Sheet 1: 10 test patients with base + optional vars (hba1c, uacr, zip)
# Sheet 2: 20 correct result rows (10 respondents × 2 time horizons)
#
# Each respondent uses a different model depending on which optional vars
# are non-NA, selected per-row via select_prevent_model():
#   1=full, 2=uacr, 3=uacr, 4=hba1c, 5=base,
#   6=full, 7=sdi,  8=uacr, 9=base,  10=sdi
# ============================================================================

rm(list=ls())

library(dplyr)

source("scores/risk_scores.R")
source("scores/preventr_vectorized.R")

# --- Part 1: Base model only (original QC) ---

df_prevent <- readxl::read_excel("scores/qc_preventr dat.xlsx", sheet = "Sheet1") |>
  mutate(respondentid = row_number()) |>
  mutate(
    age_prev = case_when(
      is.na(age) ~ NA_real_,
      age < 30 ~ NA_real_,
      age >= 80 ~ 79,
      TRUE ~ as.numeric(age)
    ),
    sex_prev = sex,
    bp_meds_prev = bp_tx,
    # PREVENT allowable bounds in supplement: TC 130-320, HDL 20-100, SBP 90-180, eGFR 15-140.
    tc_b  = clip_lo_hi(total_c, 130, 320),
    hdl_b = clip_lo_hi(hdl_c,  20, 100),
    statin_prev = statin,
    sbp_b = clip_lo_hi(sbp,    90, 180),
    smoking_prev = smoking,
    diabetes_prev = dm,
    egfr_prev = egfr,
    bmi_prev = bmi
  ) |>
  mutate(egfr_b = clip_lo_hi(egfr_prev, 15, 140)) |>
  dplyr::select(
    respondentid,
    age_prev, sex_prev, bmi_prev, bp_meds_prev, sbp_b, tc_b, hdl_b,
    egfr_prev, egfr_b, smoking_prev, statin_prev, bp_meds_prev, diabetes_prev
  )

prevent_10y_total_cvd <- estimate_risk_fast(
  data    = df_prevent,
  age     = age_prev,
  sex     = sex_prev,
  time    = "10yr",
  bp_tx   = bp_meds_prev,
  total_c = tc_b,
  hdl_c   = hdl_b,
  statin  = statin_prev,
  sbp     = sbp_b,
  smoking = smoking_prev,
  dm      = diabetes_prev,
  egfr    = egfr_b,
  bmi     = bmi_prev,
  chol_unit = "mg/dL"
)

# --- Part 2: All models QC (base/hba1c/uacr/sdi/full) ---

# Read and clean input — hba1c/uacr/zip are character with literal "NA" in Excel
input_clean <- readxl::read_excel("scores/qc_preventr dat.xlsx", sheet = "Sheet1") |>
  mutate(
    respondentid = row_number(),
    hba1c = as.numeric(ifelse(hba1c == "NA", NA, hba1c)),
    uacr  = as.numeric(ifelse(uacr  == "NA", NA, uacr)),
    zip   = ifelse(zip == "NA", NA_character_, as.character(zip))
  )

# Per-row estimation: model is selected per row based on available optional vars
run_one_row <- function(i, input_df, th) {
  row_i <- input_df[i, ]
  mdl <- select_prevent_model(
    hba1c = row_i$hba1c,
    uacr  = row_i$uacr,
    zip   = row_i$zip
  )
  estimate_risk_fast(
    row_i,
    hba1c_var = hba1c, uacr_var = uacr, zip_var = zip,
    time = th, model = mdl, quiet = TRUE
  ) |>
    mutate(respondentid = row_i$respondentid, .before = 1)
}

res_10yr <- purrr::map_dfr(seq_len(nrow(input_clean)), run_one_row,
                            input_df = input_clean, th = "10yr")
res_30yr <- purrr::map_dfr(seq_len(nrow(input_clean)), run_one_row,
                            input_df = input_clean, th = "30yr")

our_results <- bind_rows(res_10yr, res_30yr)

# Load correct answers from Sheet 2
correct <- readxl::read_excel("scores/qc_preventr dat.xlsx", sheet = "Sheet2") |>
  mutate(
    hba1c = as.numeric(ifelse(hba1c == "NA", NA, hba1c)),
    uacr  = as.numeric(ifelse(uacr  == "NA", NA, uacr)),
    zip   = ifelse(zip == "NA", NA_character_, as.character(zip))
  )

# Compare our results to reference (all diffs should be 0)
comparison <- our_results |>
  select(respondentid, model, over_years,
         our_total_cvd = total_cvd, our_ascvd = ascvd,
         our_hf = heart_failure, our_chd = chd, our_stroke = stroke) |>
  inner_join(
    correct |> select(respondentid, model, over_years,
                      ref_total_cvd = total_cvd, ref_ascvd = ascvd,
                      ref_hf = heart_failure, ref_chd = chd, ref_stroke = stroke),
    by = c("respondentid", "model", "over_years")
  ) |>
  mutate(
    diff_total_cvd = our_total_cvd - ref_total_cvd,
    diff_ascvd     = our_ascvd     - ref_ascvd,
    diff_hf        = our_hf        - ref_hf,
    diff_chd       = our_chd       - ref_chd,
    diff_stroke    = our_stroke    - ref_stroke
  )

# Print summary — expect all zeros
comparison |>
  select(respondentid, model, over_years, starts_with("diff_")) |>
  print(n = 20)