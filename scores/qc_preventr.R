
dat <- make_vignette_dat() 

source("scores/risk_scores.R")

df_prevent = readxl::read_excel("scores/qc_preventr dat.xlsx",sheet="Sheet1") |> 
  mutate(respondentid = row_number()) |> 
  mutate(age_prev = case_when(
        is.na(age) ~ NA_real_,
        age < 30 ~ NA_real_,
        age >= 80 ~ 79,
        TRUE ~ as.numeric(age)
      ),
    sex_prev = sex,
    bp_meds_prev = bp_tx,
      # PREVENT allowable bounds in supplement: TC 130-320, HDL 20-100, SBP 90-180, eGFR 15-140.
      tc_b  = clip_lo_hi(total_c,   130, 320),
      hdl_b = clip_lo_hi(hdl_c,   20, 100),
      statin_prev =statin,
      sbp_b = clip_lo_hi(sbp,   90, 180),
      smoking_prev = smoking, 
      diabetes_prev = dm,
      egfr_prev = egfr,
      
      bmi_prev = bmi) |> 
    mutate(egfr_b= clip_lo_hi(egfr_prev,  15, 140)) |>
      dplyr::select(respondentid,
        age_prev, sex_prev, bmi_prev, bp_meds_prev, sbp_b, tc_b, hdl_b, egfr_prev,egfr_b,smoking_prev, 
        statin_prev, bp_meds_prev, diabetes_prev)

prevent_10y_total_cvd = 
      estimate_risk_fast(data = df_prevent,
        # event naming can differ by package version; "total_cvd" is typical in preventr
        # event   = "total_cvd",
        # horizon = 10,
        age     = age_prev,
        sex     = sex_prev,
        time = "10yr",
        bp_tx = bp_meds_prev,
        total_c      = tc_b,
        hdl_c     = hdl_b,
        statin = statin_prev,
        sbp     = sbp_b,
        smoking = smoking_prev, # current smoker
        dm = diabetes_prev,
        egfr    = egfr_b,
        bmi = bmi_prev,
        # hba1c = glycohemoglobin,
        chol_unit = "mg/dL"
      )  

# Correct answers are in "qc_preventr dat.xlsx" sheet "Sheet2": https://cran.r-project.org/web/packages/preventr/vignettes/using-data-frame.html