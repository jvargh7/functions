# female: 1,0
# age: continuous
# smoke_current = 1, 0
# sbp = mmHg
# total_cholesterol = mg/dL
# hdl = mg/dL
# dx_dm = 1,0
# hba1c = %
# fpg = mg/dL
# dmagediag = age
# serum_creatinine 
# rx_bp, rx_chol = 1, 0
# is_asian = 1, 0
# bmi: kg/m2
# physically_active: 1,0
# uacr_mg_g: mg/g
# dx_mi_stroke: 1,0
# dx_gdm = 1,0
# famdx_dm = 1,0


# source("functions/preventr.R")
source("functions/preventr_vectorized.R")
source("functions/nephro.R")
source("functions/kdigo.R")
source("functions/RECODe.R")

clip_lo_hi <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# data = df_harmonized
risk_scores = function(data){

  df_score2 = data |> 
    mutate(
      
      
      sex_score2 = case_when(female == 1 ~ "female",
                             TRUE ~ "male"),
      
      
      # ---- Demographics ----
      age_score2 = case_when(is.na(age) ~ NA_real_,
                             age < 40 ~ NA_real_,
                             age >=70 ~ NA_real_,
                             TRUE ~ as.numeric(age)),

      age_score2op = case_when(is.na(age) ~ NA_real_,
                             age < 70 ~ NA_real_,
                             age >=70 ~ as.numeric(age)),

      cage_score2 = (age_score2-60)/5,
      cage_score2op = (age_score2op - 73),


      # ---- Smoking ----
      smoke_score2 = case_when(smoke_current == 1 ~ 1,
                               TRUE ~ 0),

      # ---- Blood pressure / HTN ----
      # Use averaged SBP/DBP from the three available readings
      sbp_score2 = sbp,
      csbp_score2 = (sbp_score2 - 120)/20,
      csbp_score2op = sbp_score2 - 150,

      # ---- Total Cholesterol ----
      # Convert mg/dL to mmol/L
      tc_score2_mmol_l = total_cholesterol/ 38.67, 
      ctchol_score2 = (tc_score2_mmol_l - 6),

      # ---- HDL Cholesterol ----
      hdl_score2_mmol_l = hdl/38.67,
      chdl_score2 = (hdl_score2_mmol_l-1.3)/0.5,
      chdl_score2op = hdl_score2_mmol_l - 1.4,

      dm_self = if_else(dx_dm == 1, TRUE, FALSE, missing = FALSE),
      diabetes_score2 = case_when(
        is.na(dm_self) & is.na(hba1c) & is.na(fpg) ~ NA,
        dm_self ~ 1,
        (!is.na(hba1c) & hba1c >= 6.5) | (!is.na(fpg) & fpg >= 126) ~ 1,
        TRUE ~ 0),

      dmagediag_score2dm = case_when(
        diabetes_score2 == 1 & dm_self == 1 ~ dmagediag,
        diabetes_score2 == 1 ~ age,
        TRUE ~ NA_real_),
      cagediab_score2dm = diabetes_score2*(dmagediag_score2dm-50)/5,


      # ---- Glycated Hemoglobin ----
      # https://pmc.ncbi.nlm.nih.gov/articles/PMC10398719/
      hba1c_score2dm_mmol_l = 10.929 * (hba1c -2.15),
      chba1c_score2dm = (hba1c_score2dm_mmol_l-31)/9.34,

      # ----------- eGFR CKD-EPI 2021 -----------
      egfr_score2dm = egfr_ckdepi_2021_creat(scr = serum_creatinine, age = age, sex = female),
      cegfr_score2dm = (log(egfr_score2dm)-4.5)/0.15,
      cegfr_score2dm_sq = cegfr_score2dm^2,
    

      
      bp_meds_score2 = if_else(rx_bp == 1, "yes", "no", missing = "no"),
      
      # BASELINE -------------------
      score2_10y_cvd_baseline = case_when(
        sex_score2=="male" ~ 
          0.3742*cage_score2 + 
          0.6012*(smoke_score2 == 1) +  
          0.2777*csbp_score2  + 
          0.1458*ctchol_score2  
          - 0.2698*chdl_score2 
          - 0.0755*(smoke_score2 == 1)*cage_score2
          - 0.0255*csbp_score2*cage_score2
          - 0.0281*ctchol_score2*cage_score2
          + 0.0426*chdl_score2*cage_score2,
          sex_score2 == "female" ~ 
          0.4648*cage_score2 + 
          0.7744*(smoke_score2 == 1) +  
          0.3131*csbp_score2  + 
          0.1002*ctchol_score2  
          - 0.2606*chdl_score2 
          - 0.1088*(smoke_score2 == 1)*cage_score2
          - 0.0277*csbp_score2*cage_score2
          - 0.0226*ctchol_score2*cage_score2
          + 0.0613*chdl_score2*cage_score2,
          TRUE ~ NA_real_), 

           # OLDER -------------------

      score2op_10y_cvd_baseline = case_when(
        sex_score2=="male" ~ 
          0.0634*cage_score2op +
          0.4245*diabetes_score2 +
          0.3524*(smoke_score2 == 1) +  
          0.0094*csbp_score2op  + 
          0.0850*ctchol_score2 
          - 0.3564*chdl_score2op 
          - 0.0174*diabetes_score2*cage_score2op   
          - 0.0247*(smoke_score2 == 1)*cage_score2op
          - 0.0005*csbp_score2op*cage_score2op
          + 0.0073*ctchol_score2*cage_score2op
          + 0.0091*chdl_score2op*cage_score2op,
          sex_score2 == "female" ~ 
          0.0789*cage_score2op + 
          0.6010*diabetes_score2 +
          0.4921*(smoke_score2 == 1) +  
          0.0102*csbp_score2op  + 
          0.0605*ctchol_score2  
          - 0.3040*chdl_score2op 
          - 0.0107*diabetes_score2*cage_score2op   
          - 0.0255*(smoke_score2 == 1)*cage_score2op
          - 0.0004*csbp_score2op*cage_score2op
          - 0.0009*ctchol_score2*cage_score2op
          + 0.0154*chdl_score2op*cage_score2op,
          TRUE ~ NA_real_),

           # Diabetes -------------------
      score2dm_10y_cvd_baseline = case_when(
        sex_score2=="male" ~ 
          0.5368*cage_score2 +
          0.4774*(smoke_score2 == 1) +  
          0.1322*csbp_score2  + 
          0.6457*diabetes_score2 +
          0.1102*ctchol_score2 
          - 0.1087*chdl_score2 
          - 0.0672*(smoke_score2 == 1)*cage_score2
          - 0.0268*csbp_score2*cage_score2
          - 0.0983*diabetes_score2*cage_score2   
          - 0.0181*ctchol_score2*cage_score2
          + 0.0095*chdl_score2*cage_score2
          - 0.0998*cagediab_score2dm
          + 0.0955*chba1c_score2dm
          - 0.0591*cegfr_score2dm
          + 0.0058*cegfr_score2dm_sq
          - 0.0134*chba1c_score2dm*cage_score2
          + 0.0115*cegfr_score2dm*cage_score2
          ,

          sex_score2 == "female" ~ 
          0.6624*cage_score2 +
          0.6139*(smoke_score2 == 1) +  
          0.1421*csbp_score2  + 
          0.8096*diabetes_score2 +
          0.1127*ctchol_score2 
          - 0.1568*chdl_score2 
          - 0.1122*(smoke_score2 == 1)*cage_score2
          - 0.0167*csbp_score2*cage_score2
          - 0.1272*diabetes_score2*cage_score2   
          - 0.0200*ctchol_score2*cage_score2
          + 0.0186*chdl_score2*cage_score2
          - 0.1180*cagediab_score2dm
          + 0.1173*chba1c_score2dm
          - 0.0640*cegfr_score2dm
          + 0.0062*cegfr_score2dm_sq
          - 0.0196*chba1c_score2dm*cage_score2
          + 0.0169*cegfr_score2dm*cage_score2,
          TRUE ~ NA_real_) 
        
      
    ) |>
    mutate(score2_10y_cvd_risk_manual = case_when(
      sex_score2 == "male" ~ 1 - (0.9605)^(exp(score2_10y_cvd_baseline)),
      sex_score2 == "female" ~ 1 - (0.9776)^(exp(score2_10y_cvd_baseline)),
      TRUE ~ NA_real_),
    
    score2op_10y_cvd_risk_manual = case_when(
      sex_score2 == "male" ~ 1 - (0.7576)^(exp(score2op_10y_cvd_baseline -0.0929)),
      sex_score2 == "female" ~ 1 - (0.8082)^(exp(score2op_10y_cvd_baseline -0.2290)),
      TRUE ~ NA_real_),
    
    score2dm_10y_cvd_risk_manual = case_when(
      sex_score2 == "male" ~ 1 - (0.9605)^(exp(score2dm_10y_cvd_baseline)),
      sex_score2 == "female" ~ 1 - (0.9776)^(exp(score2dm_10y_cvd_baseline)),
      TRUE ~ NA_real_)
    
  )

  df_prevent = data |> 
  mutate(age_prev = case_when(
        is.na(age) ~ NA_real_,
        age < 30 ~ NA_real_,
        age >= 80 ~ 79,
        TRUE ~ as.numeric(age)
      ),
    sex_prev = case_when(female == 1 ~ "female",
                        TRUE ~ "male"),
    bp_meds_prev = case_when(
      is.na(rx_bp) ~ FALSE,
      TRUE ~ rx_bp),
      # PREVENT allowable bounds in supplement: TC 130-320, HDL 20-100, SBP 90-180, eGFR 15-140.
      tc_b  = clip_lo_hi(total_cholesterol,   130, 320),
      hdl_b = clip_lo_hi(hdl,   20, 100),
      statin_prev = case_when(
        is.na(rx_chol) ~ FALSE,
        TRUE ~ rx_chol),
      sbp_b = clip_lo_hi(sbp,   90, 180),
      smoking_prev = case_when(smoke_current == 1 ~ TRUE,TRUE ~ FALSE), 
      dm_self = if_else(dx_dm == 1, TRUE, FALSE, missing = FALSE),
      diabetes_prev = case_when(
        is.na(dm_self) & is.na(hba1c) & is.na(fpg) ~ NA,
        dm_self ~ 1,
        (!is.na(hba1c) & hba1c >= 6.5) | (!is.na(fpg) & fpg >= 126) ~ 1,
        TRUE ~ 0),
      egfr_prev = egfr_ckdepi_2021_creat(scr = serum_creatinine, age = age, sex = female),
      
      bmi_prev = case_when(
        is.na(bmi) ~ NA_real_,
        bmi < 18.5 ~ 18.5,
        bmi >= 40 ~ 39.9,
        TRUE ~ as.numeric(bmi)
      )) |> 
    mutate(egfr_b= clip_lo_hi(egfr_prev,  15, 140)) |>
      dplyr::select(respondentid,
        age_prev, sex_prev, bmi_prev, bp_meds_prev, sbp_b, tc_b, hdl_b, egfr_prev,egfr_b,smoking_prev, 
        statin_prev, bp_meds_prev, diabetes_prev)

    
    print(names(df_prevent))
  
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

  
  df_kdigo = data |> 
    mutate(
      g_cat = kigo_g_cat(egfr = egfr_ckdepi_2021_creat(scr = serum_creatinine, age = age, sex = female)),
    a_cat = kdigo_a_cat(uacr_mg_g = uacr_mg_g),
    kdigo_risk = kdigo_risk_cat(g_cat = g_cat, a_cat = a_cat)
    ) |> 
    dplyr::select(respondentid, g_cat, a_cat, kdigo_risk)
  
  
  df_recode = data    |>
    rename(
      sex = female,
      age = age,
      black = black,
      hisp = hispanic,
      cursmoke = smoke_current,
      anticoag = rx_anticoagulant,
      cvdhist = dx_mi_stroke,
      sysbp = sbp,
      totchol = total_cholesterol,
      hdlchol = hdl,
      hgba1c = hba1c,
      sercreat = serum_creatinine,
      bprx = rx_bp,
      statin = rx_chol,
      oralrx = rx_dm_oral,
      uralbcreat = uacr_mg_g
    )
  
  df_ada_cdc_risk = data |> 
    mutate(diabetes_ac = case_when(
        is.na(dx_dm) & is.na(hba1c) & is.na(fpg) ~ NA,
        dx_dm == 1 ~ 1,
        (!is.na(hba1c) & hba1c >= 6.5) | (!is.na(fpg) & fpg >= 126) ~ 1,
        TRUE ~ 0)) |> 
    mutate(
      ac_age = case_when(
        age >= 40 & age <=49 ~ 1,
        age >= 50 & age <=59 ~ 2,
        age >= 60 ~ 3,
        TRUE ~ 0
      ),
      ac_sex = case_when(female == 1 ~ 0,
                         TRUE ~ 1),
      ac_gestational_dm = case_when(dx_gdm == 1 ~ 1,
                                    TRUE ~ 0),
      ac_family_history_dm = case_when(famdx_dm == 1 ~ 1,
                                        TRUE ~ 0),
      ac_physically_active = case_when(physically_active == 1 ~ 0,
                                      TRUE ~ 1),
      ac_bmi = case_when(bmi < 25 ~ 0,
                         bmi >=25 & bmi <30 ~ 1,
                         bmi >=30 & bmi <40 ~ 2,
                         bmi >= 40 ~ 3,
                         TRUE ~ 0)
    ) |> 
    mutate(ac_score = ac_age + ac_sex + ac_gestational_dm + ac_family_history_dm + ac_physically_active + ac_bmi) |> 
    mutate(ac_risk = case_when(
      diabetes_ac == 1 ~ NA_character_,
      ac_score >=5 ~ "predm_risk",
     ac_score <5 ~ "not predm_risk",
      TRUE ~ NA_character_
    )) |> 
    dplyr::select(respondentid, starts_with("ac_"))
  
  
  RECODe_risk_out = RECODe_risk(df_recode)

    
  return(list(df_score2,prevent_10y_total_cvd,df_kdigo, RECODe_risk_out,df_ada_cdc_risk))




}




