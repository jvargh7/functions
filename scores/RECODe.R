# female: 1,0
# age: continuous
# NEW!! black
# NEW!! hispanic
# NEW!! anticoag --> rx_anticoagulant
# NEW!! history_mi_stroke --> dx_mi_stroke
# smoke_current = 1, 0 --> cursmoke
# sbp = mmHg --> sysbp
# total_cholesterol = mg/dL -->totchol
# hdl = mg/dL --> hdlchol
# dx_dm = 1,0
# hba1c = % --> hgba1c
# fpg = mg/dL
# dmagediag = age
# serum_creatinine -->sercreat 
# rx_bp --> bprx, rx_chol = 1, 0 --> statin
# is_asian = 1, 0
# bmi: kg/m2
# uacr_mg_g
# rx_dm_oral --> oralrx


# Input variables -----------

# sex~female: 1,
# age~age
# black~black: 1,0
# hisp~hispanic: 1, 0
# cursmoke ~ smoke_current: 1,0
# anticoag ~ rx_anticoagulant: 1,0
# cvdhist ~ dx_mi_stroke: 1,0
# sysbp ~ sbp: mmHg
# totchol ~ total_cholesterol: mg/dL
# hdlchol ~ hdl: mg/dL
# hgbpa1c ~ hba1c: %
# sercreat ~ serum_creatinine: mg/dL
# bprx ~ rx_bp: 1,0
# statin ~ rx_chol: 1,0
# oralrx ~ rx_dm_oral: 1,0
# uralbcreat ~ uacr_mg_g: mg/g




RECODe_risk = function(data) {
  
  df_RECODe_out = data |> 
    mutate(
      neph_lp = 0 + 
        -0.0193838993 * as.numeric(age) + 
        -0.0112943865 * as.numeric(sex) + 
        -0.0881241594 * as.numeric(black) + 
        0.2337712368 * as.numeric(hisp) + 
        0.0030271330 * as.numeric(sysbp) + 
        -0.0795168593 * as.numeric(bprx) + 
        0.1483078052 * as.numeric(cursmoke) +
        -0.0216363649 * as.numeric(cvdhist) + 
        -0.1255530728 * as.numeric(oralrx) + 
        0.8608801402 * as.numeric(sercreat) + 
        -0.0011121510 * as.numeric(totchol) + 
        0.0062888253 * as.numeric(hdlchol) +
        0.0319895697 * as.numeric(anticoag) + 
        0.1369126389 * as.numeric(hgba1c) +
        0.0003615507 * as.numeric(uralbcreat) -
        0.2269629,
      retin_lp = 0 + 
        0.0228504061 * as.numeric(age) + 
        0.2264337097 * as.numeric(sex) + 
        -0.1676573729 * as.numeric(black) + 
        0.0082431088 * as.numeric(sysbp) + 
        0.0639339678 * as.numeric(bprx) + 
        0.1127372373 * as.numeric(cvdhist) + 
        -0.2348989478 * as.numeric(oralrx) + 
        0.6946500975 * as.numeric(sercreat) + 
        -0.0001676169 * as.numeric(totchol) + 
        0.0054470159 * as.numeric(hdlchol) +
        0.1449446673 * as.numeric(hgba1c) +
        0.0001991881 * as.numeric(uralbcreat) -
        4.563441,
      neuro_lp = 0 + 
        3.022e-02 * as.numeric(age) + 
        -1.868e-01 * as.numeric(sex) + 
        -9.448e-02 * as.numeric(black) + 
        4.561e-03 * as.numeric(sysbp) + 
        1.819e-01 * as.numeric(bprx) + 
        2.667e-01 * as.numeric(cvdhist) + 
        -2.575e-01 * as.numeric(oralrx) + 
        6.044e-01 * as.numeric(sercreat) + 
        2.185e-03 * as.numeric(totchol) + 
        -5.389e-03 * as.numeric(hdlchol) +
        1.887e-01 * as.numeric(hgba1c) -
        4.746261,
      mi_lp = 0 + 
        0.034210 * as.numeric(age) + 
        -0.167200 * as.numeric(sex) + 
        -0.118700 * as.numeric(black) + 
        0.151000 * as.numeric(cursmoke) +
        0.000074 * as.numeric(sysbp) + 
        0.055790 * as.numeric(bprx) + 
        0.778400 * as.numeric(cvdhist) + 
        -0.033610 * as.numeric(statin) + 
        0.252400 * as.numeric(anticoag) +
        0.435500 * as.numeric(sercreat) + 
        0.001929 * as.numeric(totchol) + 
        -0.008370 * as.numeric(hdlchol) +
        0.171600 * as.numeric(hgba1c) +
        0.000333 * as.numeric(uralbcreat) -
        3.65,
      chf_lp = 0 + 
        5.268e-02 * as.numeric(age) + 
        2.529e-01 * as.numeric(sex) + 
        -4.969e-02 * as.numeric(black) + 
        2.905e-01 * as.numeric(cursmoke) +
        1.217e-03 * as.numeric(sysbp) + 
        6.389e-01 * as.numeric(bprx) + 
        1.007e00 * as.numeric(cvdhist) +
        -1.175e-01 * as.numeric(statin) +
        7.365e-01 * as.numeric(anticoag) +
        4.142e-04 * as.numeric(uralbcreat) + 
        8.214e-01 * as.numeric(sercreat) + 
        -1.358e-03 * as.numeric(totchol) + 
        -1.758e-02 * as.numeric(hdlchol) +
        2.092e-01 * as.numeric(hgba1c) -
        5.15,
      death_lp = 0 + 
        6.703e-02 * as.numeric(age) + 
        -1.529e-01 * as.numeric(sex) + 
        -2.393e-02 * as.numeric(black) + 
        5.399e-01 * as.numeric(cursmoke) +
        -2.988e-03 * as.numeric(sysbp) + 
        8.766e-02 * as.numeric(bprx) + 
        5.888e-01 * as.numeric(cvdhist) +
        -2.681e-01 * as.numeric(statin) +
        4.036e-01 * as.numeric(anticoag) +
        3.889e-04 * as.numeric(uralbcreat) + 
        3.597e-01 * as.numeric(sercreat) + 
        -9.478e-04 * as.numeric(totchol) + 
        -4.378e-03 * as.numeric(hdlchol) +
        1.659e-01 * as.numeric(hgba1c) -
        4.66
    ) |> 
    mutate(

      neph_risk = 100*(1-0.973^exp(neph_lp)),
      retin_risk = 100*(1-0.921^exp(retin_lp)),
      neuro_risk = 100*(1-0.870^exp(neuro_lp)),
      mi_risk = 100*(1-0.850^exp(mi_lp)),
      chf_risk = 100*(1-0.960^exp(chf_lp)),
      death_risk = 100*(1-0.930^exp(death_lp))
    ) |> 

    dplyr::select(respondentid,
      neph_lp,
      neph_risk,
      retin_lp,
      retin_risk,
      neuro_lp,
      neuro_risk,
      mi_lp,
      mi_risk,
      chf_lp,
      chf_risk,
      death_lp,
      death_risk
    )
  
  return(df_RECODe_out)
  
}
