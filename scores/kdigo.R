kigo_g_cat <- function(egfr) {
  case_when(
    is.na(egfr)        ~ NA_character_,
    egfr >= 90         ~ "G1",
    egfr >= 60         ~ "G2",
    egfr >= 45         ~ "G3a",
    egfr >= 30         ~ "G3b",
    egfr >= 15         ~ "G4",
    egfr < 15          ~ "G5"
  )
}

kdigo_a_cat <- function(uacr_mg_g) {
  case_when(
    is.na(uacr_mg_g)      ~ NA_character_,
    uacr_mg_g < 30        ~ "A1",
    uacr_mg_g <= 300      ~ "A2",
    uacr_mg_g > 300       ~ "A3"
  )
}

kdigo_risk_cat <- function(g_cat, a_cat) {
  # KDIGO heatmap mapping used commonly in CKD staging:
  # Moderate: G1A2, G2A2, G3aA1
  # High:     G1A3, G2A3, G3aA2, G3bA1
  # Very high:G3aA3, G3bA2, G3bA3, any G4 or G5 with A1-3
  # Low:      everything else with CKD present (often G1A1/G2A1 is "low/none" unless other markers)

  ga <- ifelse(is.na(g_cat) | is.na(a_cat), NA_character_, paste0(g_cat, a_cat))

  case_when(
    is.na(ga) ~ NA_character_,
    ga %in% c("G1A2", "G2A2", "G3aA1") ~ "moderate",
    ga %in% c("G1A3", "G2A3", "G3aA2", "G3bA1") ~ "high",
    ga %in% c("G3aA3", "G3bA2", "G3bA3",
              "G4A1","G4A2","G4A3",
              "G5A1","G5A2","G5A3") ~ "very_high",
    TRUE ~ "low"
  )
}