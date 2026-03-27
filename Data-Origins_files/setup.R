options(repos = "https://cran.us.r-project.org")

# loads necessary packages
pacman::p_load(
  curl, ggpubr, gratia, gridExtra, gt, lme4, lubridate, mice, psych, rlang, scales, tidyverse, VIM, XML, yaml
)

# loads data files 

# AMNIOTES 
amniote <- readr::read_csv(
  file = "https://raw.githubusercontent.com/bygentry/LHDB-PCA/main/Data/AmnioteDB2015/Amniote_Database_Aug_2015.csv",
  col_names = TRUE,
  show_col_types = FALSE
)

# ANAGE
AnAge <- readr::read_tsv(
  file = "https://raw.githubusercontent.com/bygentry/LHDB-PCA/main/Data/AnAge/anage_data.txt", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# ERNEST
ernest <- readr::read_delim(
  file = "https://raw.githubusercontent.com/bygentry/LHDB-PCA/main/Data/Ernest/Mammal_lifehistories_v2.txt", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# PANTHERIA
pan <- readr::read_tsv(
  file = "https://raw.githubusercontent.com/bygentry/LHDB-PCA/main/Data/PanTHERIA/PanTHERIA_1-0_WR05_Aug2008.txt", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Kappeler & Pereira 
kp <- readr::read_csv(
  file = "https://raw.githubusercontent.com/bygentry/LHDB-PCA/main/Data/Kappeler-Pereira/KP.csv",
  col_names = TRUE,
  show_col_types = FALSE
)


## converts -999 ('no data' value) to NA
missing_val_to_na <- function(df){
  df |> mutate(across(where(is.numeric), ~ na_if(.x, -999)))
}

# creates subset dfs for primate rows and removes non mammal variables
amniote <- amniote %>% dplyr::filter(order == "Primates")
amniote <- missing_val_to_na(amniote)

AnAge <- AnAge %>% dplyr::filter(Order == "Primates")

ernest <- ernest %>% dplyr::filter(order == "Primates")
ernest <- missing_val_to_na(ernest)

pan <- pan %>% dplyr::filter(MSW05_Order == "Primates")
pan <- missing_val_to_na(pan)

# Kappeler & Pereira is already only primates, no filter necessary
## Point Correction: one value of BM.F has a "?" in it, making the object type character instead of numeric (see Kappeler & Pereira Transcription Key for more details). 
## not removing non-number characters will result in forced NA for that value when converting to type numeric, so it must be corrected for
kp <- kp %>% 
  mutate(
    BM.F = readr::parse_number(as.character(BM.F)),
    BM.F = as.numeric(BM.F)
  )
sources = as.factor(c("Amniotes", "AnAge", "Ernest", "PanTHERIA", "Kappeler & Pereira"))

## selecting vars for cross-database comparison
amniote_s <- amniote %>% 
  transmute(
    species = paste(genus, species),
    body_mass_u = adult_body_mass_g,
    body_mass_f = female_body_mass_g,
    body_mass_m = male_body_mass_g,
    AFR = NA,
    gest_len = gestation_d,
    IBI = inter_litter_or_interbirth_interval_y,
    max_longevity = maximum_longevity_y,
    age_at_maturity_u = NA,
    age_at_maturity_f = female_maturity_d,
    age_at_maturity_m = male_maturity_d,
    weaning_age = weaning_d,
    source = sources[1]
  )

AnAge_s <- AnAge %>% 
  transmute(
    species = paste(Genus, Species),
    body_mass_u = `Body mass (g)`,
    body_mass_f = NA,
    body_mass_m = NA,
    AFR = NA,
    gest_len = `Gestation/Incubation (days)`,
    IBI = `Inter-litter/Interbirth interval`,
    max_longevity = `Maximum longevity (yrs)`,
    age_at_maturity_u = NA,
    age_at_maturity_f = `Female maturity (days)`,
    age_at_maturity_m = `Male maturity (days)`,
    weaning_age = `Weaning (days)`,
    source = sources[2]
  )

#note: this version does not convert litters/year to IBI
ernest_s <- ernest %>% 
  transmute(
    species = paste(Genus, species),
    body_mass_u = NA,
    body_mass_f = `mass(g)`,
    body_mass_m = NA,
    AFR = `AFR(mo)`,
    gest_len = `gestation(mo)`,
    IBI = NA,
    max_longevity = `max. life(mo)`,
    age_at_maturity_u = NA,
    age_at_maturity_f = NA,
    age_at_maturity_m = NA,
    weaning_age = `weaning(mo)`,
    source = sources[3]
  )

pan_s <- pan %>% 
  transmute(
    species = paste(MSW05_Genus, MSW05_Species),
    body_mass_u = `5-1_AdultBodyMass_g`,
    body_mass_f = NA,
    body_mass_m = NA,
    AFR = `3-1_AgeatFirstBirth_d`,
    gest_len = `9-1_GestationLen_d`,
    IBI = `14-1_InterbirthInterval_d`,
    max_longevity = `17-1_MaxLongevity_m`,
    age_at_maturity_u = `23-1_SexualMaturityAge_d`,
    age_at_maturity_f = NA,
    age_at_maturity_m = NA,
    weaning_age = `25-1_WeaningAge_d`,
    source = sources[4]
  )

kp_s <- kp %>% 
  transmute(
    species = paste(Genus, Species),
    body_mass_u = BM.U,
    body_mass_f = BM.F,
    body_mass_m = NA,
    AFR = AFR,
    gest_len = GL,
    IBI = IBI,
    max_longevity = NA,
    age_at_maturity_u = NA,
    age_at_maturity_f = NA,
    age_at_maturity_m = NA,
    weaning_age = WA,
    source = sources[5]
  )