options(repos = "https://cran.us.r-project.org")

# loads necessary packages
pacman::p_load(
  psych, tidyverse, curl, mice, rlang, yaml, gridExtra, lme4, XML, lubridate, gratia, VIM
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
missing.val.to.na <- function(df){
  df |> mutate(across(where(is.numeric), ~ na_if(.x, -999)))
}

# creates subset dfs for primate rows and removes non mammal variables
amniote_p <- amniote %>% dplyr::filter(order == "Primates")
amniote_p <- missing.val.to.na(amniote_p)

AnAge_p <- AnAge %>% dplyr::filter(Order == "Primates")

ernest_p <- ernest %>% dplyr::filter(order == "Primates")
ernest_p <- missing.val.to.na(ernest_p)

pan_p <- pan %>% dplyr::filter(MSW05_Order == "Primates")
pan_p <- missing.val.to.na(pan_p)

# Kappeler & Pereira is already only primates, no filter necessary
## Point Correction: one value of BM.F has a "?" in it, making the object type character instead of numeric (see Kappeler & Pereira Transcription Key for more details). 
## not removing non-number characters will result in forced NA for that value when converting to type numeric, so it must be corrected for
kp <- kp %>% 
  mutate(
    BM.F = readr::parse_number(as.character(BM.F)),
    BM.F = as.numeric(BM.F)
  )