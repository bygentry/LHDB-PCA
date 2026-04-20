# Setup script for Database-Analysis.qmd
options(repos = "https://cloud.r-project.org")

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