library(DoubleML)
library(mlr3extralearners)
library(tidyverse)

source("Code/Estimator Functions.R")

# Load Data
df_bwt <- read.csv(
  "Data/Empirical Application/twin_pairs_T_3years_samesex.csv"
  )

df_co_var <- read.csv(
  "Data/Empirical Application/twin_pairs_X_3years_samesex.csv"
  )

df_mort <- read.csv(
  "Data/Empirical Application/twin_pairs_Y_3years_samesex.csv"
  )

# Convert data formats
df_bwt <- df_bwt %>% 
  dplyr::select(-1) %>% 
  set_names(c("bwt_1", "bwt_2")) %>% 
  mutate(diff_bwt = bwt_1 - bwt_2)

df_mort <- df_mort %>% 
  dplyr::select(-1) %>% 
  mutate(
    dep_mort = case_when(
      mort_0 == 0 & mort_1 == 0 ~ "both alive",
      xor(mort_0 == 1, mort_1 == 1) ~ "one dead",
      mort_0 == 1 & mort_1 == 1 ~ "both dead",
      TRUE ~ NA_character_
    ),
    mort_0 = factor(mort_0, labels = c("no", "yes")),
    mort_1 = factor(mort_1, labels = c("no", "yes"))
  ) %>% 
  set_names(c("dead_1", "dead_2", "mort_total"))

df_cov_var <- df_co_var %>% 
  dplyr::select(-c(1:2)) %>% 
  dplyr::select(sort(current_vars()))

# Recode a lot of variables
vec_race <- c("white", "black", "native", "chinese", "japanese", "hawaiian", 
              "filipino", "other asian or pacific", "all other")

vec_gestat <- c("< 20", "20 - 27", "28 - 31", "32 - 35", "36", "37 - 39", "40", 
                "41", "42", "> 42")

vec_educ <- c("0 - 8", "9 - 11", "12", "13 - 15", "> 15")

vec_states <- c(state.abb[1:8], "DoC", state.abb[9:50])
vec_pl_bt <- c(vec_states, "PR", "VI", "Guam", "CAN", "CU", "MEX", "Rest")

df_cov_var <- df_cov_var %>% 
  mutate(
    adequacy = factor(adequacy, labels = c("adequate", "intermediate", "inadequate")),
    alcohol = factor(alcohol, labels = c("no", "yes")),
    anemia = factor(anemia, labels = c("no", "yes")),
    birattnd = factor(birattnd, labels = c("m.d.", "d.o.", "c.n.m.", "other midwife", "other")),
    birmon = factor(birmon, labels = c("jan.", "feb.", "mar.", "apr.", "may", "june", "juli", "aug.", "sep.", "oct.", "nov.", "dec.")),
    bord_0 = factor(bord_0, labels = c("first", "second")),
    bord_1 = factor(bord_1, labels = c("first", "second")),
    brstate = factor(brstate),
    cardiac = factor(cardiac, labels = c("no", "yes")),
    chyper = factor(chyper, labels = c("no", "yes")),
    crace = factor(crace, labels = vec_race),
    csex = factor(csex, labels = c("male", "female")),
    dfageq = as.integer(dfageq),
    diabets = factor(diabetes, labels = c("no", "yes")),
    dlivord_min = as.integer(dlivord_min),
    dmar = factor(dmar, labels = c("no", "yes")),
    drink5 = as.integer(drink5),
    dtotord_min = as.integer(dtotord_min),
    eclamp = factor(eclamp, labels = c("no", "yes")),
    feduc6 = factor(feduc6, labels = vec_educ),
    frace = factor(frace, labels = vec_race),
    gestat10 = factor(gestat10, labels = vec_gestat),
    hemo = factor(hemo, labels = c("no", "yes")),
    herpes = factor(herpes, labels = c("no", "yes")),
    hydra = factor(hydra, labels = c("no", "yes")),
    incervix = factor(incervix, labels = c("no", "yes")),
    lung = factor(lung, labels = c("no", "yes")),
    mager8 = as.integer(mager8),
    meduc6 = factor(meduc6, labels = vec_educ),
    mplbir = factor(mplbir, labels = vec_pl_bt),
    mplbir_reg = factor(mplbir_reg),
    mpre5 = factor(mpre5, labels = paste0(1:4, "_trimester")),
    mrace = factor(mrace, labels = vec_race),
    orfath = factor(orfath, labels = c("no", "mex", "pr", "cu", "csa", "other_hisp")),
    ormoth = factor(ormoth, labels = c("no", "mex", "pr", "cu", "csa", "other_hisp")),
    othermr = factor(othermr, labels = c("no", "yes")),
    phyper = factor(phyper, labels = c("no", "yes")),
    pldel = factor(pldel, labels = c("hospital", "freestanding birth center", "clinic or doctors", "a residence", "other")),
    pre4000 = factor(pre4000, labels = c("no", "yes")),
    preterm = factor(preterm, labels = c("no", "yes")),
    renal = factor(renal, labels = c("no", "yes")),
    rh = factor(rh, labels = c("no", "yes")),
    stoccfipb = factor(stoccfipb),
    stoccfipb_reg = factor(stoccfipb_reg),
    tobacco = factor(tobacco, labels = c("no", "yes")),
    uterine = factor(uterine, labels = c("no", "yes"))
  )

vec_X_cols <- colnames()