################################################################################
# 00_clean_data.R
#
# Fertility in the Knowledge Economy: Data Preparation
#
# PURPOSE: Load raw IPUMS USA extract, construct analysis samples, and save
#          as compressed .rds files for use in 01_analysis.R.
#
# INPUT:  usa_00004.dta  (IPUMS USA extract with IND1990)
#
# OUTPUT: data/df_wage_clean.rds       — employed workers, 21-65
#         data/df_fert_clean.rds       — women 15-44, ACS years
#         data/nchlt5_rates.rds        — children-under-5 rates by year/age/education
#         data/emp_rates_by_age.rds    — employment rates by year and age group
#
# NOTES:  Memory-constrained workflow. Each sample is saved and removed
#         before the next is constructed, so that df_raw and any single
#         analysis sample are never in memory simultaneously.
################################################################################

library(tidyverse)
library(haven)
library(beepr)

beep <- function(msg = "Done") {
  cat(paste0("\n>>> ", msg, " <<<\n"))
  beepr::beep(sound = 4)
}

raw_path <- "usa_00004.dta"
dir.create("data", showWarnings = FALSE)


# ── Step 1: Load and prepare raw data ────────────────────────────────────────

cat("Loading raw data...\n")
df_raw <- read_dta(raw_path)
names(df_raw) <- toupper(names(df_raw))
df_raw <- zap_labels(df_raw)
cat("Raw data loaded:", nrow(df_raw), "rows\n")
cat("Years:", paste(sort(unique(df_raw$YEAR)), collapse = ", "), "\n")
beep("Raw data loaded")


# ── Step 2: Wage sample ──────────────────────────────────────────────────────
# Employed workers aged 21-65 with positive wages.
# Variables are added in stages to avoid memory spikes from large mutate calls.

cat("\nBuilding wage sample...\n")

# 2a. Filter to relevant observations
df_wage <- df_raw %>%
  filter(AGE >= 21, AGE <= 65, EMPSTAT == 1, INCWAGE > 0, INCWAGE < 999998)
gc()

# 2b. Core wage and experience variables
df_wage <- df_wage %>%
  mutate(
    wage    = if ("INCWAGE_CPIU_2010" %in% names(.)) INCWAGE_CPIU_2010 else INCWAGE,
    lwage   = log(wage),
    potexp  = pmax(AGE - 22, 0),
    potexp2 = potexp^2,
    female    = as.integer(SEX == 2),
    sex_label = ifelse(SEX == 2, "Female", "Male"),
    decade     = floor(YEAR / 10) * 10,
    decade_fct = as.factor(decade),
    state_fct  = as.factor(STATEFIP)
  )
gc()

# 2c. Education groups
df_wage <- df_wage %>%
  mutate(
    educ_group = case_when(
      EDUC <= 6      ~ "HS or less",
      EDUC %in% 7:9  ~ "Some college",
      EDUC >= 10     ~ "Bachelor+",
      TRUE           ~ NA_character_
    ),
    educ_group = factor(educ_group, levels = c("HS or less", "Some college", "Bachelor+"))
  )
gc()

# 2d. Race/ethnicity
df_wage <- df_wage %>%
  mutate(
    race_eth = case_when(
      HISPAN > 0 & HISPAN < 9 ~ "Hispanic",
      RACE == 1                ~ "White",
      RACE == 2                ~ "Black",
      TRUE                     ~ "Other"
    )
  )
gc()

# 2e. Industry sector (IND1990 harmonized codes, consistent across all years)
df_wage <- df_wage %>%
  mutate(
    sector = case_when(
      IND1990 >= 700 & IND1990 <= 712 ~ "Knowledge",   # Finance, insurance, RE
      IND1990 >= 440 & IND1990 <= 442 ~ "Knowledge",   # Communications
      IND1990 == 732                   ~ "Knowledge",   # Computer/data processing
      IND1990 >= 812 & IND1990 <= 840 ~ "Knowledge",   # Health services
      IND1990 >= 842 & IND1990 <= 860 ~ "Knowledge",   # Educational services
      IND1990 >= 872 & IND1990 <= 893 ~ "Knowledge",   # Professional services
      IND1990 >= 100 & IND1990 <= 392 ~ "Routine",     # Manufacturing
      IND1990 == 60                    ~ "Routine",     # Construction
      IND1990 >= 580 & IND1990 <= 691 ~ "Routine",     # Retail trade
      IND1990 >= 400 & IND1990 <= 432 ~ "Routine",     # Transportation
      IND1990 >= 10  & IND1990 <= 50  ~ "Routine",     # Agriculture, mining
      TRUE                             ~ "Other"
    ),
    sector = factor(sector, levels = c("Routine", "Other", "Knowledge"))
  )
gc()

# 2f. Occupation task type (Census 2010 OCC codes, consistent 2005+)
df_wage <- df_wage %>%
  mutate(
    occ_task = case_when(
      YEAR < 2005                ~ NA_character_,
      OCC >= 10   & OCC <= 430   ~ "Cognitive",   # Management
      OCC >= 500  & OCC <= 950   ~ "Cognitive",   # Business, financial
      OCC >= 1000 & OCC <= 1240  ~ "Cognitive",   # Computer, mathematical
      OCC >= 1300 & OCC <= 1560  ~ "Cognitive",   # Architecture, engineering
      OCC >= 1600 & OCC <= 1980  ~ "Cognitive",   # Sciences
      OCC >= 2100 & OCC <= 2160  ~ "Cognitive",   # Legal
      OCC >= 2200 & OCC <= 2550  ~ "Cognitive",   # Education, library
      OCC >= 3000 & OCC <= 3540  ~ "Cognitive",   # Healthcare practitioners
      OCC >= 4000 & OCC <= 4150  ~ "Routine",     # Food preparation
      OCC >= 4200 & OCC <= 4250  ~ "Routine",     # Cleaning, maintenance
      OCC >= 6005 & OCC <= 6130  ~ "Routine",     # Farming, fishing
      OCC >= 6200 & OCC <= 6940  ~ "Routine",     # Construction, extraction
      OCC >= 7000 & OCC <= 7630  ~ "Routine",     # Installation, repair
      OCC >= 7700 & OCC <= 8965  ~ "Routine",     # Production
      OCC >= 9000 & OCC <= 9750  ~ "Routine",     # Transportation
      TRUE                       ~ "Mixed"         # Sales, admin, personal care
    ),
    occ_task = factor(occ_task, levels = c("Routine", "Mixed", "Cognitive"))
  )

cat("Wage sample:", nrow(df_wage), "rows\n")
saveRDS(df_wage, "data/df_wage_clean.rds")
rm(df_wage); gc()
beep("Wage sample saved")


# ── Step 3: Fertility sample ─────────────────────────────────────────────────
# Women aged 15-44 in ACS years who are in the FERTYR universe.

cat("\nBuilding fertility sample...\n")

df_fert <- df_raw %>%
  filter(SEX == 2, AGE >= 15, AGE <= 44, YEAR >= 2001, FERTYR %in% c(1, 2))
gc()

df_fert <- df_fert %>%
  mutate(
    had_birth = as.integer(FERTYR == 2),
    age_group = cut(AGE,
                    breaks = c(14, 19, 24, 29, 34, 39, 44),
                    labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")),
    state_fct = as.factor(STATEFIP)
  )
gc()

df_fert <- df_fert %>%
  mutate(
    educ_group = case_when(
      EDUC <= 6      ~ "HS or less",
      EDUC %in% 7:9  ~ "Some college",
      EDUC >= 10     ~ "Bachelor+",
      TRUE           ~ NA_character_
    ),
    educ_group = factor(educ_group, levels = c("HS or less", "Some college", "Bachelor+"))
  )

cat("Fertility sample:", nrow(df_fert), "rows\n")
saveRDS(df_fert, "data/df_fert_clean.rds")
rm(df_fert); gc()
beep("Fertility sample saved")


# ── Step 4: NCHLT5 rates ────────────────────────────────────────────────────
# Share of women with at least one child under 5, by year/age/education.
# Uses all women (including non-employed), available in all census years.

cat("\nBuilding NCHLT5 rates...\n")

nchlt5_data <- df_raw %>%
  filter(SEX == 2, AGE >= 21, AGE <= 44)
gc()

nchlt5_data <- nchlt5_data %>%
  mutate(
    has_young_child = as.integer(NCHLT5 > 0),
    age_group = cut(AGE,
                    breaks = c(20, 24, 29, 34, 39, 44),
                    labels = c("21-24", "25-29", "30-34", "35-39", "40-44")),
    educ_group = case_when(
      EDUC <= 6      ~ "HS or less",
      EDUC %in% 7:9  ~ "Some college",
      EDUC >= 10     ~ "Bachelor+",
      TRUE           ~ NA_character_
    ),
    educ_group = factor(educ_group, levels = c("HS or less", "Some college", "Bachelor+"))
  )
gc()

nchlt5_rates <- nchlt5_data %>%
  filter(!is.na(age_group), !is.na(educ_group)) %>%
  group_by(YEAR, age_group, educ_group) %>%
  summarise(
    share_young_child = weighted.mean(has_young_child, PERWT, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

saveRDS(nchlt5_rates, "data/nchlt5_rates.rds")
rm(nchlt5_data, nchlt5_rates); gc()
beep("NCHLT5 rates saved")


# ── Step 5: Employment rates by age group ────────────────────────────────────
# For the precariousness analysis. Includes non-employed individuals.

cat("\nBuilding employment rates...\n")

# Select only needed columns to save memory
emp_data <- df_raw %>%
  select(YEAR, AGE, EMPSTAT, PERWT) %>%
  filter(AGE >= 21, AGE <= 55)
gc()

emp_rates <- emp_data %>%
  mutate(
    age_group = case_when(
      AGE <= 25 ~ "Young (21-25)",
      AGE <= 34 ~ "Early career (26-34)",
      AGE <= 45 ~ "Mid career (35-45)",
      TRUE      ~ "Late career (46-55)"
    ),
    employed = as.integer(EMPSTAT == 1)
  ) %>%
  group_by(YEAR, age_group) %>%
  summarise(
    emp_rate = weighted.mean(employed, PERWT, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(emp_rates, "data/emp_rates_by_age.rds")
rm(emp_data, emp_rates); gc()
beep("Employment rates saved")


# ── Cleanup ──────────────────────────────────────────────────────────────────

rm(df_raw); gc()
beep("ALL CLEANING DONE — close R and run 01_analysis.R")