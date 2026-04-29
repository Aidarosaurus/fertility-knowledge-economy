################################################################################
# 01_analysis.R
#
# Fertility in the Knowledge Economy: Empirical Analysis
#
# PURPOSE: Estimate wage-experience profiles, test structural breaks,
#          analyze fertility timing by education, and document youth
#          employment precariousness. Produces all figures and tables
#          for the paper.
#
# PREREQUISITE: Run 00_clean_data.R to generate .rds files in data/.
#
# INPUT:  data/df_wage_clean.rds
#         data/df_fert_clean.rds
#         data/nchlt5_rates.rds
#         data/emp_rates_by_age.rds
#         bls_cpi.csv                 (BLS CPI data, manually downloaded)
#
# OUTPUT: figures/*.png               (all plots)
#         results/*.txt               (regression output and coefficient tables)
################################################################################


# ══════════════════════════════════════════════════════════════════════════════
# 0. Setup
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fixest)
library(beepr)

dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

beep <- function(msg = "Done") {
  cat(paste0("\n>>> ", msg, " <<<\n"))
  beepr::beep(sound = 4)
}

# Load cleaned data
df_wage      <- readRDS("data/df_wage_clean.rds")
df_fert      <- readRDS("data/df_fert_clean.rds")
nchlt5_rates <- readRDS("data/nchlt5_rates.rds")
emp_rates    <- readRDS("data/emp_rates_by_age.rds")

cat("Wage sample:", nrow(df_wage), "obs\n")
cat("Fertility sample:", nrow(df_fert), "obs\n")

# Publication theme
theme_paper <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    plot.title       = element_text(face = "bold", size = 14),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold"),
    panel.border     = element_rect(color = "grey50", fill = NA, linewidth = 0.5)
  )
theme_set(theme_paper)

# Colorblind-safe palettes with distinct linetypes and shapes for B&W printing
pal2_color <- c("#D55E00", "#0072B2")
pal2_lty   <- c("solid", "dashed")
pal2_shape <- c(16, 17)

pal3_color <- c("#D55E00", "#0072B2", "#009E73")
pal3_lty   <- c("solid", "dashed", "dotted")
pal3_shape <- c(16, 17, 15)

pal4_color <- c("#000000", "#D55E00", "#0072B2", "#009E73")
pal4_lty   <- c("solid", "longdash", "dashed", "dotted")
pal4_shape <- c(16, 17, 15, 18)

pal6_color <- c("#000000", "#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00")
pal6_lty   <- c("solid", "longdash", "dashed", "dotdash", "dotted", "twodash")
pal6_shape <- c(16, 17, 15, 18, 8, 4)

beep("Data loaded")


# ══════════════════════════════════════════════════════════════════════════════
# 1. Year-by-Year Mincer Regressions
# ══════════════════════════════════════════════════════════════════════════════

# --- 1a. No controls: ln(w) = b0 + b1*X + b2*X^2 ---

run_mincer_by_year <- function(data, sex_filter) {
  d <- data %>% filter(sex_label == sex_filter)
  map_dfr(sort(unique(d$YEAR)), function(y) {
    dy <- d %>% filter(YEAR == y)
    m <- lm(lwage ~ potexp + potexp2, data = dy, weights = PERWT)
    s <- summary(m)
    tibble(year = y,
           intercept = coef(m)[1], beta_exp = coef(m)[2], beta_exp2 = coef(m)[3],
           se_intercept = s$coefficients[1,2], se_exp = s$coefficients[2,2],
           se_exp2 = s$coefficients[3,2], n = nrow(dy), r2 = s$r.squared)
  })
}

mincer_female_noctl <- run_mincer_by_year(df_wage, "Female")
mincer_male_noctl   <- run_mincer_by_year(df_wage, "Male")
beep("Mincer no-controls done")

# --- 1b. With controls: + education, race/ethnicity, marital status ---

run_mincer_by_year_ctl <- function(data, sex_filter) {
  d <- data %>% filter(sex_label == sex_filter)
  map_dfr(sort(unique(d$YEAR)), function(y) {
    dy <- d %>% filter(YEAR == y)
    m <- lm(lwage ~ potexp + potexp2 + educ_group + race_eth + factor(MARST),
            data = dy, weights = PERWT)
    s <- summary(m)
    tibble(year = y, intercept = coef(m)[1],
           beta_exp = coef(m)["potexp"], beta_exp2 = coef(m)["potexp2"],
           se_exp = s$coefficients["potexp",2], se_exp2 = s$coefficients["potexp2",2],
           n = nrow(dy), r2 = s$r.squared)
  })
}

mincer_female_ctl <- run_mincer_by_year_ctl(df_wage, "Female")
mincer_male_ctl   <- run_mincer_by_year_ctl(df_wage, "Male")
beep("Mincer with-controls done")

# --- 1c. By education group ---

run_mincer_by_year_educ <- function(data, sex_filter) {
  d <- data %>% filter(sex_label == sex_filter, !is.na(educ_group))
  combos <- d %>% distinct(YEAR, educ_group) %>% arrange(YEAR, educ_group)
  map2_dfr(combos$YEAR, combos$educ_group, function(y, eg) {
    dy <- d %>% filter(YEAR == y, educ_group == eg)
    if (nrow(dy) < 50) return(NULL)
    m <- lm(lwage ~ potexp + potexp2, data = dy, weights = PERWT)
    s <- summary(m)
    tibble(year = y, educ_group = eg,
           beta_exp = coef(m)["potexp"], beta_exp2 = coef(m)["potexp2"],
           se_exp = s$coefficients["potexp",2], se_exp2 = s$coefficients["potexp2",2],
           n = nrow(dy))
  })
}

mincer_female_educ <- run_mincer_by_year_educ(df_wage, "Female")
mincer_male_educ   <- run_mincer_by_year_educ(df_wage, "Male")
beep("Mincer by-education done")

# --- Figures ---

plot_beta <- function(noctl, ctl, title_prefix) {
  bind_rows(noctl %>% mutate(spec = "No controls"),
            ctl   %>% mutate(spec = "With controls")) %>%
    mutate(ci_lo = beta_exp - 1.96*se_exp, ci_hi = beta_exp + 1.96*se_exp) %>%
    ggplot(aes(x = year, y = beta_exp, color = spec, linetype = spec,
               shape = spec, fill = spec)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    labs(title = paste(title_prefix, "— Return to Experience Over Time"),
         x = "Year", y = expression(beta[1]),
         color = NULL, linetype = NULL, shape = NULL, fill = NULL) +
    scale_color_manual(values = pal2_color) + scale_fill_manual(values = pal2_color) +
    scale_linetype_manual(values = pal2_lty) + scale_shape_manual(values = pal2_shape)
}

plot_beta_educ <- function(data, title_prefix) {
  data %>%
    mutate(ci_lo = beta_exp - 1.96*se_exp, ci_hi = beta_exp + 1.96*se_exp) %>%
    ggplot(aes(x = year, y = beta_exp, color = educ_group, linetype = educ_group,
               shape = educ_group, fill = educ_group)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.1, color = NA) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    labs(title = paste(title_prefix, "— Return to Experience by Education"),
         x = "Year", y = expression(beta[1]),
         color = NULL, linetype = NULL, shape = NULL, fill = NULL) +
    scale_color_manual(values = pal3_color) + scale_fill_manual(values = pal3_color) +
    scale_linetype_manual(values = pal3_lty) + scale_shape_manual(values = pal3_shape)
}

ggsave("figures/fig_beta1_female.png",
       plot_beta(mincer_female_noctl, mincer_female_ctl, "Female"), width=10, height=6)
ggsave("figures/fig_beta1_male.png",
       plot_beta(mincer_male_noctl, mincer_male_ctl, "Male"), width=10, height=6)
ggsave("figures/fig_beta1_female_educ.png",
       plot_beta_educ(mincer_female_educ, "Female"), width=10, height=6)
ggsave("figures/fig_beta1_male_educ.png",
       plot_beta_educ(mincer_male_educ, "Male"), width=10, height=6)
beep("Section 1 figures saved")


# ══════════════════════════════════════════════════════════════════════════════
# 2. Pooled Regression with Decade Interactions
# ══════════════════════════════════════════════════════════════════════════════
# Tests whether the change in the experience coefficient across decades is
# statistically significant. Base decade: 1980. Uses a 20% subsample for
# computational feasibility on memory-constrained machines.

run_pooled_mincer <- function(data, sex_filter, sample_frac = 0.2) {
  set.seed(42)
  d <- data %>%
    filter(sex_label == sex_filter, YEAR >= 1940) %>%
    mutate(decade_fct = relevel(decade_fct, ref = "1980")) %>%
    slice_sample(prop = sample_frac)
  cat("  Pooled sample:", nrow(d), "obs\n")
  
  m1 <- feols(lwage ~ potexp * decade_fct + potexp2 * decade_fct,
              data = d, weights = ~PERWT, vcov = ~state_fct)
  m2 <- feols(lwage ~ potexp * decade_fct + potexp2 * decade_fct +
                educ_group + race_eth + i(MARST),
              data = d, weights = ~PERWT, vcov = ~state_fct)
  m3 <- feols(lwage ~ potexp * decade_fct + potexp2 * decade_fct +
                educ_group + race_eth + i(MARST) | state_fct,
              data = d, weights = ~PERWT, vcov = ~state_fct)
  list(no_controls = m1, with_controls = m2, with_state_fe = m3)
}

cat("Running pooled regressions...\n")
pooled_female <- run_pooled_mincer(df_wage, "Female")
beep("Pooled female done")
pooled_male <- run_pooled_mincer(df_wage, "Male")
beep("Pooled male done")


# ══════════════════════════════════════════════════════════════════════════════
# 3. Structural Break Test
# ══════════════════════════════════════════════════════════════════════════════
# Piecewise linear regression on the b1 time series, testing whether the
# slope changes at 2010. Weighted by inverse standard error.

run_break_test <- function(coef_data, break_year = 2010) {
  d <- coef_data %>% arrange(year) %>% filter(!is.na(beta_exp)) %>%
    mutate(post = as.integer(year >= break_year),
           year_centered = year - break_year)
  m_r <- lm(beta_exp ~ year_centered, data = d, weights = 1/se_exp^2)
  m_u <- lm(beta_exp ~ year_centered * post, data = d, weights = 1/se_exp^2)
  f   <- anova(m_r, m_u)
  list(restricted = summary(m_r), unrestricted = summary(m_u), f_test = f,
       break_year = break_year,
       pre_slope  = coef(m_u)["year_centered"],
       post_slope = coef(m_u)["year_centered"] + coef(m_u)["year_centered:post"],
       n = nrow(d))
}

break_female     <- run_break_test(mincer_female_noctl)
break_female_ctl <- run_break_test(mincer_female_ctl)
break_male       <- run_break_test(mincer_male_noctl)
beep("Structural break tests done")


# ══════════════════════════════════════════════════════════════════════════════
# 4. Fertility Analysis
# ══════════════════════════════════════════════════════════════════════════════

# --- 4a. NCHLT5: children under 5 by education and age (1940-2024) ---

p_nchlt5 <- nchlt5_rates %>%
  filter(age_group %in% c("21-24", "25-29", "30-34", "35-39")) %>%
  ggplot(aes(x = YEAR, y = share_young_child, color = age_group,
             linetype = age_group, shape = age_group)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ educ_group, nrow = 1) +
  labs(title = "Share of Women with Children Under 5 by Education and Age",
       x = "Year", y = "Share with child under 5",
       color = NULL, linetype = NULL, shape = NULL) +
  scale_color_manual(values = pal4_color) + scale_linetype_manual(values = pal4_lty) +
  scale_shape_manual(values = pal4_shape)
ggsave("figures/fig_nchlt5_v2.png", p_nchlt5, width = 14, height = 5)

# --- 4b. Age-specific fertility rates (ACS, FERTYR) ---

asfr_aggregate <- df_fert %>%
  filter(!is.na(had_birth), !is.na(age_group)) %>%
  group_by(YEAR, age_group) %>%
  summarise(asfr = sum(had_birth * PERWT) / sum(PERWT) * 1000, .groups = "drop")

p_asfr <- asfr_aggregate %>%
  ggplot(aes(x = YEAR, y = asfr, color = age_group, linetype = age_group,
             shape = age_group)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Age-Specific Fertility Rates (U.S., ACS)",
       x = "Year", y = "Births per 1,000 women",
       color = NULL, linetype = NULL, shape = NULL) +
  scale_color_manual(values = pal6_color) +
  scale_linetype_manual(values = pal6_lty) +
  scale_shape_manual(values = pal6_shape)
ggsave("figures/fig_asfr_aggregate.png", p_asfr, width = 10, height = 6)

# --- 4c. ASFR by education ---

asfr_educ <- df_fert %>%
  filter(!is.na(had_birth), !is.na(age_group), !is.na(educ_group)) %>%
  group_by(YEAR, age_group, educ_group) %>%
  summarise(asfr = sum(had_birth * PERWT) / sum(PERWT) * 1000, .groups = "drop")

p_asfr_educ <- asfr_educ %>%
  ggplot(aes(x = YEAR, y = asfr, color = age_group, linetype = age_group,
             shape = age_group)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  facet_wrap(~ educ_group, nrow = 1) +
  labs(title = "Age-Specific Fertility Rates by Education",
       x = "Year", y = "Births per 1,000 women",
       color = NULL, linetype = NULL, shape = NULL) +
  scale_color_manual(values = pal6_color) +
  scale_linetype_manual(values = pal6_lty) +
  scale_shape_manual(values = pal6_shape)
ggsave("figures/fig_asfr_by_education.png", p_asfr_educ, width = 14, height = 5)

# --- 4d. Mean age at birth by education ---

mean_age_birth <- df_fert %>%
  filter(had_birth == 1) %>%
  group_by(YEAR) %>%
  summarise(mean_age = weighted.mean(AGE, PERWT, na.rm = TRUE), .groups = "drop")

mean_age_birth_educ <- df_fert %>%
  filter(had_birth == 1, !is.na(educ_group)) %>%
  group_by(YEAR, educ_group) %>%
  summarise(mean_age = weighted.mean(AGE, PERWT, na.rm = TRUE),
            n = n(), .groups = "drop")

p_mean_age <- mean_age_birth_educ %>%
  ggplot(aes(x = YEAR, y = mean_age, color = educ_group, linetype = educ_group,
             shape = educ_group)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  labs(title = "Mean Age at Birth by Education",
       x = "Year", y = "Mean age of mother",
       color = NULL, linetype = NULL, shape = NULL) +
  scale_color_manual(values = pal3_color) +
  scale_linetype_manual(values = pal3_lty) +
  scale_shape_manual(values = pal3_shape)
ggsave("figures/fig_mean_age_birth.png", p_mean_age, width = 10, height = 6)

rm(df_fert); gc()
beep("Fertility analysis done")


# ══════════════════════════════════════════════════════════════════════════════
# 5. Experience Premium and Fertility: Direct Test
# ══════════════════════════════════════════════════════════════════════════════
# Scatterplot and regression of NCHLT5 (25-29) on beta_1 across
# year x education cells.

fert_25_29 <- nchlt5_rates %>%
  filter(age_group == "25-29") %>%
  select(YEAR, educ_group, share_young_child)

beta_by_educ <- mincer_female_educ %>%
  select(YEAR = year, educ_group, beta_exp)

scatter_data <- beta_by_educ %>%
  inner_join(fert_25_29, by = c("YEAR", "educ_group")) %>%
  filter(!is.na(beta_exp), !is.na(share_young_child))

p_scatter <- scatter_data %>%
  ggplot(aes(x = beta_exp, y = share_young_child,
             color = educ_group, shape = educ_group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  labs(title = "Experience Premium vs. Share of 25-29 Year Olds with Young Children",
       x = expression(paste("Experience coefficient (", beta[1], ")")),
       y = "Share with child under 5 (ages 25-29)",
       color = NULL, shape = NULL) +
  scale_color_manual(values = pal3_color) +
  scale_shape_manual(values = pal3_shape)
ggsave("figures/fig_scatter_beta_fertility.png", p_scatter, width = 10, height = 6)

scatter_reg_pooled <- lm(share_young_child ~ beta_exp, data = scatter_data)
scatter_reg_educ   <- lm(share_young_child ~ beta_exp + factor(educ_group),
                         data = scatter_data)
beep("Scatter test done")


# ══════════════════════════════════════════════════════════════════════════════
# 6. Sector and Occupation Analysis
# ══════════════════════════════════════════════════════════════════════════════

# --- 6a. Beta by group (generic function) ---

run_mincer_by_group <- function(data, sex_filter, group_var, min_year = 1950) {
  data %>%
    filter(sex_label == sex_filter, !is.na(!!sym(group_var)), YEAR >= min_year) %>%
    group_by(YEAR, !!sym(group_var)) %>%
    group_modify(~ {
      if (nrow(.x) < 100) return(tibble())
      m <- lm(lwage ~ potexp + potexp2, data = .x, weights = PERWT)
      s <- summary(m)
      tibble(beta_exp = coef(m)["potexp"], beta_exp2 = coef(m)["potexp2"],
             se_exp = s$coefficients["potexp",2], n = nrow(.x))
    }) %>% ungroup()
}

mincer_by_sector      <- run_mincer_by_group(df_wage, "Female", "sector")
mincer_by_sector_male <- run_mincer_by_group(df_wage, "Male", "sector")
mincer_by_occ_female  <- run_mincer_by_group(df_wage, "Female", "occ_task", 2005)
mincer_by_occ_male    <- run_mincer_by_group(df_wage, "Male", "occ_task", 2005)
beep("Sector/occupation betas done")

# --- 6b. Plotting ---

plot_group_beta <- function(data, group_var, title) {
  data %>%
    mutate(ci_lo = beta_exp - 1.96*se_exp, ci_hi = beta_exp + 1.96*se_exp) %>%
    ggplot(aes(x = YEAR, y = beta_exp, color = !!sym(group_var),
               linetype = !!sym(group_var), shape = !!sym(group_var),
               fill = !!sym(group_var))) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.1, color = NA) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    labs(title = title, x = "Year", y = expression(beta[1]),
         color = NULL, linetype = NULL, shape = NULL, fill = NULL) +
    scale_color_manual(values = pal3_color) + scale_fill_manual(values = pal3_color) +
    scale_linetype_manual(values = pal3_lty) + scale_shape_manual(values = pal3_shape)
}

ggsave("figures/fig_beta1_by_sector.png",
       plot_group_beta(mincer_by_sector, "sector",
                       "Return to Experience by Industry Sector (Female)"), width=10, height=6)
ggsave("figures/fig_beta1_by_sector_male.png",
       plot_group_beta(mincer_by_sector_male, "sector",
                       "Return to Experience by Industry Sector (Male)"), width=10, height=6)
ggsave("figures/fig_beta1_by_occ_female.png",
       plot_group_beta(mincer_by_occ_female, "occ_task",
                       "Return to Experience by Occupation Task (Female)"), width=10, height=6)
ggsave("figures/fig_beta1_by_occ_male.png",
       plot_group_beta(mincer_by_occ_male, "occ_task",
                       "Return to Experience by Occupation Task (Male)"), width=10, height=6)

# --- 6c. Employment shares ---

sector_shares <- df_wage %>%
  filter(!is.na(sector), YEAR >= 1950) %>%
  group_by(YEAR, sex_label, sector) %>%
  summarise(emp = sum(PERWT), .groups = "drop") %>%
  group_by(YEAR, sex_label) %>% mutate(share = emp/sum(emp)) %>% ungroup()

p_sector_shares <- sector_shares %>%
  ggplot(aes(x = YEAR, y = share, fill = sector)) +
  geom_area(alpha = 0.8) + facet_wrap(~ sex_label) +
  labs(title = "Employment Share by Industry Sector Over Time",
       x = "Year", y = "Share of employment", fill = NULL) +
  scale_fill_manual(values = c("Routine"="#D55E00","Other"="#E69F00","Knowledge"="#0072B2")) +
  scale_y_continuous(labels = scales::percent)
ggsave("figures/fig_sector_shares.png", p_sector_shares, width = 12, height = 5)

occ_shares <- df_wage %>%
  filter(!is.na(occ_task), YEAR >= 2005) %>%
  group_by(YEAR, sex_label, occ_task) %>%
  summarise(emp = sum(PERWT), .groups = "drop") %>%
  group_by(YEAR, sex_label) %>% mutate(share = emp/sum(emp)) %>% ungroup()

p_occ_shares <- occ_shares %>%
  ggplot(aes(x = YEAR, y = share, fill = occ_task)) +
  geom_area(alpha = 0.8) + facet_wrap(~ sex_label) +
  labs(title = "Employment Share by Occupation Task (2005+)",
       x = "Year", y = "Share of employment", fill = NULL) +
  scale_fill_manual(values = c("Routine"="#D55E00","Mixed"="#E69F00","Cognitive"="#0072B2")) +
  scale_y_continuous(labels = scales::percent)
ggsave("figures/fig_occ_shares.png", p_occ_shares, width = 12, height = 5)

# --- 6d. Within-between decomposition ---

decompose_wb <- function(sector_betas, sector_shares_data, sex) {
  shares <- sector_shares_data %>% filter(sex_label == sex) %>%
    select(YEAR, sector, share)
  combined <- sector_betas %>% inner_join(shares, by = c("YEAR", "sector"))
  years <- sort(unique(combined$YEAR))
  base <- combined %>% filter(YEAR == min(years))
  map_dfr(years[-1], function(y) {
    curr <- combined %>% filter(YEAR == y)
    m <- inner_join(base %>% select(sector, b0=beta_exp, s0=share),
                    curr %>% select(sector, b1=beta_exp, s1=share), by="sector")
    if (nrow(m)==0) return(NULL)
    tibble(year=y, base_year=min(years),
           within  = sum(m$s0*(m$b1-m$b0)),
           between = sum(m$b0*(m$s1-m$s0)),
           cross   = sum((m$b1-m$b0)*(m$s1-m$s0)),
           total   = within+between+cross)
  })
}

decomp_female <- decompose_wb(mincer_by_sector, sector_shares, "Female")
decomp_male   <- decompose_wb(mincer_by_sector_male, sector_shares, "Male")

plot_decomp <- function(data, title) {
  data %>%
    pivot_longer(c(within,between,total), names_to="component", values_to="value") %>%
    mutate(component = factor(component, levels=c("total","within","between"))) %>%
    ggplot(aes(x=year, y=value, color=component, linetype=component, shape=component)) +
    geom_line(linewidth=1) + geom_point(size=2.5) +
    geom_hline(yintercept=0, linetype="dashed", color="grey50") +
    labs(title=title, x="Year", y=expression(paste(Delta," aggregate ",beta[1])),
         color=NULL, linetype=NULL, shape=NULL) +
    scale_color_manual(values=pal3_color) +
    scale_linetype_manual(values=pal3_lty) +
    scale_shape_manual(values=pal3_shape)
}

ggsave("figures/fig_decomposition_female.png",
       plot_decomp(decomp_female, "Within-Between Decomposition (Female)"), width=10, height=6)
ggsave("figures/fig_decomposition_male.png",
       plot_decomp(decomp_male, "Within-Between Decomposition (Male)"), width=10, height=6)
beep("Sector/occupation analysis done")


# ══════════════════════════════════════════════════════════════════════════════
# 7. Wage-Experience Profile Plots (raw averages)
# ══════════════════════════════════════════════════════════════════════════════

plot_wage_profiles <- function(data, sex_filter, title) {
  d <- data %>% filter(sex_label == sex_filter)
  sel_years <- c(1940, 1960, 1980, 2000, 2010, 2020)
  sel_years <- sel_years[sel_years %in% unique(d$YEAR)]
  
  d %>%
    filter(YEAR %in% sel_years, potexp <= 40) %>%
    group_by(YEAR, potexp) %>%
    summarise(mean_wage = weighted.mean(wage, PERWT, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(year = as.factor(YEAR)) %>%
    ggplot(aes(x = potexp, y = mean_wage, color = year, linetype = year)) +
    geom_line(linewidth = 1) +
    labs(title = title, x = "Potential Experience (years)",
         y = "Wage (2010 $)", color = NULL, linetype = NULL) +
    scale_color_manual(values = pal6_color[1:length(sel_years)]) +
    scale_linetype_manual(values = pal6_lty[1:length(sel_years)])
}

ggsave("figures/fig_wage_profiles_female.png",
       plot_wage_profiles(df_wage, "Female", "Female Wage-Experience Profiles"),
       width = 10, height = 6)
ggsave("figures/fig_wage_profiles_male.png",
       plot_wage_profiles(df_wage, "Male", "Male Wage-Experience Profiles"),
       width = 10, height = 6)
beep("Wage profiles done")


# ══════════════════════════════════════════════════════════════════════════════
# 8. Youth Employment Precariousness
# ══════════════════════════════════════════════════════════════════════════════

p_emp <- emp_rates %>%
  ggplot(aes(x=YEAR, y=emp_rate, color=age_group, linetype=age_group, shape=age_group)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  geom_vline(xintercept=2008, linetype="dashed", color="grey50") +
  labs(title="Employment-to-Population Ratio by Age Group",
       x="Year", y="Employment rate", color=NULL, linetype=NULL, shape=NULL) +
  scale_color_manual(values=pal4_color) + scale_linetype_manual(values=pal4_lty) +
  scale_shape_manual(values=pal4_shape) +
  scale_y_continuous(labels=scales::percent)
ggsave("figures/fig_emp_rates_by_age.png", p_emp, width=10, height=6)

emp_gap <- emp_rates %>%
  filter(age_group %in% c("Young (21-25)","Mid career (35-45)")) %>%
  pivot_wider(names_from=age_group, values_from=emp_rate) %>%
  mutate(gap = `Young (21-25)` - `Mid career (35-45)`)

p_gap <- emp_gap %>%
  ggplot(aes(x=YEAR, y=gap)) +
  geom_line(linewidth=1, color="#D55E00") + geom_point(size=2.5, color="#D55E00") +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  geom_vline(xintercept=2008, linetype="dashed", color="grey50") +
  labs(title="Youth Employment Gap (21-25 minus 35-45)",
       x="Year", y="Employment rate gap") +
  scale_y_continuous(labels=scales::percent)
ggsave("figures/fig_youth_emp_gap.png", p_gap, width=10, height=6)
beep("Precariousness plots done")


# ══════════════════════════════════════════════════════════════════════════════
# 9. Baumol's Cost Disease
# ══════════════════════════════════════════════════════════════════════════════
# CPI data from BLS, manually downloaded. Columns: Year, SA0, SAM, SAE1, SAH1

baumol_raw <- read_csv("bls_cpi.csv")

baumol_long <- baumol_raw %>%
  pivot_longer(-Year, names_to="series", values_to="index") %>%
  filter(!is.na(index)) %>%
  mutate(category = case_when(
    series=="SA0" ~ "All items", series=="SAM" ~ "Medical care",
    series=="SAE1" ~ "Education", series=="SAH1" ~ "Shelter"),
    category = factor(category,
                      levels=c("Education","Medical care","Shelter","All items")))

baumol_norm <- baumol_long %>% filter(Year >= 1993) %>%
  group_by(category) %>% mutate(normalized = index/index[Year==1993]*100) %>% ungroup()

p_baumol <- baumol_norm %>%
  ggplot(aes(x=Year, y=normalized, color=category, linetype=category, shape=category)) +
  geom_line(linewidth=1.2) + geom_point(size=2) +
  labs(title="Child-Related Services vs. General Prices",
       subtitle="CPI indices normalized to 1993 = 100",
       x="Year", y="Price index (1993 = 100)",
       color=NULL, linetype=NULL, shape=NULL) +
  scale_color_manual(values=c("#D55E00","#0072B2","#009E73","grey50")) +
  scale_linetype_manual(values=c("solid","longdash","dotdash","dotted")) +
  scale_shape_manual(values=c(16,17,15,4))
ggsave("figures/fig_baumol_cpi.png", p_baumol, width=10, height=6)

baumol_ratio <- baumol_raw %>% filter(Year >= 1993) %>%
  mutate(`Education / CPI`=SAE1/SA0, `Medical care / CPI`=SAM/SA0,
         `Shelter / CPI`=SAH1/SA0) %>%
  select(Year, ends_with("/ CPI")) %>%
  pivot_longer(-Year, names_to="category", values_to="ratio") %>% filter(!is.na(ratio))

p_ratio <- baumol_ratio %>%
  ggplot(aes(x=Year, y=ratio, color=category, linetype=category, shape=category)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  labs(title="Relative Price of Child-Related Services",
       subtitle="Category CPI / All-Items CPI",
       x="Year", y="Relative price ratio",
       color=NULL, linetype=NULL, shape=NULL) +
  scale_color_manual(values=c("#D55E00","#0072B2","#009E73")) +
  scale_linetype_manual(values=c("solid","longdash","dotdash")) +
  scale_shape_manual(values=c(16,17,15))
ggsave("figures/fig_baumol_ratio.png", p_ratio, width=10, height=6)
beep("Baumol figures done")


# ══════════════════════════════════════════════════════════════════════════════
# 10. Summary Statistics
# ══════════════════════════════════════════════════════════════════════════════

summary_stats <- df_wage %>%
  group_by(sex_label, decade) %>%
  summarise(n=n(), mean_age=weighted.mean(AGE, PERWT),
            mean_wage=weighted.mean(wage, PERWT),
            mean_exp=weighted.mean(potexp, PERWT),
            share_ba=weighted.mean(EDUC>=10, PERWT), .groups="drop")

rm(df_wage); gc()


# ══════════════════════════════════════════════════════════════════════════════
# 11. Export All Results
# ══════════════════════════════════════════════════════════════════════════════

# --- Coefficient trajectories ---

sink("results/mincer_coefficients.txt")
cat("================================================================\n")
cat("YEAR-BY-YEAR MINCER COEFFICIENTS\n")
cat("================================================================\n\n")
cat("FEMALE — No controls:\n")
print(as.data.frame(mincer_female_noctl), row.names=FALSE)
cat("\nFEMALE — With controls:\n")
print(as.data.frame(mincer_female_ctl), row.names=FALSE)
cat("\nMALE — No controls:\n")
print(as.data.frame(mincer_male_noctl), row.names=FALSE)
cat("\nMALE — With controls:\n")
print(as.data.frame(mincer_male_ctl), row.names=FALSE)
cat("\n\n================================================================\n")
cat("BY EDUCATION GROUP\n")
cat("================================================================\n\n")
cat("FEMALE:\n"); print(as.data.frame(mincer_female_educ), row.names=FALSE)
cat("\nMALE:\n"); print(as.data.frame(mincer_male_educ), row.names=FALSE)
cat("\n\n================================================================\n")
cat("BY SECTOR\n")
cat("================================================================\n\n")
cat("FEMALE:\n"); print(as.data.frame(mincer_by_sector), row.names=FALSE)
cat("\nMALE:\n"); print(as.data.frame(mincer_by_sector_male), row.names=FALSE)
cat("\n\n================================================================\n")
cat("BY OCCUPATION TASK (2005+)\n")
cat("================================================================\n\n")
cat("FEMALE:\n"); print(as.data.frame(mincer_by_occ_female), row.names=FALSE)
cat("\nMALE:\n"); print(as.data.frame(mincer_by_occ_male), row.names=FALSE)
sink()

# --- Regressions ---

sink("results/regressions.txt")
cat("================================================================\n")
cat("POOLED MINCER — FEMALE\n")
cat("================================================================\n\n")
cat("(1) No controls:\n"); print(summary(pooled_female$no_controls))
cat("\n(2) With controls:\n"); print(summary(pooled_female$with_controls))
cat("\n(3) With controls + state FE:\n"); print(summary(pooled_female$with_state_fe))
cat("\n\n================================================================\n")
cat("POOLED MINCER — MALE\n")
cat("================================================================\n\n")
cat("(1) No controls:\n"); print(summary(pooled_male$no_controls))
cat("\n(2) With controls:\n"); print(summary(pooled_male$with_controls))
cat("\n(3) With controls + state FE:\n"); print(summary(pooled_male$with_state_fe))
cat("\n\n================================================================\n")
cat("STRUCTURAL BREAK TESTS\n")
cat("================================================================\n\n")
cat("Female, no controls:\n")
print(break_female$unrestricted)
cat("\nF-test:\n"); print(break_female$f_test)
cat("Pre-break slope:", break_female$pre_slope, "\n")
cat("Post-break slope:", break_female$post_slope, "\n")
cat("\nFemale, with controls — F-test:\n"); print(break_female_ctl$f_test)
cat("Pre-break slope:", break_female_ctl$pre_slope, "\n")
cat("Post-break slope:", break_female_ctl$post_slope, "\n")
cat("\nMale, no controls — F-test:\n"); print(break_male$f_test)
cat("Pre-break slope:", break_male$pre_slope, "\n")
cat("Post-break slope:", break_male$post_slope, "\n")
cat("\n\n================================================================\n")
cat("EXPERIENCE PREMIUM vs FERTILITY (SCATTER REGRESSION)\n")
cat("================================================================\n\n")
cat("Pooled:\n"); print(summary(scatter_reg_pooled))
cat("\nWith education FE:\n"); print(summary(scatter_reg_educ))
sink()

# --- Fertility and decomposition ---

sink("results/fertility_and_decomposition.txt")
cat("================================================================\n")
cat("AGGREGATE ASFR\n")
cat("================================================================\n\n")
print(as.data.frame(asfr_aggregate), row.names=FALSE)
cat("\n\n================================================================\n")
cat("ASFR BY EDUCATION\n")
cat("================================================================\n\n")
print(as.data.frame(asfr_educ), row.names=FALSE)
cat("\n\n================================================================\n")
cat("MEAN AGE AT BIRTH\n")
cat("================================================================\n\n")
cat("Aggregate:\n"); print(as.data.frame(mean_age_birth), row.names=FALSE)
cat("\nBy education:\n"); print(as.data.frame(mean_age_birth_educ), row.names=FALSE)
cat("\n\n================================================================\n")
cat("NCHLT5 RATES (sample)\n")
cat("================================================================\n\n")
print(as.data.frame(nchlt5_rates %>% filter(age_group=="25-29")), row.names=FALSE)
cat("\n\n================================================================\n")
cat("SECTOR SHARES\n")
cat("================================================================\n\n")
print(as.data.frame(sector_shares), row.names=FALSE)
cat("\n\n================================================================\n")
cat("OCCUPATION SHARES (2005+)\n")
cat("================================================================\n\n")
print(as.data.frame(occ_shares), row.names=FALSE)
cat("\n\n================================================================\n")
cat("WITHIN-BETWEEN DECOMPOSITION\n")
cat("================================================================\n\n")
cat("Female:\n"); print(as.data.frame(decomp_female), row.names=FALSE)
cat("\nMale:\n"); print(as.data.frame(decomp_male), row.names=FALSE)
cat("\n\n================================================================\n")
cat("EMPLOYMENT RATES BY AGE GROUP\n")
cat("================================================================\n\n")
print(as.data.frame(emp_rates), row.names=FALSE)
cat("\nYouth employment gap (21-25 minus 35-45):\n")
print(as.data.frame(emp_gap), row.names=FALSE)
cat("\n\n================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("================================================================\n\n")
print(as.data.frame(summary_stats), row.names=FALSE)
sink()

beep("ALL DONE")