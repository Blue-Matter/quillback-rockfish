FRENCH <- FALSE

if (FRENCH) options(OutDec = ",")

library(reshape2)
library(dplyr)
library(gfplot)
library(ggplot2)
library(rosettafish)

gf <- readRDS("data-raw/quillback_inside_data_jan21.rds")

surv_samples <- gf[[1]] %>% 
  dplyr::select(year, age, length, sex, weight, usability_code, sample_id, specimen_id, trip_start_date,
                maturity_code, maturity_convention_code) %>%
  dplyr::mutate(Source = "Survey", species_common_name = "qb")
comm_samples <- gf[[2]] %>% 
  dplyr::select(year, age, length, sex, weight, usability_code, sample_id, specimen_id, trip_start_date,
                maturity_code, maturity_convention_code) %>%
  dplyr::mutate(Source = "Commercial", species_common_name = "qb")
samps <- rbind(surv_samples, comm_samples)
samps$usability_code <- 1L

# GROWTH
check_convergence_tmb <- TRUE

tmb_init <- list(k = 0.1, linf = 30, log_sigma = log(0.1), t0 = -1)

# Linf = 38.5, K = 0.10, t0 = -3.54, sigma = 0.10
vb_m <- fit_vb(samps, sex = "male", method = "tmb",
               too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
               tmb_init = tmb_init)
# Linf = 40.6, K = 0.08, t0 = -4.71, sigma = 0.10
vb_f <- fit_vb(samps, sex = "female", method = "tmb",
               too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
               tmb_init = tmb_init)

# Linf = 39.2, K = 0.10, t0 = -3.42, sigma = 0.10
vb_a <- fit_vb(samps, sex = "all", method = "tmb",
               too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
               tmb_init = tmb_init)

#----------------------------------------------------
# use this plot to plot vb for both sexes combined
# plot_growth(object_all = vb_a) + geom_line(data = vb_a$predictions, aes(age, length)) +
#   guides(col = FALSE, lty = FALSE)  +
#   ggtitle("Growth")
#----------------------------------------------------

#----------------------------------------------------
# use this section to plot both male and female, as well as combined parameters
g <- plot_growth(object_f = vb_f, object_m = vb_m, lab_x = 0.6, lab_x_gap = 0.20, french = FRENCH) +
  geom_line(data = vb_a$predictions, aes(age, length)) +
  guides(col = FALSE, lty = FALSE)  +
  ggtitle(en2fr("Growth", FRENCH)) +
  xlab(en2fr("Age (years)", FRENCH)) +
  ylab(paste(en2fr("Length", FRENCH), "(cm)"))

gfplot:::ann_vb(g, vb_a$pars,
                en2fr("Both sexes", FRENCH), col = "black",
                x = 0.35*max(vb_a$predictions$age),
                y = 0.36*max(vb_a$predictions$length),
                gap = 0.06 * max(vb_a$predictions$length))
#-----------------------------------------------------
ggsave("inside/figures/vb.png", width = 4, height = 3)


# Plot figure and residual
datpl <- samps %>%
  mutate(pred = vb_a$pars$linf * (1 - exp(-vb_a$pars$k * (age - vb_a$pars$t0))) * exp(-0.5 * exp(vb_a$pars$log_sigma)^2),
         resid = length - pred, resid_log = log(length/pred))
plot(length ~ jitter(age), datpl)
lines(length ~ age, vb_a$predictions, col = "red", lwd = 3)

plot(resid ~ jitter(age), datpl)
abline(h = 0)

plot(resid_log ~ jitter(age), datpl)
abline(h = 0)


ML <- summarise(group_by(samps, Source, age), ML = mean(length, na.rm = TRUE))
ggplot(ML, aes(age, ML, colour = Source)) + geom_point() + geom_line() + theme_bw() + labs(y = "Mean length") +
  geom_line(data = vb_a$predictions, inherit.aes = FALSE, aes(age, length))
ggsave("inside/figures/data_length_ML.png", height = 3, width = 5)


ggplot(samps, aes(age, length, colour = Source)) + geom_jitter(alpha = 0.6) + theme_bw() + labs(y = "Length (cm)", x = "Age") +
  geom_line(data = vb_a$predictions, inherit.aes = FALSE, aes(age, length))
ggsave("inside/figures/data_length_source.png", height = 3, width = 5)

ML <- gf[[1]] %>% 
  dplyr::select(year, age, length, sex, weight, usability_code, sample_id, specimen_id, trip_start_date,
                survey_abbrev, maturity_code, maturity_convention_code) %>%
  dplyr::mutate(species_common_name = "qb") %>%
  group_by(age, survey_abbrev) %>% summarise(ML = mean(length, na.rm = TRUE))

ggplot(ML, aes(age, ML, colour = survey_abbrev)) + geom_point() + geom_line() + theme_bw() + labs(y = "Mean length") +
  geom_line(data = vb_a$predictions, inherit.aes = FALSE, aes(age, length))
ggsave("inside/figures/data_length_survey.png", height = 3, width = 5)


ML <- gf[[2]] %>% 
  dplyr::select(year, age, length, sex, weight, usability_code, sample_id, specimen_id, trip_start_date,
                gear_desc, maturity_code, maturity_convention_code) %>%
  dplyr::mutate(species_common_name = "qb") %>%
  group_by(age, gear_desc) %>% summarise(ML = mean(length, na.rm = TRUE))
ggplot(ML, aes(age, ML, colour = gear_desc)) + geom_point() + geom_line() + theme_bw() + labs(y = "Mean length") +
  geom_line(data = vb_a$predictions, inherit.aes = FALSE, aes(age, length))
ggsave("inside/figures/data_length_gear.png", height = 3, width = 5)




lw_m <- fit_length_weight(samps, sex = "male", method = "tmb",
                          too_high_quantile = 1)
lw_f <- fit_length_weight(samps, sex = "female", method = "tmb",
                          too_high_quantile = 1)

# log_a = -11.05, b = 3.06
lw_a <- gfplot::fit_length_weight(samps, sex = "all", method = "tmb", too_high_quantile = 1)
plot_length_weight(object_all = lw_a, french = FRENCH) + geom_line(data = lw_a$predictions, aes(length, weight)) +
  guides(col = FALSE, lty = FALSE)
ggsave("inside/figures/length-weight.png", width = 4, height = 3)





#### MATURITY
if (sum(!is.na(samps$maturity_code)) > 10) {
  mat_age <- samps %>%
    fit_mat_ogive(
      type = "age",
      months = seq(1, 12),
      link = "cauchit")
} else {
  mat_age <- NA
}

ogive_age <- plot_mat_ogive(mat_age)
ggsave("inside/figures/mat-ogive-age-cauchit.png", width = 5, height = 3)

if (sum(!is.na(dat$survey_samples$maturity_code)) > 10) {
  mat_length <- samps %>%
    fit_mat_ogive(
      type = "length",
      months = seq(1, 12),
      link = "cauchit")
} else {
  mat_length <- NA
}

ogive_length <- plot_mat_ogive(mat_length)
ggsave("inside/figures/mat-ogive-length-cauchit.png", width = 5, height = 3)

cowplot::plot_grid(ogive_age, ogive_length, nrow = 2, ncol = 1)
ggsave("inside/figures/mat-ogives-cauchit.png", width = 5, height = 6)


# Show predicted vs. observed proportions
gender_labeller <- function(variable, value) {
  codes <- c("Male", "Female")
  codes[value]
} 

prop <- mat_age[[3]]$data %>% group_by(female, age_or_length) %>% summarise(prop_mature = sum(mature)/n())
ggplot(prop, aes(age_or_length, prop_mature)) +
  geom_hline(yintercept = c(0, 1), colour = "grey") + 
  geom_point() + geom_line(data = mat_age[[2]], aes(age_or_length, glmm_fe)) + 
  gfplot::theme_pbs() + facet_grid(female ~ ., labeller = labeller(female = gender_labeller)) +
  coord_cartesian(xlim = c(0, 60)) + labs(x = "Age (years)", y = "Probability mature") 
ggsave("inside/figures/mat-prop-cauchit.png", width = 5, height = 6)

prop <- mat_length[[3]]$data %>% group_by(female, age_or_length) %>% summarise(prop_mature = sum(mature)/n())
ggplot(prop, aes(age_or_length, prop_mature)) + 
  geom_hline(yintercept = c(0, 1), colour = "grey") +
  geom_point() + geom_line(data = mat_length[[2]], aes(age_or_length, glmm_fe)) + gfplot::theme_pbs() +
  facet_grid(female ~ ., labeller = labeller(female = gender_labeller)) +
  coord_cartesian(xlim = c(0, 60)) + labs(x = "Length (cm)", y = "Probability mature") 
ggsave("inside/figures/mat-len-prop-cauchit.png", width = 5, height = 6)



# Plot maturity by month: ----------------------------------------------------
gfplot::tidy_maturity_months(samps) %>% gfplot::plot_maturity_months()
ggsave("inside/figures/mat-months.png", width = 5, height = 3)

# Age frequencies (by survey, HBLL INS only): ---------------------------------
ages <- gfplot::tidy_ages_raw(gf[[1]],
                              survey = c("HBLL INS N", "HBLL INS S"),
                              sample_type = "survey")

survey_col_names = c("HBLL INS N", "HBLL INS S", "HBLL OUT S", "OTHER")
survey_cols = c(RColorBrewer::brewer.pal(length(survey_col_names), "Set2"))
survey_cols <- stats::setNames(survey_cols, survey_col_names)

g_ages <- gfplot::plot_ages(ages, survey_cols = survey_cols) +
  guides(fill = FALSE, colour = FALSE) +
  ggtitle("Age frequencies") +
  labs(y = "Age (years)")
ggsave("inside/figures/age-freq-hbll.png", width = 3, height = 5)

# Age frequencies (by survey, all surveys): ----------------------------------------------------------
ages <- gfplot::tidy_ages_raw(dat$survey_samples,
                              survey = c("HBLL INS N", "HBLL INS S", "HBLL OUT S", "OTHER"),
                              sample_type = "survey")

survey_col_names = c("HBLL INS N", "HBLL INS S", "HBLL OUT S", "OTHER")
survey_cols = c(RColorBrewer::brewer.pal(length(survey_col_names), "Set2"))
survey_cols <- stats::setNames(survey_cols, survey_col_names)

g_ages <- gfplot::plot_ages(ages, survey_cols = survey_cols) +
  guides(fill = FALSE, colour = FALSE) +
  ggtitle("Age frequencies") +
  labs(y = "Age (years)")+
  scale_x_continuous(
    breaks =
      c(1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018))
ggsave("inside/figures/age-freq.png", width = 6, height = 5)

# Age frequencies (all surveys combined): ------------------------------------
# ages <- dat$survey_samples %>%
#   mutate(survey_abbrev = "All surveys") %>%
#   gfplot::tidy_ages_raw(
#     survey = "All surveys",
#     sample_type = "survey")
#
# survey_col_names = c("All surveys")
# survey_cols = c(RColorBrewer::brewer.pal(length(survey_col_names), "Set2"))
# survey_cols <- stats::setNames(survey_cols, survey_col_names)
#
# g_ages <-
#   gfplot::plot_ages(ages, survey_cols = survey_cols) +
#   guides(fill = FALSE, colour = FALSE) +
#   ggtitle("Age frequencies")  +
#   labs(y = "Age (years)", x = "Years")
# ggsave("mse/figures/age-freq-all-surveys-combined.png", width = 3, height = 5)

# Length frequencies (HBLL INS only): ----------------------------------------------------------
len <- gfplot::tidy_lengths_raw(gf[[1]],
                                sample_type = "survey",
                                survey = c("HBLL INS N", "HBLL INS S"))

len$survey_abbrev <- factor(len$survey_abbrev,
                            levels = c("HBLL INS N", "HBLL INS S"))

g_lengths <- gfplot::plot_lengths(len, survey_cols = survey_cols,
                                  bin_size = 2) +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("Length frequencies") +
  ggplot2::xlab(paste("Length", "(cm)")) +
  ggplot2::ylab("Relative length frequency")
ggsave("inside/figures/length-freq-hbll.png", width = 3, height = 6)

# Length frequencies (by survey, all surveys): ----------------------------------------------------------
len <- gfplot::tidy_lengths_raw(gf[[1]],
                                sample_type = "survey",
                                survey = c("HBLL INS N", "HBLL INS S", "HBLL OUT S", "OTHER")) %>%
  dplyr::filter(total > 20)

len$survey_abbrev <- factor(len$survey_abbrev,
                            levels = c("HBLL INS N", "HBLL INS S", "HBLL OUT S", "OTHER"))

g_lengths <- gfplot::plot_lengths(len, survey_cols = survey_cols,
                                  bin_size = 2) +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("Length frequencies") +
  ggplot2::xlab(paste("Length", "(cm)")) +
  ggplot2::ylab("Relative length frequency")
ggsave("inside/figures/length-freq.png", width = 5, height = 8)

# Length frequencies (all surveys combined): ----------------------------------------------------------
# len <- dat$survey_samples %>%
#   filter(!is.na(length)) %>%
#   mutate(survey_abbrev = "All surveys") %>%
#   gfplot::tidy_lengths_raw(
#     survey = "All surveys",
#     sample_type = "survey")
#
# len$survey_abbrev <- factor(len$survey_abbrev,
#   levels ="All surveys")
#
# g_lengths <- gfplot::plot_lengths(len, survey_cols = NULL,
#   bin_size = 2) +
#   #guides(colour = FALSE, fill = FALSE) +
#   ggtitle("Length frequencies") +
#   ggplot2::xlab(paste("Length", "(cm)")) +
#   ggplot2::ylab("Relative length frequency")
# ggsave("mse/figures/length-freq-all-surveys-combined.png", width = 3, height = 5)

optimize_png <- TRUE
if (optimize_png && !identical(.Platform$OS.type, "windows")) {
  files_per_core <- 4
  setwd("mse/figures")
  system(paste0(
    "find -X . -name 'ye*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", parallel::detectCores() / 2, " optipng -strip all"
  ))
  setwd("../../")
}

