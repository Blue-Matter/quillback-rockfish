
FRENCH <- FALSE

if (FRENCH) options(OutDec = ",")

library("dplyr")
library("SAMtool")
library("here")
library("purrr")
library("cowplot")
library("ggplot2")
library(rosettafish)

species_name <- "Inside Quillback Rockfish"
#starting_year <- 1918
#ending_year <- 2020
#all_years <- seq(starting_year, ending_year)
#nyear <- length(all_years)

if (!FRENCH) {
  fig_dir <- "inside/figures/RCM"
} else {
  dir.create("mse/figures-french", showWarnings = FALSE)
  fig_dir <- "mse/figures-french"
}
#if (!dir.exists(fig_dir)) dir.create(fig_dir)

# Set up the scenario names ---------------------------------------------------

sc <- tibble::tribble(
  ~scenario,                ~scenario_human,                  ~scenario_type,
  "RCM_stitch",               "(1) Base",                       "Reference",
  "RCM_stitch_HBLL_dome",     "(2) HBLL dome selectivity",      "Reference",
  "RCM_stitch_upW",           "(3) Upweight HBLL",              "Reference",
  "RCM_stitch_HBLL_dome_upW", "(4) Upweight HBLL w/dome",       "Reference",
  "RCM_stitch_dogfish",       "(A) Dogfish survey",             "Robustness",
  "RCM_stitch_HBLL_dome_incM","(B) Increasing M w/HBLL dome",   "Robustness",
)

sc_french <- c(
  "(1) Base",
  "(2) Faibles prises",
  "(3) Recrutement\népisodique",
  "(4) Estimation de la\nsélectivité du RPFD",
  "(A) Faible M ",
  "(B) CV élevé\ndu RPFD")
if (FRENCH) sc$scenario_human <- sc_french

sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = "inside/MSE/qb-scenarios.rds")

RCM <- lapply(sc$scenario, function(x) readRDS(paste0("inside/RCM/", x, ".rds"))) %>% 
  structure(names = sc$scenario)

scenarios <- sc$scenario %>% purrr::set_names(sc$scenario_human)

oms <- lapply(RCM, function(x) x@OM)
qb_converged <- map_dfr(oms, ~tibble(nsim = .x@nsim), .id = "scenario")
#saveRDS(yelloweye_converged, file = here("mse/om/ye-converged.rds"))

# Depletion -------------------------------------------------------------------
g <- gfdlm::plot_rcm_depletion(RCM, sc$scenario_human, FRENCH)
ggsave(file.path(fig_dir, paste0("insQB-compare-RCM-depletion-panel.png")), g,
       width = 8, height = 5
)

# SSB -------------------------------------------------------------------------
g <- gfdlm::plot_rcm_SSB(RCM, sc$scenario_human, FRENCH)
ggsave(file.path(fig_dir, paste0("insQB-compare-RCM-SSB-panel.png")), g,
       width = 8, height = 5
)

#F
g <- gfdlm::plot_rcm_F(RCM, sc$scenario_human, FRENCH)
ggsave(file.path(fig_dir, "insQB-compare-RCM-F-panel.png"), g,
       width = 8, height = 5
)

#Recdevs
g <- gfdlm::plot_rcm_recdev(RCM, sc$scenario_human, FRENCH, logspace = TRUE)
ggsave(file.path(fig_dir, "insQB-compare-RCM-recdev-panel.png"), g,
       width = 8, height = 5
)

# Recdevs for the whole historical and projection period
# Only plot the first 50 replicates (plot too dense and takes too long with too many)
# Log space

g <- gfdlm::plot_rcm_recdev(RCM, sc$scenario_human, FRENCH, logspace = TRUE, proj = TRUE)
ggsave(file.path(fig_dir, "insQB-compare-RCM-log-recdev-panel-proj.png"), g,
       width = 8, height = 5)

# Natural space
g <- gfdlm::plot_rcm_recdev(RCM, sc$scenario_human, FRENCH, proj = TRUE)
ggsave(file.path(fig_dir, "insQB-compare-RCM-nat-recdev-panel-proj.png"), g,
       width = 8, height = 5)

# SSB/SSBMSY  -----------------------------------------------------------------
#sra <- readRDS("mse/om/upweight_dogfish.rds")
#Hist <- runMSE(sra@OM, parallel = TRUE, Hist = TRUE)
#saveRDS(Hist, file = 'mse/om/Hist_upweight_dogfish.rds')
#MSE <- lapply(sc$scenario, function(x) readRDS(paste0("inside/MSE/MSE_", substr(x, 5, nchar(x)), ".rds"))) %>%
#  structure(names = sc$scenario)
#
#sg <- do.call(rbind, Map(get_SSB, x = RCM, scenario = sc$scenario_human, mse = MSE, type = "MSY", get_medians = TRUE)) %>%
#  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
#  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
#  geom_ribbon(fill = "grey90") +
#  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
#  geom_line() +
#  facet_wrap(vars(scenario)) +
#  gfplot::theme_pbs() +
#  labs(x = en2fr("Year", FRENCH), y = expression(B/B[MSY])) + coord_cartesian(expand = FALSE, ylim = c(0, 5)) +
#  geom_hline(yintercept = c(0.4, 0.8), linetype = 3)
#ggsave(file.path(fig_dir, "insQB-compare-RCM-MSY-panel.png"), sg,
#       width = 8, height = 5
#)
#
#g <- do.call(rbind, Map(get_SSB, x = RCM, scenario = sc$scenario_human, mse = MSE, type = "MSY")) %>%
#  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
#  ggplot(aes(year, value, group = iteration)) +
#  geom_line(alpha = 0.05) +
#  facet_wrap(vars(scenario)) +
#  gfplot::theme_pbs() +
#  labs(x = en2fr("Year", FRENCH), y = expression(B/B[MSY])) + coord_cartesian(expand = FALSE, ylim = c(0, 5)) +
#  geom_hline(yintercept = c(0.4, 0.8), linetype = 3)
#ggsave(file.path(fig_dir, paste0("ye-compare-SRA-MSY-panel-lines.png")), g,
#       width = 8, height = 5
#)

# Plot all surveys and OMs  ---------------------------------------------------
survey_names = c("HBLL INS N", "HBLL INS S", "HBLL INS", "Jig 12", "Jig 13", "Dogfish")
color_manual <- RColorBrewer::brewer.pal(4, "Set2") %>% structure(names = survey_names[-c(1:2)])

g <- gfdlm::plot_rcm_index(RCM, sc$scenario_human, FRENCH, i = 3:5, index_names = survey_names[3:5],
                           color = color_manual[1:3], xlim = c(1980, 2020))
ggsave(file.path(fig_dir, "insQB-index-all.png"), g, width = 8, height = 4)

g <- gfdlm::plot_rcm_index(RCM, sc$scenario_human, FRENCH, i = 3, index_names = survey_names[3],
                           color = color_manual[1], xlim = c(1980, 2020))
ggsave(file.path(fig_dir, "insQB-index-HBLL.png"), g, width = 8, height = 4)

g <- gfdlm::plot_rcm_index(RCM, sc$scenario_human, FRENCH, i = 6, index_names = survey_names[6],
                           color = color_manual[4], xlim = c(1980, 2020))
ggsave(file.path(fig_dir, "insQB-index-dogfish.png"), g, width = 8, height = 4)

# Plot HBLL age comps, report N = sampling trips for the multinomial likelihood in the SRA -----------
RCM_data <- readRDS("inside/OM/OM_dat.rds")

for(i in 1:length(RCM)) {
  if (FRENCH) {
    .dir <- "mse/figures-french/conditioning/" 
  } else {
    .dir <- "inside/figures/RCM/"
  }
  
  g <- gfdlm::plot_rcm_age_comps(RCM[[i]], sc$scenario_human[i], FRENCH, type = "fleet", i = 1, color = "red")
  ggsave(paste0(.dir, "HL_age_comp_", sc$scenario[i], ".png"), g, width = 8, height = 6)
  
  g <- gfdlm::plot_rcm_age_comps(RCM[[i]], sc$scenario_human[i], FRENCH, type = "index", i = 3, color = color_manual[1])
  ggsave(paste0(.dir, "HBLL_age_comp_", sc$scenario[i], ".png"), g, width = 8, height = 6)
  
  g <- gfdlm::plot_rcm_age_comps(RCM[[i]], sc$scenario_human[i], FRENCH, type = "index", i = 4, color = color_manual[2])
  ggsave(paste0(.dir, "Jig12_age_comp_", sc$scenario[i], ".png"), g, width = 4, height = 4)
  
  g <- gfdlm::plot_rcm_age_comps(RCM[[i]], sc$scenario_human[i], FRENCH, type = "index", i = 5, color = color_manual[3])
  ggsave(paste0(.dir, "Jig13_age_comp_", sc$scenario[i], ".png"), g, width = 6, height = 2)
}

# Plot mean age of HBLL and comm
g <- gfdlm::plot_rcm_mean_age(RCM, sc$scenario_human, type = "index", i = 3, name = "HBLL")
ggsave(file.path(fig_dir, "HBLL-mean-age.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_mean_age(RCM, sc$scenario_human, type = "fleet", i = 1, name = "Commercial")
ggsave(file.path(fig_dir, "comm-mean-age.png"), g, width = 8, height = 5)

# Plot mean length of HBLL and comm
g <- gfdlm::plot_rcm_mean_length(RCM, sc$scenario_human, type = "index", i = 3, name = "HBLL mean length")
ggsave(file.path(fig_dir, "HBLL-mean-length.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_mean_length(RCM, sc$scenario_human, type = "fleet", i = 1, name = "Commercial mean length")
ggsave(file.path(fig_dir, "comm-mean-length.png"), g, width = 8, height = 5)

# Plot length comp
g <- gfdlm::plot_rcm_length_comps(RCM[[1]], sc$scenario_human[1], type = "index", i = 3)
ggsave(file.path(fig_dir, "HBLL-mean-length.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_length_comps(RCM[[1]], sc$scenario_human[1], type = "fleet", i = 1)
ggsave(file.path(fig_dir, "comm-mean-length.png"), g, width = 8, height = 5)


# Plot selectivity
g <- gfdlm::plot_rcm_sel(RCM, sc$scenario_human, type = "index", i = 3, name = "HBLL")
ggsave(file.path(fig_dir, "HBLL-selectivity.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_sel(RCM, sc$scenario_human, type = "index", i = 4, name = "Jig Area 12")
ggsave(file.path(fig_dir, "Jig12-selectivity.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_sel(RCM, sc$scenario_human, type = "index", i = 5, name = "Jig Area 13")
ggsave(file.path(fig_dir, "Jig13-selectivity.png"), g, width = 8, height = 5)

g <- gfdlm::plot_rcm_sel_multi(RCM, sc$scenario_human, fleet_i = 1:2, index_i = 3,
                               fleet_names = c("Commercial", "Recreational"),
                               index_names = "HBLL")
ggsave(file.path(fig_dir, "fishery-selectivity.png"), g, width = 8, height = 5)

# Plot sel vs. maturity
g <- plot_rcm_bio_sel(RCM, sc$scenario_human, bio_type = "mat", sel_name = "Commercial selectivity") +
  scale_shape_manual(values = c("Maturity" = 1, "Commercial selectivity" = 16))
ggsave(file.path(fig_dir, "fishery-selectivity-maturity.png"), g, width = 8, height = 5)

# Plot survey biomass at age
g <- plot_rcm_biomass_age(RCM[[1]], sc$scenario_human[1], type = 'index', i = 3)
ggsave(file.path(fig_dir, "HBLL-biomass-age-prop.png"), g, width = 8, height = 5)



