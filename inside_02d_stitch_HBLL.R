
# Inside HBLL quillback
library(gfplot)
library(sdmTMB)

#### Generate inside grid Taken from inside yelloweye github repo
#d2 <- readRDS("data-raw/complete_research_blocks_table.rds")
#area <- readRDS("data-raw/All_HBLL_Blocks_Area_Water.rds") %>%
#  dplyr::filter(SS_ID %in% c(39, 40)) %>%
#  dplyr::select(block = BLOCK_DESI, area = Area_Water_km2)
#
#d2 <- d2 %>%
#  dplyr::filter(exported_research_blocks.SS_ID %in% c(39, 40)) %>%
#  select(exported_research_blocks.SS_ALT_DES, ZonalStats_bathy100.MEAN,
#         exported_research_blocks.LATITUDE, exported_research_blocks.LONGITUDE, exported_research_blocks.percent100_rock, exported_research_blocks.Percent20m_rock, exported_research_blocks.BLOCK_DESI) %>%
#  rename(survey = exported_research_blocks.SS_ALT_DES,
#         depth = ZonalStats_bathy100.MEAN, longitude = exported_research_blocks.LONGITUDE,
#         latitude = exported_research_blocks.LATITUDE,
#         rock20 = exported_research_blocks.Percent20m_rock,
#         rock100 = exported_research_blocks.percent100_rock,
#         block = exported_research_blocks.BLOCK_DESI) %>%
#  mutate(depth = -1 * depth) %>%
#  dplyr::filter(!is.na(depth)) %>% # fixme (removing 8)
#  mutate(survey = paste0("HBLL ", survey))
#
#plot(d2$rock100, d2$rock20)
#d2 <- left_join(d2, area)
#stopifnot(sum(is.na(d2$area)) == 0)
#saveRDS(d2, file = "data-generated/hbll-inside-grid.rds")

#f <- "data-generated/yelloweye-rockfish-inside.rds"
#if (file.exists(f)) {
#  d <- readRDS(f)
#} else {
#  d <- gfdata::get_survey_sets("yelloweye rockfish", ssid = c(39, 40))
#  block_ids <- gfdata::get_survey_blocks(ssid = c(39, 40))
#  d <- dplyr::left_join(d, block_ids)
#  d$block_designation <- as.numeric(d$block_designation)
#  saveRDS(d, file = f)
#}
d <- readRDS("data-raw/inside-surveys-blocks.rds")

hook <- readRDS("data-raw/quillback_insideHBLL_hook_adjusted.rds") %>%
  select(
    survey_abbrev, survey_series_id, year, fishing_event_id, total_hooks, prop_bait_hooks,
    hook_adjust_factor, catch_count, catch_weight, hook_count, latitude, longitude, depth
  ) %>% 
  rename(survey = survey_abbrev) 

d <- left_join(hook, d, by = "fishing_event_id")


#dogfish_test_remove <- FALSE
#if (dogfish_test_remove) {
#  dg <- c(5151558, 5151667, 5151581, 5151551, 5151555, 5151559, 5151676,
#          5151584, 5151588, 5151592, 5151562, 5151665, 5151586, 5151595,
#          5151670, 5151671, 5151554, 5151591, 5151675)
#  d <- filter(d, !fishing_event_id %in% dg)
#}


convert2utm <- function(df, coords = c("X", "Y"), out_crs = 3156) {
  x <- sf::st_as_sf(df,
                    coords = coords, crs = 4326
  ) %>%
    sf::st_transform(crs = out_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  x$X <- x$X / 100000
  x$Y <- x$Y / 100000
  dplyr::bind_cols(x, df)
}
d_utm <- convert2utm(d, coords = c("longitude", "latitude")) %>% dplyr::filter(!is.na(year))
#d_utm <- filter(d_utm, grouping_code > 270 & grouping_code < 330) # not all part of survey

joint_grid <- readRDS("data-generated/hbll-inside-grid.rds")

d_utm$depth_log <- log(d_utm$depth)
d_utm$depth_centred <- d_utm$depth_log - mean(d_utm$depth_log)
d_utm$depth_scaled <- d_utm$depth_centred / sd(d_utm$depth_centred)
d_utm$Y_cent <- d_utm$Y - mean(d_utm$Y)
d_utm$X_cent <- d_utm$X - mean(d_utm$X)
d_utm$area_swept <- d_utm$hook_count * 0.0024384 * 0.009144 * 1000
d_utm$offset_area_swept <- log(d_utm$area_swept)
d_utm$offset_area_hook <- log(d_utm$area_swept / d_utm$hook_adjust_factor)

joint_grid_utm <- convert2utm(joint_grid, coords = c("longitude", "latitude"))
joint_grid_utm$offset_area_hook <- mean(d_utm$offset_area_hook)
joint_grid_utm$offset_area_swept <- mean(d_utm$offset_area_swept)

expand_prediction_grid <- function(grid, years) {
  nd <- do.call("rbind",
                replicate(length(years), grid, simplify = FALSE))
  nd[["year"]] <- rep(years, each = nrow(grid))
  nd
}
joint_grid_utm <- expand_prediction_grid(joint_grid_utm, years = sort(unique(d_utm$year))) %>%
  mutate(depth_centred = log(depth) - mean(d_utm$depth_log)) %>%
  mutate(depth_scaled = depth_centred / sd(d_utm$depth_centred))
joint_grid_utm <- mutate(joint_grid_utm, Y_cent = Y - mean(d_utm$Y))
joint_grid_utm <- mutate(joint_grid_utm, X_cent = X - mean(d_utm$X))
north_grid_utm <- dplyr::filter(joint_grid_utm, survey %in% "HBLL INS N")
south_grid_utm <- dplyr::filter(joint_grid_utm, survey %in% "HBLL INS S")

get_predictions <- function(model) {
  predict(model,
          newdata = joint_grid_utm,
          return_tmb_object = TRUE, xy_cols = c("X", "Y"), area = joint_grid_utm$area
  )
}

# Fit models -----------------------------------------------------
model_file <- "data-generated/hbll-inside-joint-hook-eps.rds"
model_file_depth <- "data-generated/hbll-inside-joint-hook-eps-depth.rds"
model_file_nohook <- "data-generated/hbll-inside-joint-no-hook-eps.rds"
if (!file.exists(model_file) || !file.exists(model_file_depth) ||
    !file.exists(model_file_nohook)) {
  
  sp <- make_mesh(d_utm, c("X", "Y"), n_knots = 400)
  
  png("inside/figures/hbll-joint-spde.png", width = 7, height = 7, units = "in", res = 180)
  plot(sp)
  dev.off()
  
  fit_model <- function(formula, file) {
    tictoc::tic()
    .m <- sdmTMB(
      formula = formula,
      data = d_utm,
      spde = sp,
      time = "year",
      silent = TRUE,
      anisotropy = TRUE,
      #cores = cores,
      reml = TRUE,
      family = nbinom2(link = "log")
    )
    tictoc::toc()
    saveRDS(.m, file = file)
    .m
  }
  d_utm$offset <- d_utm$offset_area_hook
  m <- fit_model(catch_count ~ 0 + as.factor(year) + offset, model_file)
  m_depth <- fit_model(
    catch_count ~ 0 + as.factor(year) + depth_scaled + I(depth_scaled^2) + offset,
    model_file_depth
  )
  d_utm$offset <- d_utm$offset_area_swept
  m_nohook <- fit_model(catch_count ~ 0 + as.factor(year) + offset,
                        model_file_nohook
  )
  
  bias_correct <- FALSE
  joint_grid_utm$offset <- joint_grid_utm$offset_area_hook
  predictions <- get_predictions(m)
  ind <- get_index(predictions, bias_correct = TRUE)
  saveRDS(ind, file = "data-generated/hbll-inside-joint-index.rds")
  
  joint_grid_utm$offset <- joint_grid_utm$offset_area_hook
  predictions_depth <- get_predictions(m_depth)
  ind_depth <- get_index(predictions_depth, bias_correct = bias_correct)
  saveRDS(ind_depth, file = "data-generated/hbll-inside-joint-index-depth.rds")
  
  joint_grid_utm$offset <- joint_grid_utm$offset_area_swept
  predictions_nohook <- get_predictions(m_nohook)
  ind_nohook <- get_index(predictions_nohook, bias_correct = bias_correct)
  saveRDS(ind_nohook, file = "data-generated/hbll-inside-joint-index-nohook.rds")
  
} else {
  m <- readRDS(model_file)
  m_depth <- readRDS(model_file_depth)
  m_nohook <- readRDS(model_file_nohook)
  
  ind <- readRDS("data-generated/hbll-inside-joint-index.rds")
  ind_depth <- readRDS("data-generated/hbll-inside-joint-index-depth.rds")
  ind_nohook <- readRDS("data-generated/hbll-inside-joint-index-nohook.rds")
}
#m
#m_depth
#m_nohook

#sink("figs/hbll-model.txt")
#m
#sink()

set.seed(82302)
d_utm$resids <- residuals(m) # randomized quantile residuals

# Project density ------------------------------
s_years <- dplyr::filter(d_utm, survey == "HBLL INS S") %>% pull(year) %>% unique()

# What about the individual surveys? -----------------------
north_grid_utm$offset <- north_grid_utm$offset_area_hook
pred_north <- predict(m,
                      newdata = north_grid_utm,
                      return_tmb_object = TRUE, xy_cols = c("X", "Y"), area = north_grid_utm$area
)
ind_north <- get_index(pred_north, bias_correct = bias_correct)

south_grid_utm$offset <- south_grid_utm$offset_area_hook
pred_south <- predict(m,
                      newdata = south_grid_utm,
                      return_tmb_object = TRUE, xy_cols = c("X", "Y"), area = south_grid_utm$area
)
ind_south <- get_index(pred_south, bias_correct = bias_correct)

ind_north$type <- "HBLL INS N"
ind_south$type <- "HBLL INS S"
ind$type <- "HBLL INS"
ind_depth$type <- "HBLL INS (with depth)"
ind_nohook$type <- "HBLL INS (no hook competition)"

n_years <- dplyr::filter(d_utm, survey %in% "HBLL INS N") %>%
  pull(year) %>% unique()
s_years <- dplyr::filter(d_utm, survey %in% "HBLL INS S") %>%
  pull(year) %>% unique()
ind_north_plot <- dplyr::filter(ind_north, year %in% n_years)
ind_south_plot <- dplyr::filter(ind_south, year %in% s_years)

.geomean <- exp(mean(log(ind$est)))
.ratio <- (exp(mean(log(ind_depth$est))) / .geomean)
.ind_depth <- ind_depth %>%
  mutate(est = est / .ratio) %>%
  mutate(upr = upr / .ratio) %>%
  mutate(lwr = lwr / .ratio)

.ratio <- (exp(mean(log(ind_nohook$est))) / .geomean)
.ind_nohook <- ind_nohook %>%
  mutate(est = est / .ratio) %>%
  mutate(upr = upr / .ratio) %>%
  mutate(lwr = lwr / .ratio)

all_plot <- bind_rows(ind_north_plot, ind_south_plot) %>%
  bind_rows(ind) %>%
  mutate(type2 = type) %>%
  bind_rows(mutate(.ind_depth, type2 = "HBLL INS")) %>%
  bind_rows(mutate(.ind_nohook, type2 = "HBLL INS"))


# Raw data plots -----------------------------------------------------
coast <- rnaturalearth::ne_coastline(scale = 10, returnclass = "sf")

g <- ggplot(d_utm, aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(
    data = north_grid_utm, size = 0.5,
    colour = "grey50", fill = "white"
  ) +
  geom_tile(
    data = south_grid_utm, size = 0.5,
    colour = "grey80", fill = "white"
  ) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(
    size = catch_count / area_swept,
    colour = catch_count / exp(offset_area_hook)
  ), alpha = 1) +
  scale_color_viridis_c(trans = "log") +
  scale_fill_viridis_c(trans = "log", breaks = c(0.4, 1, 2.7, 7.4)) +
  scale_size_area(max_size = 8) +
  theme_bw() + theme(panel.spacing = unit(0, "in")) +
  labs(
    colour = "Numbers/swept area/hook", size = "Numbers/swept area",
    fill = "Numbers/swept area"
  )

g <- ggplot(d_utm) + facet_grid(year ~ survey) + 
  geom_histogram(binwidth = 1, boundary = 0,  
                 closed = "left", colour = "black", fill = "grey70",
                 aes(x = catch_count / exp(offset_area_hook), y = ..density..)) +
  theme_bw() + theme(panel.spacing = unit(0, "in")) + 
  labs(x = expression("Numbers/baited hook/"~km^2)) + coord_cartesian(xlim = c(0, 10))
ggsave("inside/figures/hbll-inside-raw-data-hist.png", g, width = 4, height = 7, dpi = 600)

g <- ggplot(d_utm, aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_label(data = dplyr::summarise(group_by(d_utm, year), n = round(sum(catch_count > 0)/n(), 2)),
             x = -123, y = 51, hjust = "inward", vjust = "inward", 
             inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(
    fill = catch_count / exp(offset_area_hook)
  ), alpha = 1) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.4, 1, 2.7, 7.4)) +
  scale_size_area(max_size = 8) +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    fill = expression("Numbers/baited hook/"~km^2),
    x = "Longitude", y = "Latitude"
  )
ggsave("inside/figures/hbll-inside-raw-data-with-zero.png", g, width = 10, height = 7, dpi = 600)

g <- ggplot(d_utm %>% dplyr::filter(year >= 2018), aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_label(data = dplyr::summarise(group_by(d_utm, year), n = round(sum(catch_count > 0)/n(), 2)) %>% 
               dplyr::filter(year >= 2018),
             x = -123, y = 51, hjust = "inward", vjust = "inward", 
             inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(
    fill = catch_count / exp(offset_area_hook)
  ), alpha = 1) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.4, 1, 2.7, 7.4)) +
  scale_size_area(max_size = 8) +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    fill = expression("Numbers/baited hook/"~km^2),
    x = "Longitude", y = "Latitude"
  )
ggsave("inside/figures/hbll-inside-raw-data-with-zero-2018.png", g, width = 8, height = 5, dpi = 600)

g <- ggplot(d_utm %>% dplyr::filter(catch_count > 0), aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_label(data = dplyr::summarise(group_by(d_utm, year), n = round(sum(catch_count > 0)/n(), 2)),
             x = -123, y = 51, hjust = "inward", vjust = "inward", 
             inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(
    fill = catch_count / exp(offset_area_hook)
  ), alpha = 1) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.4, 1, 2.7, 7.4)) +
  scale_size_area(max_size = 8) +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    fill = expression("Numbers/baited hook/"~km^2),
    x = "Longitude", y = "Latitude"
  )
ggsave("inside/figures/hbll-inside-raw-data.png", g, width = 10, height = 7, dpi = 600)

g <- ggplot(d_utm, aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(colour = hook_adjust_factor,
                                     fill = hook_adjust_factor), alpha = 0.2) +
  geom_point(pch = 21, mapping = aes(colour = hook_adjust_factor)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45)) +
  scale_size_area(max_size = 8) +
  labs(colour = "Hook\nadjustment\nfactor", fill = "Hook\nadjustment\nfactor")
ggsave("inside/figures/hbll-inside-joint-hook-adjust.png", g, width = 10, height = 7, dpi = 600)

g <- ggplot(d_utm, aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  facet_wrap(~year) +
  geom_point(pch = 21, mapping = aes(colour = prop_bait_hooks,
                                     fill = prop_bait_hooks), alpha = 0.2) +
  geom_point(pch = 21, mapping = aes(colour = prop_bait_hooks)) +
  scale_color_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45)) +
  scale_size_area(max_size = 8) +
  labs(colour = "Proportion\nbaited hooks", fill = "Proportion\nbaited hooks")
ggsave("inside/figures/hbll-inside-joint-baited.png", g, width = 10, height = 7, dpi = 600)

g <- ggplot(dplyr::filter(joint_grid_utm, year == 2019), aes(longitude, latitude)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(aes(fill = area), width = 0.03, height = 0.03) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45)) +
  labs(fill = expression(Area ~ "in" ~ water ~ (km^2)))
ggsave("inside/figures/hbll-inside-area-in-water.png", g, width = 7, height = 5, dpi = 600)

# Diagnostics and plots -----------------------------------
diverging_scale <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")

g <- ggplot(d_utm, aes(longitude, latitude, fill = resids)) +
  geom_sf(
    data = coast, inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  scale_fill_gradient2(high = "red",
                         low = "blue", mid = "grey90") +
  geom_point(shape = 21, size = 1.75) + facet_wrap(~year) + 
  theme_bw() + theme(panel.spacing = unit(0, "in"), axis.text.x = element_text(angle = 45)) +
  labs(fill = "Residual")
ggsave("inside/figures/hbll-inside-joint-residual-map.png", g, width = 10, height = 10, dpi = 600)

qqnorm(d_utm$resids)
qqline(d_utm$resids)

plot_map <- function(dat, column, wrap = TRUE) {
  gg <- ggplot(data = dat) +
    geom_tile(mapping = aes(X, Y, fill = {{ column }}), width = 0.025, height = 0.025) +
    coord_fixed() +
    scale_fill_viridis_c(option = "D")
  if (wrap) gg + facet_wrap(~year) else gg
}

plot_map2 <- function(dat, column, wrap = TRUE) {
  gg <- ggplot(data = dat) +
    geom_sf(
      data = coast, inherit.aes = FALSE
    ) +
    coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
    geom_tile(mapping = aes(longitude, latitude, fill = {{ column }}), width = 0.025, height = 0.025) +
    scale_fill_viridis_c(option = "D") +
    theme_bw()
  if (wrap) gg + facet_wrap(~year) else gg
}

g <- plot_map2(predictions$data, exp(est)) +
  scale_fill_viridis_c(trans = "sqrt", option = "D") +
  labs(
    fill = "Estimated\nadjusted\ncount",
    size = "Observed\nadjusted\ncount"
  ) +
  geom_point(
    data = d_utm, pch = 21, mapping = aes(
      x = longitude, y = latitude,
      size = catch_count / exp(offset_area_hook)
    ),
    inherit.aes = FALSE, colour = "grey20", alpha = 0.5
  ) +
  scale_size_area(max_size = 7)
ggsave("inside/figures/hbll-joint-prediction-sqrt.png", g, width = 10, height = 10)

g <- g + scale_fill_viridis_c(trans = "log", option = "D", breaks = c(0.37, 1, 2.72, 7.39, 20.1))
ggsave("inside/figures/hbll-joint-prediction-log.png", g, width = 10, height = 10)

g <- plot_map2(predictions$data, exp(est_non_rf)) +
  scale_fill_viridis_c(trans = "log", option = "D") +
  labs(fill = "Fixed effect\nestimate")
ggsave("inside/figures/hbll-joint-non-rf.png", g, width = 10, height = 10)

plot_map2(dplyr::filter(predictions$data, year == 2018), omega_s, wrap = FALSE) +
  scale_fill_viridis_b() + labs(fill = expression("Spatial effects" ~ omega[s]))
ggsave("inside/figures/hbll-inside-joint-omega.png", width = 5, height = 5, dpi = 600)

g <- plot_map2(predictions$data, epsilon_st) +
  scale_fill_viridis_c(trans = "log") + labs(fill = expression("Spatiotemporal effects" ~ epsilon[st]))
ggsave("inside/figures/hbll-inside-joint-epsilon.png", g, width = 10, height = 10, dpi = 600)

palette <- c(RColorBrewer::brewer.pal(5, "Set2"))
bind_rows(ind_north, ind_south) %>%
  bind_rows(ind) %>%
  mutate(type2 = type) %>%
  bind_rows(mutate(.ind_depth, type2 = "HBLL INS")) %>%
  bind_rows(mutate(.ind_nohook, type2 = "HBLL INS")) %>%
  # mutate(type = forcats::fct_rev(type)) %>%
  # filter(year > 2003) %>%
  ggplot(aes(year, est)) +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type), pch = 21) +
  # geom_point(data = all_plot, aes(colour = type)) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, fill = type),
  #            alpha = 0.2
  #) +
  facet_wrap(~type2, ncol = 1, scales = "free_y") +
  labs(colour = "Type", fill = "Type") +
  xlab("Year") + ylab("Estimated relative abundance") +
  geom_vline(xintercept = s_years, lty = 2, alpha = 0.6, lwd = 0.2) +
  theme_bw() + theme(panel.spacing = unit(0, "in"))
  #scale_color_manual(values = palette) +
  #scale_fill_manual(values = palette) +
  #scale_x_continuous(breaks = seq(2004, 2018, 2)) +
  #coord_cartesian(ylim = c(0, max(ind$upr) * 0.65), expand = FALSE,
  #                xlim = range(ind$year) + c(-0.3, 0.3)) +
  #theme(legend.position = c(0.26, 0.56))
# guides(colour = FALSE, fill = FALSE)
ggsave("inside/figures/hbll-inside-index-compare.png", width = 8, height = 8, dpi = 600)

optimize_png <- TRUE
if (optimize_png && !identical(.Platform$OS.type, "windows")) {
  files_per_core <- 2
  setwd("figs")
  system(paste0(
    "find -X . -name 'hbll-*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", parallel::detectCores() / 2, " optipng -strip all"
  ))
  setwd("../")
}

