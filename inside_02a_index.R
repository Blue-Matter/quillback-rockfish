

# other
other_survey <- read.csv("data-raw/quillback_survey_index.csv")
other_survey %>% 
  dplyr::filter(survey_series_desc == "Strait of Georgia Dogfish Longline" | survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S") %>% 
  ggplot(aes(year, biomass, ymin = lowerci, ymax = upperci)) + geom_point() + geom_line() + 
  facet_grid(survey_abbrev ~ ., scales = "free_y") + geom_linerange() + theme_bw()
ggsave("inside/figures/data_index.png", height = 6, width = 4)

ggplot(other_survey, aes(year, biomass, ymin = lowerci, ymax = upperci)) + geom_point() + geom_line() + 
  facet_wrap(~survey_abbrev, scales = "free_y") + geom_linerange() + theme_bw()
ggsave("qb_index.png", height = 6, width = 8)

other_survey %>% dplyr::filter(survey_abbrev == "OTHER") %>%
ggplot(aes(year, biomass, ymin = lowerci, ymax = upperci)) + geom_point() + geom_line() + 
  facet_wrap(~survey_series_desc, scales = "free_y") + geom_linerange() + theme_bw()
ggsave("qb_index.png", height = 6, width = 8)


# Stitch HBLL index
HBLL <- readRDS("data-raw/quillback_insideHBLL_hook_adjusted.rds")

HBLL %>% group_by(year, survey_desc) %>% summarise(x = n())

# Mean depth of sets
ggplot(HBLL) + geom_histogram(aes(x = depth), binwidth = 5) + facet_grid(year ~ survey_abbrev) + 
  theme_bw() + theme(panel.spacing = unit(0, "in")) + 
  geom_text(data = HBLL %>% group_by(year, survey_abbrev) %>% summarise(depth = mean(depth)),
            aes(label = depth %>% round(2)), x = 160, y = 15)
ggsave("inside/figures/HBLL_set_depth.png", height = 6, width = 3)

# Map sets
coast <- rnaturalearth::ne_coastline(scale = 10, returnclass = "sf")
zero_sets <- dplyr::summarise(group_by(HBLL, year), n = round(sum(catch_count > 0)/n(), 2))
g <- ggplot(HBLL %>% dplyr::filter(catch_count > 0), aes(x = longitude, y = latitude)) + 
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(height = 0.15, width = 0.15, aes(fill = 100 * catch_count/hook_count)) + 
  geom_text(data = zero_sets, x = -123, y = 51, hjust = "inward", vjust = "inward", 
            inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~ year) + theme_bw() + theme(panel.spacing = unit(0, "in")) +
  scale_fill_viridis_b(trans = "log", breaks = c(1, 2.7, 7.5, 20.1)) +
  labs(fill = "Numbers/100 hooks", x = "Longitude", y = "Latitude")
ggsave("inside/figures/HBLL_set_map.png", g, height = 8, width = 10, dpi = 600)

g <- ggplot(HBLL, aes(x = longitude, y = latitude)) + 
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(height = 0.15, width = 0.15, aes(fill = 100 * catch_count/hook_count)) + 
  geom_text(data = zero_sets, x = -123, y = 51, hjust = "inward", vjust = "inward", 
            inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~ year) + theme_bw() + theme(panel.spacing = unit(0, "in")) +
  scale_fill_viridis_b(trans = "log", breaks = c(1, 2.7, 7.5, 20.1)) +
  labs(fill = "Numbers/100 hooks", x = "Longitude", y = "Latitude")
ggsave("inside/figures/HBLL_set_map_with_zero.png", g, height = 8, width = 10, dpi = 600)

g <- ggplot(HBLL %>% dplyr::filter(catch_count > 0, year >= 2018), aes(x = longitude, y = latitude)) + 
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(height = 0.15, width = 0.15, aes(fill = 100 * catch_count/hook_count)) + 
  geom_text(data = zero_sets %>% dplyr::filter(year >= 2018), x = -123, y = 51, hjust = "inward", vjust = "inward", 
            inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~ year) + theme_bw() + theme(panel.spacing = unit(0, "in")) +
  scale_fill_viridis_b(trans = "log", breaks = c(1, 2.7, 7.5, 20.1)) +
  labs(fill = "Numbers/100 hooks", x = "Longitude", y = "Latitude")
ggsave("inside/figures/HBLL_set_map_2018.png", g, height = 4, width = 8, dpi = 600)

g <- ggplot(HBLL %>% dplyr::filter(year >= 2018), aes(x = longitude, y = latitude)) + 
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(xlim = c(-128, -123), ylim = c(48, 51)) +
  geom_tile(height = 0.15, width = 0.15, aes(fill = 100 * catch_count/hook_count)) + 
  geom_text(data = zero_sets %>% dplyr::filter(year >= 2018), x = -123, y = 51, hjust = "inward", vjust = "inward", 
            inherit.aes = FALSE, aes(label = n)) +
  facet_wrap(~ year) + theme_bw() + theme(panel.spacing = unit(0, "in")) +
  scale_fill_viridis_b(trans = "log", breaks = c(1, 2.7, 7.5, 20.1)) +
  labs(fill = "Numbers/100 hooks", x = "Longitude", y = "Latitude")
ggsave("inside/figures/HBLL_set_map_2018_with_zero.png", g, height = 4, width = 8, dpi = 600)


