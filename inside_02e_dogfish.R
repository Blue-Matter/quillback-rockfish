
# Inside dogfish
library(gfplot)
library(sdmTMB)

d <- readxl::read_excel("data-raw/QB-inside_dogfish_survey_sets.xlsx")
joint_grid <- gfplot::dogfish_grid$grid

group_by(d, year) %>% summarise(n_na = sum(!is.na(quillback_count)))
group_by(d, year) %>% summarise(n_na = sum(is.na(quillback_count)))

d <- d %>% arrange(year, fishing_event_id)
d$circle_hook <- ifelse(d$hook_code == 1, 1, 0)
d$circle_hook_centered <- d$circle_hook - mean(d$circle_hook)
d$hook_code <- as.factor(d$hook_code)
d$dogfish_count[d$dogfish_count == 0] <- 1

# sf::st_crs(3156)
convert2utm <- function(df, coords = c("X", "Y"), out_crs = 3156) {
  x <- sf::st_as_sf(df,
                    coords = coords, crs = 4326
  ) %>%
    sf::st_transform(crs = out_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  x$X <- x$X / 100000
  x$Y <- x$Y / 100000
  dplyr::bind_cols(x,
                   df[, which(!names(df) %in% coords), drop = FALSE])
}
d_utm <- convert2utm(d, coords = c("longitude", "latitude"))
joint_grid_utm <- joint_grid %>%
  mutate(X = X / (1000*100), Y = Y / (1000*100))

joint_grid_utm$area <- gfplot::dogfish_grid$cell_area # .5x.5 km grid
# joint_grid_utm$hook_code <- filter(d_utm, year == 1986)$hook_code[1]
# joint_grid_utm$hook_code <- filter(d_utm, year == 2019)$hook_code[1]
joint_grid_utm$circle_hook_centered <- 0 # mean(d_utm$circle_hook_centered)
joint_grid_utm$dogfish_count <- mean(d_utm$dogfish_count)

d_utm$offset <- log(d_utm$area_swept_km2 * 100)
# d_utm$offset <- log(d_utm$lglsp_hook_count / 100)
joint_grid_utm$offset <- mean(d_utm$offset)
years <- sort(unique(d_utm$year))

expand_prediction_grid <- function(grid, years) {
  nd <- do.call("rbind",
                replicate(length(years), grid, simplify = FALSE))
  nd[["year"]] <- rep(years, each = nrow(grid))
  nd
}
joint_grid_utm <- expand_prediction_grid(joint_grid_utm, years = years)

# Fit models -----------------------------------------------------
length(unique(paste(d_utm$X, d_utm$Y)))
sp <- sdmTMB::make_mesh(d_utm, xy_cols = c("X", "Y"), n_knots = 300)
png("inside/figures/dogfish-mesh.png", width = 7, height = 7,
    units = "in", res = 200)
plot(sp)
dev.off()

ggplot(d_utm, aes(x = X, y = Y, fill = quillback_count)) + facet_wrap(~year) + 
  geom_tile(height = 0.05, width = 0.05) + theme_bw()

d_utm %>% group_by(year) %>% summarise(zero = sum(quillback_count == 0)/n())

m <- sdmTMB(
  formula =
    quillback_count ~ 0 + #circle_hook_centered +
    as.factor(year) +
    log(dogfish_count) + offset,
  data = d_utm,
  spde = sp,
  time = "year",
  silent = TRUE,
  anisotropy = TRUE,
  reml = TRUE,
  family = nbinom2(link = "log")
)
print(m)
sink("inside/tables/dogfish-model.txt")
print(m)
sink()
saveRDS(m, file = "data-generated/dogfish-model.rds")
m <- readRDS("data-generated/dogfish-model.rds")

set.seed(82302)
d_utm$resids <- residuals(m) # randomized quantile residuals


