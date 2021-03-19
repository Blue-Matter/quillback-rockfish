
# Inside
gf <- readRDS("data-raw/quillback_inside_data_jan21.rds")

# Catch
generate_catch <- function() {
  catch <- gf[[3]] %>% group_by(year, fishery_sector, minor_stat_area_code) %>% 
    summarise(land = sum(landed_kg), disc = sum(discarded_kg), n_trip = unique(fishing_event_id) %>% length()) %>% 
    dplyr::mutate(Catch = 1e-3 * (land + disc))
}
generate_catch() %>% ggplot(aes(year, Catch, colour = minor_stat_area_code)) + 
  geom_line() + geom_point() +
  facet_wrap(~ fishery_sector, scales = "free_y") + theme_bw()
ggsave("inside/figures/data_catch_sector_area.png", height = 4, width = 8)

generate_catch() %>% ggplot(aes(year, Catch)) + 
  stat_summary(fun = "sum", geom = "line") + 
  stat_summary(fun = "sum", geom = "point") + 
  facet_wrap(~ fishery_sector, scales = "free_y") + theme_bw()
ggsave("inside/figures/data_catch_sector.png", height = 4, width = 8)

generate_catch() %>% group_by(year, fishery_sector) %>% summarise(Catch = sum(Catch, na.rm = TRUE)) %>% 
  reshape2::acast(list("year", "fishery_sector"), value.var = "Catch") %>% 
  write.csv("inside/tables/data_catch_sector.csv")

# reconstructed rec catch
rec <- read.csv("data-raw/insideQB_total_Rec_Catch_Year.csv")
plot(QBCatch ~ YEAR, rec, typ = "o", ylab = "Rec catch (pieces?)")
#lines(1e3 * tonnes ~ YEAR, rec, typ = "o")
plot(totalEffort ~ YEAR, rec, typ = "o")

png("inside/figures/data_rec_catch.png", height = 3, width = 4, units = "in", res = 400)
par(mar = c(5, 4, 1, 1))
plot(tonnes ~ YEAR, rec, typ = "o", ylab = "Rec catch (t)", ylim = c(0, 70))
dev.off()

# catch
comm_ns <- read.csv("data-raw/Catch-History-424-noStrat.csv")
comm_ys <- read.csv("data-raw/Catch-History-424-yesStrat.csv")

comm <- data.frame(year = comm_ns$year, NS = comm_ns[, 2], YS = comm_ys[, 2], Type = comm_ns$Type) %>%
  dplyr::filter(year < 2021) %>% 
  reshape2::melt(list("year", "Type"), value.name = "Catch", variable.name = "Method")

ggplot(comm, aes(year, Catch, linetype = Method, shape = Method)) + geom_point() + geom_line() + 
  facet_wrap(~Type, scale = "free_y") + theme_bw() + scale_shape_manual(values = c(16, 1))
ggsave("inside/figures/data_catch_reconstructed.png", height = 3, width = 6)

comm %>% group_by(year, Method) %>% summarise(Catch = sum(Catch)) %>% 
  ggplot(aes(year, Catch, linetype = Method, shape = Method)) + geom_line() + geom_point() + theme_bw() +
  scale_shape_manual(values = c(16, 1))
ggsave("inside/figures/data_catch_reconstructed_total.png", height = 3, width = 5)

