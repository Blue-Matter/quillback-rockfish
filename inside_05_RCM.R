
library(SAMtool)

OM <- readRDS("inside/OM/ins_OM.rds")
dat <- readRDS("inside/OM/OM_dat.rds")

f_name <- c("Commercial", "Recreational")
s_name <- c("HBLL N", "HBLL S", "HBLL INS", "Jig Area 12", "Jig Area 13", "Dogfish")




RCM_base <- local({
  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, c(1, 2, 4)] <- 1:6
  map_s_vul_par[1:3, 5] <- 7:9
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, map_s_vul_par = map_s_vul_par)
})

saveRDS(RCM_base, "inside/RCM/RCM_base.rds")
plot(RCM_base, compare = FALSE, dir = "inside/RCM", filename = "RCM", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_comm_break <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  #map_vul_par <- matrix(c(1, 2, 3), 3, 2)
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, c(1, 2, 4)] <- 1:6
  map_s_vul_par[1:3, 5] <- 7:9
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_comm_break, "inside/RCM/RCM_comm_break.rds")
plot(RCM_comm_break, compare = FALSE, dir = "inside/RCM", filename = "RCM_comm_break", open_file = FALSE,
     f_name = f_name, s_name = s_name)




RCM_HBLLS_dome <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  #map_vul_par <- matrix(c(1, 2, 3), 3, 2)
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, 1:2] <- 1:4
  map_s_vul_par[3, 2] <- 5
  map_s_vul_par[1:2, 4] <- 6:7
  map_s_vul_par[1:3, 5] <- 8:10
  
  s_selectivity <- c("logistic", "dome", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_HBLLS_dome, "inside/RCM/RCM_HBLLS_dome.rds")
plot(RCM_HBLLS_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_HBLLS_dome", open_file = FALSE,
     f_name = f_name, s_name = s_name)

RCM_HBLL_dome <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  #map_vul_par <- matrix(c(1, 2, NA), 3, 2)
  
  #selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:3, 1:2] <- 1:6
  map_s_vul_par[1:2, 4] <- 7:8
  map_s_vul_par[1:3, 5] <- 9:11
  
  s_selectivity <- c("dome", "dome", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, map_s_vul_par = map_s_vul_par)
})

saveRDS(RCM_HBLL_dome, "inside/RCM/RCM_HBLL_dome.rds")
plot(RCM_HBLL_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_HBLL_dome", open_file = FALSE,
     f_name = f_name, s_name = s_name)





RCM_dogfish <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  
  selectivity <- c("logistic", "logistic")
  
  #map_vul_par <- matrix(c(1, 2, NA), 3, 2)
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, c(1, 2, 4)] <- 1:6
  map_s_vul_par[1:3, 5] <- 7:9
  map_s_vul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 1))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})

saveRDS(RCM_dogfish, "inside/RCM/RCM_dogfish.rds")
plot(RCM_dogfish, compare = FALSE, dir = "inside/RCM", filename = "RCM_dogfish", open_file = FALSE,
     f_name = f_name, s_name = s_name)




RCM_upW <- local({
  
  dat$nsel_block <- 1
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  #dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- "logistic"
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, c(1, 2, 4)] <- 1:6
  map_s_vul_par[1:3, 5] <- 7:9
  map_s_vul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(10, 1, 0, 1, 1, 1))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})

saveRDS(RCM_upW, "inside/RCM/RCM_upW.rds")
plot(RCM_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_upW", open_file = FALSE,
     f_name = f_name, s_name = s_name)




### Get RCMs
m_name <- c("base", "comm_dome", "HBLLS_dome", "HBLL_dome", "comm_HBLLS_dome", "comm_HBLL_dome", "dogfish")
models <- paste0("RCM_", m_name) %>%
  lapply(function(x) paste0("inside/RCM/", x, ".rds") %>% readRDS()) %>% structure(names = m_name)

do.call(compare_RCM, c(models, list(
  compare = FALSE, dir = "inside/RCM", f_name = f_name, s_name = s_name, 
  scenario = list(names = m_name),
  open_file = FALSE
)))


RCM_stitch <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, 3:4] <- 1:4
  map_s_vul_par[1:3, 5] <- 5:7
  #map_s_vul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch, "inside/RCM/RCM_stitch.rds")
plot(RCM_stitch, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch", open_file = FALSE,
     f_name = f_name, s_name = s_name)

RCM_stitch_HBLL_dome <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[, 3] <- 1:3
  map_s_vul_par[1:2, 4] <- 4:5
  map_s_vul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch_HBLL_dome, "inside/RCM/RCM_stitch_HBLL_dome.rds")
plot(RCM_stitch_HBLL_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome", open_file = FALSE,
     f_name = f_name, s_name = s_name)

RCM_stitch_dogfish <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, 3:4] <- 1:4
  map_s_vul_par[1:3, 5] <- 5:7
  map_s_vul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 1))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch_dogfish, "inside/RCM/RCM_stitch_dogfish.rds")
plot(RCM_stitch_dogfish, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_dogfish", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_HBLL_dome_incM <- local({
  
  dat$nsel_block <- 2
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[, 3] <- 1:3
  map_s_vul_par[1:2, 4] <- 4:5
  map_s_vul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  OM@cpars$M_ageArray <- array(OM@M[1], c(OM@nsim, OM@maxage + 1, OM@nyears + OM@proyears))
  OM@cpars$M_ageArray[, , seq(OM@nyears-20, OM@nyears+OM@proyears)] <- 2 * OM@M[1]
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch_HBLL_dome_incM, "inside/RCM/RCM_stitch_HBLL_dome_incM.rds")
plot(RCM_stitch_HBLL_dome_incM, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome_incM", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_upW <- local({
  
  dat$nsel_block <- 1
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  #dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- "logistic"
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[1:2, 3:4] <- 1:4
  map_s_vul_par[1:3, 5] <- 5:7
  #map_s_vul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 10, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
 
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch_upW, "inside/RCM/RCM_stitch_upW.rds")
plot(RCM_stitch_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_upW", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_HBLL_dome_upW2 <- local({
  
  dat$nsel_block <- 1
  dat$sel_block <- matrix(1, nrow(dat$Chist), ncol(dat$Chist))
  #dat$sel_block[79:nrow(dat$Chist), 1] <- 2
  
  selectivity <- c("logistic")
  
  map_s_vul_par <- matrix(NA, 3, 6)
  map_s_vul_par[, 3] <- 1:3
  map_s_vul_par[1:2, 4] <- 4:5
  map_s_vul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 10, 1, 1, 0))
  LWT$s_CAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      #map_vul_par = map_vul_par, 
      map_s_vul_par = map_s_vul_par)
})
saveRDS(RCM_stitch_HBLL_dome_upW, "inside/RCM/RCM_stitch_HBLL_dome_upW.rds")
plot(RCM_stitch_HBLL_dome_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome_upW", open_file = FALSE,
     f_name = f_name, s_name = s_name)



### Get RCMs
m_name <- c("stitch", "stitch_HBLL_dome", "stitch_dogfish", "stitch_HBLL_dome_dogfish", "stitch_HBLL_dome_incM", 
            "stitch_upW", "stitch_HBLL_dome_upW")
models <- paste0("RCM_", m_name) %>%
  lapply(function(x) paste0("inside/RCM/", x, ".rds") %>% readRDS()) %>% structure(names = m_name)

compare_RCM(models[[1]], models[[2]], models[[3]], models[[4]], models[[5]],
            compare = FALSE, dir = "inside/RCM", filename = "compare_RCM_stitch", f_name = f_name, s_name = s_name, 
            scenario = list(names = m_name),
            open_file = FALSE)

do.call(compare_RCM, c(models, 
                       list(compare = FALSE, dir = "inside/RCM", filename = "compare_RCM_stitch", f_name = f_name, s_name = s_name, 
                            scenario = list(names = m_name),
                            open_file = FALSE)))


#
#
#RCM_stitch_recdev <- local({
#  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_s_vul_par <- matrix(NA, 3, 4)
#  map_s_vul_par[1:2, 1] <- 1:2
#  map_s_vul_par[1:3, 2:3] <- 3:8
#  
#  s_selectivity <- c("logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 1, 0), s_CAA = rep(1, 4))
#  
#  OM@Perr <- c(0.6, 0.6)
#  RCM(OM, dat_stitch, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      map_vul_par = map_vul_par, map_s_vul_par = map_s_vul_par)
#})
#saveRDS(RCM_stitch_recdev, "inside/RCM/RCM_stitch_recdev.rds")
#plot(RCM_stitch_recdev, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_recdev", open_file = FALSE,
#     f_name = c("Commercial", "Recreational"), 
#     s_name = c("HBLL", "Jig Area 12", "Jig Area 13", "Dogfish"))
#
#
#RCM_stitch_incM <- local({
#  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_s_vul_par <- matrix(NA, 3, 4)
#  map_s_vul_par[1:2, 1] <- 1:2
#  map_s_vul_par[1:3, 2:3] <- 3:8
#  
#  s_selectivity <- c("logistic", "dome", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 1, 0), s_CAA = rep(1, 4))
#  
#  OM@cpars$M_ageArray <- array(OM@M[1], c(OM@nsim, OM@maxage + 1, OM@nyears + OM@proyears))
#  OM@cpars$M_ageArray[, , seq(OM@nyears-20, OM@nyears+OM@proyears)] <- 2 * OM@M[1]
#                               
#  RCM(OM, dat_stitch, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      map_vul_par = map_vul_par, map_s_vul_par = map_s_vul_par)
#})
#saveRDS(RCM_stitch_incM, "inside/RCM/RCM_stitch_incM.rds")
#plot(RCM_stitch_incM, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_incM", open_file = FALSE,
#     f_name = c("Commercial", "Recreational"), 
#     s_name = c("HBLL", "Jig Area 12", "Jig Area 13", "Dogfish"))
#
#RCM_stitch_lowH <- local({
#  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_s_vul_par <- matrix(NA, 3, 4)
#  map_s_vul_par[1:2, 1] <- 1:2
#  map_s_vul_par[1:3, 2:3] <- 3:8
#  
#  s_selectivity <- c("logistic", "dome", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 1, 0), s_CAA = rep(1, 4))
#  
#  OM@h <- c(0.4, 0.4)
#  
#  RCM(OM, dat_stitch, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      map_vul_par = map_vul_par, map_s_vul_par = map_s_vul_par)
#})
#saveRDS(RCM_stitch_lowH, "inside/RCM/RCM_stitch_lowH.rds")
#plot(RCM_stitch_lowH, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_lowH", open_file = FALSE,
#     f_name = c("Commercial", "Recreational"), 
#     s_name = c("HBLL", "Jig Area 12", "Jig Area 13", "Dogfish"))
#
#