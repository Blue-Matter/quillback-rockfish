
library(SAMtool)

OM <- readRDS("inside/OM/ins_OM.rds")
dat <- readRDS("inside/OM/OM_dat.rds")

f_name <- c("Commercial", "Recreational")
s_name <- c("HBLL N", "HBLL S", "HBLL INS", "Jig Area 12", "Jig Area 13", "Dogfish")




#RCM_base <- local({
#  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:2, c(1, 2, 4)] <- 1:6
#  map_ivul_par[1:3, 5] <- 7:9
#  
#  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      map_vul_par = map_vul_par, map_ivul_par = map_ivul_par)
#})
#
#saveRDS(RCM_base, "inside/RCM/RCM_base.rds")
#plot(RCM_base, compare = FALSE, dir = "inside/RCM", filename = "RCM", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#
#RCM_comm_break <- local({
#  
#  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
#  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
#  
#  #map_vul_par <- matrix(c(1, 2, 3), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:2, c(1, 2, 4)] <- 1:6
#  map_ivul_par[1:3, 5] <- 7:9
#  
#  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      #map_vul_par = map_vul_par, 
#      map_ivul_par = map_ivul_par)
#})
#saveRDS(RCM_comm_break, "inside/RCM/RCM_comm_break.rds")
#plot(RCM_comm_break, compare = FALSE, dir = "inside/RCM", filename = "RCM_comm_break", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#
#
#
#RCM_HBLLS_dome <- local({
#  
#  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
#  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
#  
#  #map_vul_par <- matrix(c(1, 2, 3), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:2, 1:2] <- 1:4
#  map_ivul_par[3, 2] <- 5
#  map_ivul_par[1:2, 4] <- 6:7
#  map_ivul_par[1:3, 5] <- 8:10
#  
#  s_selectivity <- c("logistic", "dome", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      #map_vul_par = map_vul_par, 
#      map_ivul_par = map_ivul_par)
#})
#saveRDS(RCM_HBLLS_dome, "inside/RCM/RCM_HBLLS_dome.rds")
#plot(RCM_HBLLS_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_HBLLS_dome", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#RCM_HBLL_dome <- local({
#  
#  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
#  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
#  
#  selectivity <- c("logistic", "logistic")
#  
#  #map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  #selectivity <- c("logistic", "logistic")
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:3, 1:2] <- 1:6
#  map_ivul_par[1:2, 4] <- 7:8
#  map_ivul_par[1:3, 5] <- 9:11
#  
#  s_selectivity <- c("dome", "dome", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 0))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      map_vul_par = map_vul_par, map_ivul_par = map_ivul_par)
#})
#
#saveRDS(RCM_HBLL_dome, "inside/RCM/RCM_HBLL_dome.rds")
#plot(RCM_HBLL_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_HBLL_dome", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#
#
#
#
#RCM_dogfish <- local({
#  
#  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
#  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
#  
#  
#  selectivity <- c("logistic", "logistic")
#  
#  #map_vul_par <- matrix(c(1, 2, NA), 3, 2)
#  
#  selectivity <- c("logistic", "logistic")
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:2, c(1, 2, 4)] <- 1:6
#  map_ivul_par[1:3, 5] <- 7:9
#  map_ivul_par[1:2, 6] <- 1:2
#  
#  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(1, 1, 0, 1, 1, 1))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      #map_vul_par = map_vul_par, 
#      map_ivul_par = map_ivul_par)
#})
#
#saveRDS(RCM_dogfish, "inside/RCM/RCM_dogfish.rds")
#plot(RCM_dogfish, compare = FALSE, dir = "inside/RCM", filename = "RCM_dogfish", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#
#
#
#RCM_upW <- local({
#  
#  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
#  #dat@sel_block[79:nrow(dat@Chist), 1] <- 2
#  
#  selectivity <- "logistic"
#  
#  map_ivul_par <- matrix(NA, 3, 6)
#  map_ivul_par[1:2, c(1, 2, 4)] <- 1:6
#  map_ivul_par[1:3, 5] <- 7:9
#  map_ivul_par[1:2, 6] <- 1:2
#  
#  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
#  
#  ESS <- c(1e5, 1e5)
#  
#  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(10, 1, 0, 1, 1, 1))
#  LWT$IAA <- LWT$Index
#  
#  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
#      selectivity = selectivity, s_selectivity = s_selectivity, 
#      #map_vul_par = map_vul_par, 
#      map_ivul_par = map_ivul_par)
#})
#
#saveRDS(RCM_upW, "inside/RCM/RCM_upW.rds")
#plot(RCM_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_upW", open_file = FALSE,
#     f_name = f_name, s_name = s_name)
#
#
#
#
#### Get RCMs
#m_name <- c("base", "comm_dome", "HBLLS_dome", "HBLL_dome", "comm_HBLLS_dome", "comm_HBLL_dome", "dogfish")
#models <- paste0("RCM_", m_name) %>%
#  lapply(function(x) paste0("inside/RCM/", x, ".rds") %>% readRDS()) %>% structure(names = m_name)
#
#do.call(compare_RCM, c(models, list(
#  compare = FALSE, dir = "inside/RCM", f_name = f_name, s_name = s_name, 
#  scenario = list(names = m_name),
#  open_file = FALSE
#)))


RCM_stitch <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  
  selectivity <- c("logistic", "dome")
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[1:2, 3:4] <- 1:4
  map_ivul_par[1:3, 5] <- 5:7
  #map_ivul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$IAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch, "inside/RCM/RCM_stitch.rds")
plot(RCM_stitch, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch", open_file = FALSE,
     f_name = f_name, s_name = s_name)



RCM_stitch_HBLL_dome <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  
  selectivity <- c("logistic", "dome")
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[, 3] <- 1:3
  map_ivul_par[1:2, 4] <- 4:5
  map_ivul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$IAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_HBLL_dome, "inside/RCM/RCM_stitch_HBLL_dome.rds")
plot(RCM_stitch_HBLL_dome, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome", open_file = FALSE,
     f_name = f_name, s_name = s_name)

RCM_stitch_dogfish <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  
  selectivity <- c("logistic", "dome")
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[1:2, 3:4] <- 1:4
  map_ivul_par[1:3, 5] <- 5:7
  map_ivul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 1))
  LWT$IAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_dogfish, "inside/RCM/RCM_stitch_dogfish.rds")
plot(RCM_stitch_dogfish, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_dogfish", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_HBLL_dome_incM <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  
  selectivity <- c("logistic", "dome")
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[, 3] <- 1:3
  map_ivul_par[1:2, 4] <- 4:5
  map_ivul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$IAA <- LWT$Index
  
  OM@cpars$M_ageArray <- array(OM@M[1], c(OM@nsim, OM@maxage + 1, OM@nyears + OM@proyears))
  OM@cpars$M_ageArray[, , seq(OM@nyears-20, OM@nyears+OM@proyears)] <- 2 * OM@M[1]
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_HBLL_dome_incM, "inside/RCM/RCM_stitch_HBLL_dome_incM.rds")
plot(RCM_stitch_HBLL_dome_incM, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome_incM", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_upW <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  #map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[1:2, 3:4] <- 1:4
  map_ivul_par[1:3, 5] <- 5:7
  #map_ivul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 10, 1, 1, 0))
  LWT$IAA <- LWT$Index
 
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_upW, "inside/RCM/RCM_stitch_upW.rds")
plot(RCM_stitch_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_upW", open_file = FALSE,
     f_name = f_name, s_name = s_name)


RCM_stitch_HBLL_dome_upW <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  selectivity <- c("logistic", "logistic")
  
  map_vul_par <- matrix(c(1, 2, NA), 3, 2)
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[, 3] <- 1:3
  map_ivul_par[1:2, 4] <- 4:5
  map_ivul_par[1:3, 5] <- 6:8
  
  s_selectivity <- c("logistic", "logistic", "dome", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(1, 1), CAL = c(0, 0), Index = c(0, 0, 10, 1, 1, 0))
  LWT$IAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_HBLL_dome_upW, "inside/RCM/RCM_stitch_HBLL_dome_upW.rds")
plot(RCM_stitch_HBLL_dome_upW, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_HBLL_dome_upW", open_file = FALSE,
     f_name = f_name, s_name = s_name)


# Use HL lengths instead of ages
# Sub-sample HBLL ages (25%)
RCM_stitch_VOI <- local({
  
  dat@sel_block <- matrix(1, nrow(dat@Chist), ncol(dat@Chist))
  dat@sel_block[79:nrow(dat@Chist), 1] <- 2
  
  # Sub-sample HBLL ages
  HBLL_samps <- readRDS("data-generated/inside_HBLL_age_samps.rds")
  N_annual <- HBLL_samps$samps %>% group_by(year) %>% summarise(n = round(0.25 * sum(n)))
  
  set.seed(234)
  for(i in 1:nrow(N_annual)) {
    
    samps_y <- dplyr::filter(HBLL_samps$samps, year == N_annual$year[i])
    samps <- sample(samps_y$age, N_annual$n[i], replace = TRUE, prob = samps_y$n)
    samps_plus <- ifelse(samps > OM@maxage, OM@maxage, samps)
    
    yind <- match(N_annual$year[i], 1918:2020)
    
    dat@IAA[yind, , 3] <- 0
    for(ii in 1:length(samps)) {
      dat@IAA[yind, samps[ii] + 1, 3] <- dat@IAA[yind, samps[ii] + 1, 3] + 1
    }
    
  }
  dat@IAA_ESS[match(N_annual$year, 1918:2020), 3] <- N_annual$n
  
  map_vul_par <- matrix(c(1:2, NA, 3:5), 3, 2)
  
  selectivity <- c("logistic", "dome")
  
  map_ivul_par <- matrix(NA, 3, 6)
  map_ivul_par[1:2, 3:4] <- 1:4
  map_ivul_par[1:3, 5] <- 5:7
  #map_ivul_par[1:2, 6] <- 1:2
  
  s_selectivity <- c("logistic", "logistic", "logistic", "logistic", "dome", "logistic")
  
  ESS <- c(1e5, 1e5)
  
  LWT <- list(IAL = rep(0, 6), CAA = c(0, 1), CAL = c(1, 0), Index = c(0, 0, 1, 1, 1, 0))
  LWT$IAA <- LWT$Index
  
  RCM(OM, dat, condition = "catch2", LWT = LWT, ESS = ESS,
      selectivity = selectivity, s_selectivity = s_selectivity, 
      map_vul_par = map_vul_par, 
      map_ivul_par = map_ivul_par)
})
saveRDS(RCM_stitch_VOI, "inside/RCM/RCM_stitch_VOI.rds")
plot(RCM_stitch_VOI, compare = FALSE, dir = "inside/RCM", filename = "RCM_stitch_VOI", open_file = FALSE,
     f_name = f_name, s_name = s_name)

### Get RCMs
m_name <- c("stitch", "stitch_HBLL_dome", "stitch_dogfish", #"stitch_HBLL_dome_dogfish", 
            "stitch_HBLL_dome_incM", 
            "stitch_upW", "stitch_HBLL_dome_upW", "stitch_VOI")
models <- paste0("RCM_", m_name) %>%
  lapply(function(x) paste0("inside/RCM/", x, ".rds") %>% readRDS()) %>% structure(names = m_name)

do.call(compare_RCM, c(models, 
                       list(compare = FALSE, dir = "inside/RCM", filename = "compare_RCM_stitch", f_name = f_name, s_name = s_name, 
                            scenario = list(names = m_name),
                            open_file = FALSE)))


