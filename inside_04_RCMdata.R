
library(SAMtool)

make_catch <- function(Year = 1918:2020, rec_start = 1945, 
                       rec_hist = c("linear", "curvilinear")) {
  rec_hist <- match.arg(rec_hist)
  
  # Commercial will be 1st fleet, mostly/all H&L
  comm <- read.csv("data-raw/Catch-History-424-noStrat.csv") %>% dplyr::filter(year %in% Year) %>%
    group_by(year) %>% summarise(catch = sum(X1))
  
  comm_out <- comm$catch
  
  rec <- read.csv("data-raw/insideQB_total_Rec_Catch_Year.csv")
  
  rec_out <- rep(0.01, length(Year))
  if(rec_hist == "curvilinear") {
    rec_out[Year > rec_start & Year < rec$YEAR[1]] <- rec$tonnes[1]/(rec$YEAR[1]-1 - seq(rec_start, rec$YEAR[1]-2))
  } else {
    ramp <- rec$YEAR[1] - 1 - seq(rec$YEAR[1] - 2, rec_start)
    rec_out[Year > rec_start & Year < rec$YEAR[1]] <- ramp * rec$tonnes[1]/length(ramp)
  }
  rec_out[Year >= rec$YEAR[1]] <- rec$tonnes
  return(cbind(comm_out, rec_out) %>% structure(dimnames = list(Year, NULL)))
}

make_CAA <- function(Year = 1918:2020, maxage = 60, ESS = 30) {
  HL <- readRDS("data-generated/inside_handline_CAA.rds")
  samps <- mutate(HL$samps, age = pmin(age, maxage)) %>% group_by(year, age) %>% summarise(tot = sum(n))
  
  CAA_out <- array(0, c(length(Year), maxage + 1, 2))
  
  for(i in 1:nrow(samps)) {
    yy <- match(samps$year[i], Year)
    aa <- match(samps$age[i], 0:maxage)
    CAA_out[yy, aa, 1] <- samps$tot[i]
  }
  
  CAA_ESS <- matrix(0, length(Year), 2)
  CAA_ESS[match(HL$trips$year, Year), 1] <- HL$trips$n_trips
  
  return(list(CAA = CAA_out, CAA_ESS = CAA_ESS))
}

make_CAL <- function(Year = 1918:2020, length_bin = 13:49) {
  #LL <- readRDS("data-generated/inside_longline_CAL.rds")
  #length_bin <- seq(min(LL$samps$length_bin), max(LL$samps$length_bin))
  #CAL_out <- array(0, c(length(Year), length(length_bin), 2))
  #
  #for(i in 1:nrow(LL$samps)) {
  #  yy <- match(LL$samps$year[i], Year)
  #  aa <- match(LL$samps$length_bin[i], length_bin)
  #  CAL_out[yy, aa, 1] <- LL$samps$n[i]
  #}
  
  LL <- readRDS("data-generated/inside_handline_CAL.rds")
  CAL_out <- array(0, c(length(Year), length(length_bin), 2))
  
  for(i in 1:nrow(LL$samps)) {
    yy <- match(LL$samps$year[i], Year)
    aa <- match(LL$samps$length_bin[i], length_bin)
    CAL_out[yy, aa, 1] <- LL$samps$n[i]
  }  
  
  CAL_ESS <- matrix(0, length(Year), 2)
  CAL_ESS[match(LL$trips$year, Year), 1] <- LL$trips$n_trips
  
  return(list(CAL = CAL_out, CAL_ESS = CAL_ESS, length_bin = length_bin))
}

make_index <- function(Year = 1918:2020, maxage = 60) {
  ind <- read.csv("data-raw/quillback_survey_index.csv")
  J12 <- dplyr::filter(ind, survey_series_desc == "Jig Survey - 4B Stat Area 12")
  J13 <- dplyr::filter(ind, survey_series_desc == "Jig Survey - 4B Stat Area 13")
  Dog <- dplyr::filter(ind, survey_series_desc == "Strait of Georgia Dogfish Longline")
  
  make_index2 <- function(x, SE = FALSE) {
    out <- rep(NA_real_, length(Year))
    if(SE) {
      out[Year %in% x$year] <- x$re
    } else {
      out[Year %in% x$year] <- x$biomass
    }
    return(out)
  }
  
  HBLL_samps <- readRDS("data-generated/inside_HBLL_age_samps.rds")
  J12_samps <- readRDS("data-generated/inside_Jig12_age_samps.rds")
  J13_samps <- readRDS("data-generated/inside_Jig13_age_samps.rds")
  
  make_IAA <- function(x, y) {
    samps <- mutate(x, age = pmin(age, maxage)) %>% group_by(year, age) %>% summarise(tot = sum(n))
    IAA <- matrix(0, length(Year), maxage + 1)
    for(i in 1:nrow(samps)) {
      yy <- match(samps$year[i], Year)
      aa <- match(samps$age[i], 0:maxage)
      
      ysamp <- match(samps$year[i], y$year)
      IAA[yy, aa] <- samps$tot[i] #* y$n_trips[ysamp]/y$n[ysamp]
    }
    IAA_ESS <- rep(0, length(Year))
    IAA_ESS[match(y$year, Year)] <- y$n_trips
    
    return(list(IAA = IAA, IAA_ESS = IAA_ESS))
  }
  
  HBLL_len_samps <- readRDS("data-generated/inside_HBLL_len_samps.rds")
  J12_len_samps <- readRDS("data-generated/inside_Jig12_len_samps.rds")
  J13_len_samps <- readRDS("data-generated/inside_Jig13_len_samps.rds")
  
  make_IAL <- function(x, y, length_bin) {
    samps <- x %>% group_by(year, length_bin) %>% summarise(tot = sum(n)) %>% 
      dplyr::filter(!is.na(length_bin))
    IAL <- matrix(0, length(Year), length(length_bin))
    for(i in 1:nrow(samps)) {
      yy <- match(samps$year[i], Year)
      aa <- match(samps$length_bin[i], length_bin)
      IAL[yy, aa] <- samps$tot[i]
    }
    IAL_ESS <- rep(0, length(Year))
    IAL_ESS[match(y$year, Year)] <- y$n_trips
    
    return(list(IAL = IAL, IAL_ESS = IAL_ESS))
  }
  
  HBLL_N <- dplyr::filter(ind, survey_abbrev == "HBLL INS N")
  HBLL_S <- dplyr::filter(ind, survey_abbrev == "HBLL INS S")
  
  HBLL_INS <- readRDS("data-generated/hbll-inside-joint-index.rds") %>% rename(biomass = est, re = se)
  
  I_out <- lapply(list(HBLL_N, HBLL_S, HBLL_INS, J12, J13, Dog), make_index2) %>% do.call(cbind, .) %>% 
    structure(dimnames = list(Year, NULL))
  I_SE <- lapply(list(HBLL_N, HBLL_S, HBLL_INS, J12, J13, Dog), make_index2, SE = TRUE) %>% do.call(cbind, .) %>% 
    structure(dimnames = list(Year, NULL))
  
  HBLL_N_samps <- dplyr::filter(HBLL_samps$samps, survey_abbrev == "HBLL INS N")
  HBLL_S_samps <- dplyr::filter(HBLL_samps$samps, survey_abbrev == "HBLL INS S")
  
  IAA <- Map(make_IAA, x = list(HBLL_N_samps, HBLL_S_samps, HBLL_samps$samps, J12_samps$samps, J13_samps$samps), 
             y = list(dplyr::filter(HBLL_samps$trips, survey_abbrev == "HBLL INS N"), 
                      dplyr::filter(HBLL_samps$trips, survey_abbrev == "HBLL INS S"), 
                      HBLL_samps$trips,
                      J12_samps$trips, 
                      J13_samps$trips))
  
  IAA_out <- array(0, c(length(Year), maxage + 1, 6))
  IAA_out[,, 1:5] <- lapply(IAA, getElement, "IAA") %>% simplify2array()
  
  IAA_ESS <- matrix(0, length(Year), 6)
  IAA_ESS[, 1:5] <- sapply(IAA, getElement, "IAA_ESS")
  
  length_bin <- 13:49
  IAL <- Map(make_IAL, x = list(dplyr::filter(HBLL_len_samps$samps, survey_abbrev == "HBLL INS N"), 
                                dplyr::filter(HBLL_len_samps$samps, survey_abbrev == "HBLL INS S"), 
                                HBLL_len_samps$samps, J12_len_samps$samps, J13_len_samps$samps), 
             y = list(dplyr::filter(HBLL_len_samps$trips, survey_abbrev == "HBLL INS N"), 
                      dplyr::filter(HBLL_len_samps$trips, survey_abbrev == "HBLL INS S"), 
                      HBLL_len_samps$trips,
                      J12_len_samps$trips, 
                      J13_len_samps$trips),
             MoreArgs = list(length_bin = length_bin))
  
  IAL_out <- array(0, c(length(Year), length(length_bin), 6))
  IAL_out[,, 1:5] <- lapply(IAL, getElement, "IAL") %>% simplify2array()
  
  IAL_ESS <- matrix(0, length(Year), 6)
  IAL_ESS[, 1:5] <- sapply(IAL, getElement, "IAL_ESS")
  
  list(Index = I_out, I_sd = I_SE, IAA = IAA_out, IAA_ESS = IAA_ESS, IAL = IAL_out, IAL_ESS = IAL_ESS)
}


make_RCMdata <- function() {
  dat <- new("RCMdata")
  dat@Chist <- make_catch()
  
  CAA <- make_CAA()
  dat@CAA <- CAA$CAA
  dat@CAA_ESS <- CAA$CAA_ESS
  
  CAL <- make_CAL()
  dat@CAL <- CAL$CAL
  dat@CAL_ESS <- CAL$CAL_ESS
  dat@length_bin <- CAL$length_bin
  
  Index <- make_index()
  dat@Index <- Index$Index
  dat@I_sd <- Index$I_sd
  dat@IAA <- Index$IAA
  dat@IAA_ESS <- Index$IAA_ESS
  dat@IAL <- Index$IAL
  dat@IAL_ESS <- Index$IAL_ESS
  
  dat@I_units <- rep(0, ncol(dat@Index))
  
  return(dat)
}

dat <- make_RCMdata()
saveRDS(dat, file = "inside/OM/OM_dat.rds")


