


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
  samps <- mutate(HL$samps, age2 = pmin(age, maxage)) %>% group_by(year, age2) %>% summarise(tot = sum(n))
  
  CAA_out <- array(0, c(length(Year), maxage + 1, 2))
  
  for(i in 1:nrow(samps)) {
    yy <- match(samps$year[i], Year)
    aa <- match(samps$age2[i], 0:maxage)
    ysamp <- match(samps$year[i], HL$trips$year)
    CAA_out[yy, aa, 1] <- samps$tot[i] * HL$trips$n_trips[ysamp]/HL$trips$n[ysamp]
  }
  return(CAA_out)
}

make_CAL <- function(Year = 1918:2020) {
  LL <- readRDS("data-generated/inside_longline_CAL.rds")
  length_bin <- seq(min(LL$samps$length_bin), max(LL$samps$length_bin))
  CAL_out <- array(0, c(length(Year), length(length_bin), 2))
  
  for(i in 1:nrow(LL$samps)) {
    yy <- match(LL$samps$year[i], Year)
    aa <- match(LL$samps$length_bin[i], length_bin)
    CAL_out[yy, aa, 1] <- LL$samps$n[i]
  }
  return(list(CAL = CAL_out, length_bin = length_bin))
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
  
  make_CAA <- function(x, y) {
    samps <- mutate(x, age2 = pmin(age, maxage)) %>% group_by(year, age2) %>% summarise(tot = sum(n))
    CAA <- matrix(0, length(Year), maxage + 1)
    for(i in 1:nrow(samps)) {
      yy <- match(samps$year[i], Year)
      aa <- match(samps$age2[i], 0:maxage)
      
      ysamp <- match(samps$year[i], y$year)
      CAA[yy, aa] <- samps$tot[i] * y$n_trips[ysamp]/y$n[ysamp]
    }
    return(CAA)
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
  
  SAA <- Map(make_CAA, x = list(HBLL_N_samps, HBLL_S_samps, HBLL_samps$samps, J12_samps$samps, J13_samps$samps), 
             y = list(dplyr::filter(HBLL_samps$trips, survey_abbrev == "HBLL INS N"), 
                      dplyr::filter(HBLL_samps$trips, survey_abbrev == "HBLL INS S"), 
                      HBLL_samps$trips,
                      J12_samps$trips, 
                      J13_samps$trips))
  
  SAA_out <- array(0, c(length(Year), maxage + 1, 6))
  SAA_out[,, 1:5] <- simplify2array(SAA)
  
  list(Index = I_out, I_sd = I_SE, s_CAA = SAA_out)
}


make_data_list <- function() {
  
  dat <- list()
  dat$Chist <- make_catch()
  dat$CAA <- make_CAA()
  dat <- c(dat, make_CAL(), make_index())
  
  return(dat)
}

dat <- make_data_list()
saveRDS(dat, file = "inside/OM/OM_dat.rds")


