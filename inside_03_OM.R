
library(SAMtool)

make_OM <- function(nsim = 48, proyears = 100, FYr = 1918, maxage = 60) {
  Stock <- new("Stock")
  Stock@maxage <- maxage
  Stock@R0 <- 3000
  Stock@M <- c(0.057, 0.057)
  Stock@h <- c(0.67, 0.67)
  Stock@SRrel <- 1L
  Stock@Perr <- c(0.4, 0.4)
  Stock@AC <- c(0, 0)
  Stock@Linf <- c(39.2, 39.2)
  Stock@K <- c(0.1, 0.1)
  Stock@t0 <- c(-3.42, -3.42)
  Stock@LenCV <- c(0.1, 0.1)
  Stock@a <- exp(-11.05)
  Stock@b <- 3.06
  Stock@Msd <- Stock@Linfsd <- Stock@Ksd <- c(0, 0)
  Stock@Size_area_1 <- Stock@Frac_area_1 <- Stock@Prob_staying <- c(0.5, 0.5)
  Stock@Fdisc <- c(0, 0)
  
  # Placeholders
  Stock@D <- c(0.9, 0.9)
  Stock@L50 <- Stock@L50_95 <- c(0, 0)
  
  Fleet <- new("Fleet")
  Fleet@CurrentYr <- 2020
  
  Fleet@nyears <- length(FYr:Fleet@CurrentYr)
  Fleet@EffYears <- FYr:Fleet@CurrentYr
  Fleet@EffLower <- rep(1e-4, Fleet@nyears)
  Fleet@EffUpper <- rep(1e-3, Fleet@nyears)
  Fleet@Esd <- Fleet@qinc <- Fleet@qcv <- c(0, 0)
  
  Fleet@L5 <- c(20, 20)
  Fleet@LFS <- c(30, 30)
  Fleet@Vmaxlen <- c(1, 1)
  Fleet@isRel <- FALSE
  Fleet@DR <- c(0, 0)
  Fleet@Spat_targ <- c(1, 1)
  Fleet@MPA <- FALSE
  
  OM <- new("OM", Stock = Stock, Fleet = Fleet, Obs = Generic_Obs, Imp = Perfect_Imp)
  OM@nsim <- nsim
  OM@proyears <- proyears
  OM@interval <- 2
  OM@maxF <- 3
  
  # Maturity
  age <- 0:Stock@maxage
  intercept <- -4.3104194 + 1.7128071
  slope <- 0.4129927 - 0.1481508
  linear_predictors <- intercept + slope * age # Estimated from binomial GLM with cauchit link
  Mat_age <- ifelse(age <= 4, 0, pcauchy(linear_predictors))
  
  OM@cpars$Mat_age <- Mat_age %>% array(c(OM@maxage + 1, OM@nyears + OM@proyears, OM@nsim)) %>% aperm(c(3, 1, 2))
  
  return(OM)
}

OM <- make_OM()
saveRDS(OM, file = "inside/OM/ins_OM.rds")

OM <- make_OM(nsim = 2)
saveRDS(OM, file = "inside/OM/ins_OM_2sim.rds")
