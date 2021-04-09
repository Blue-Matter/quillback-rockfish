
library(SAMtool)

Data <- local({
  RCM <- readRDS("inside/RCM/RCM_stitch.rds")
  Data <- RCM@OM@cpars$Data
  Data@Mort <- 0.057
  Data@steep <- 0.67
  Data@sigmaR <- 0.1
  Data@vbLinf <- 39.2
  Data@vbK <- 0.1
  Data@vbt0 <- -3.42
  Data@wla <- exp(-11.05)
  Data@wlb <- 3.06
  
  Data@L50 <- 39.2 * (1 - exp(-0.1 * (10 + 3.42)))
  
  Data
})

# Stitched survey
Assess <- DD_SS(Data = Data, AddInd = 3:5, dep = 1, start = list(tau = 0.05))
plot(Assess, dir = "inside/RCM", filename = "DD_SS", ret_yr = 7, open_file = FALSE)

Assess <- DD_SS(Data = Data, AddInd = 3:6, dep = 1, start = list(tau = 0.05))
plot(Assess, dir = "inside/RCM", filename = "DD_SS_dogfish", ret_yr = 7, open_file = FALSE)

Assess <- DD_SS(Data = Data, AddInd = c(3, 6), dep = 1, start = list(tau = 0.1))
plot(Assess, dir = "inside/RCM", filename = "DD_SS_dogfish_exJig", ret_yr = 7, open_file = FALSE)






Assess@TMB_report$Ceqpred

Assess <- DD_TMB(Data = Data, AddInd = 3, dep = 0.9)
plot(Assess)

Assess <- DD_TMB(Data = Data, AddInd = 3:5, dep = 0.9)
plot(Assess)

Assess <- DD_TMB(Data = Data, AddInd = 3:6, dep = 0.9)
plot(Assess)

Assess <- DD_TMB(Data = Data, AddInd = c(1:2, 4:5), dep = 0.9)
plot(Assess)

Assess <- DD_TMB(Data = Data, AddInd = c(1:2, 4:6), dep = 0.9)
plot(Assess)






Assess <- SP_SS(Data = Data, AddInd = 3, dep = 0.9, start = list(tau = 0.05, r_prior = c(0.091, 0.05)))
plot(Assess)

Assess <- SP(Data = Data, AddInd = 3:5, dep = 0.9, start = list(tau = 0.05, r_prior = c(0.091, 0.05)))
plot(Assess)

Assess <- SP(Data = Data, AddInd = 3:6, dep = 0.9, start = list(tau = 0.05, r_prior = c(0.091, 0.05)))
plot(Assess)

Assess <- SP(Data = Data, AddInd = c(1:2, 4:5), dep = 0.9, start = list(tau = 0.05, r_prior = c(0.091, 0.05)))
plot(Assess)

Assess <- SP(Data = Data, AddInd = c(1:2, 4:6), dep = 0.9, start = list(tau = 0.05, r_prior = c(0.091, 0.05)))
plot(Assess)
