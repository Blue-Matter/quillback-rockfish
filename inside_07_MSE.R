
library(SAMtool)

CC_55 <- function(x, Data, reps) {
  Rec <- new("Rec")
  Rec@TAC <- rep(55, reps)
  return(Rec)
}
class(CC_55) <- "MP"


m_name <- c("stitch", "stitch_HBLL_dome", "stitch_dogfish", "stitch_HBLL_dome_incM", 
            "stitch_upW", "stitch_HBLL_dome_upW")

models <- paste0("RCM_", m_name) %>%
  lapply(function(x) paste0("inside/RCM/", x, ".rds") %>% readRDS()) %>% structure(names = m_name)

#for(i in 1:length(m_name)) {
  
for(i in 1:4) {
  OM <- models[[i]]@OM
  OM@interval <- 200
  MSE <- runMSE(OM, MPs = c("NFref", "CC_55"), extended = TRUE)
  saveRDS(MSE, file = paste0("inside/MSE/MSE_", m_name[i], ".rds"))
}

#tictoc::tic()
#MSE <- runMSE(RCM@OM, MPs = c("NFref", "CC_55"), extended = TRUE)
#tictoc::toc()
#
#saveRDS(MSE, file = "inside/MSE/MSE_comm_break_extended.rds")

#saveRDS(RCM@OM, "QB_OM.rds")
#
#library(MSEtool)
#OM <- readRDS("QB_OM.rds")
#debug(MSEtool:::popdynCPP)
#Hist <- runMSE(OM, Hist = TRUE)
#

