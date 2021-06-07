
gf <- readRDS("data-raw/quillback_inside_data_jan21.rds")

generate_survey_samples <- function() {
  # Age comps
  age_samps <- function(surv = "HBLL", figure = TRUE) {
    if(grepl("HBLL", surv)) {
      HL <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_abbrev), !is.na(age)) %>% 
        group_by(year, age, survey_abbrev) %>% summarise(n = n())
      HL_trips <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_abbrev), !is.na(age)) %>%
        group_by(year, survey_abbrev) %>% 
        summarise(n = n(), n_trips = fishing_event_id %>% unique() %>% length()) %>% 
        dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    } else {
      HL <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_series_desc), !is.na(age)) %>%
        mutate(survey_abbrev = survey_series_desc) %>% group_by(year, age, survey_abbrev) %>% summarise(n = n())
      HL_trips <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_series_desc), !is.na(age)) %>%
        mutate(survey_abbrev = survey_series_desc) %>% group_by(year, survey_abbrev) %>% 
        summarise(n = n(), n_trips = fishing_event_id %>% unique() %>% length()) %>% 
        dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    } 
    stopifnot(nrow(HL) > 0)
    if(figure) {
      ggplot(HL, aes(age, n)) + geom_line() + geom_point() + facet_grid(year ~ survey_abbrev, scales = "free_y") +
        geom_label(data = HL_trips, x = 60, vjust = "inward", aes(y = max_n, label = paste0(n, " - ", n_trips))) +
        labs(x = "Age", y = "Number of samples") + geom_hline(yintercept = 0, colour = "grey") +
        ggtitle(surv) + theme_bw()
    } else {
      list(samps = HL, trips = HL_trips)
    }
  }
  len_samps <- function(surv = "HBLL", figure = TRUE) {
    if(grepl("HBLL", surv)) {
      HL <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_abbrev), !is.na(length)) %>% 
        dplyr::mutate(length_bin = cut(length, breaks = seq(10, 50), labels = FALSE) %>%
                        `-`(1) %>% `*`(1) %>% `+`(10)) %>%
        group_by(year, length_bin, survey_abbrev) %>% 
        summarise(n = n())
      HL_trips <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_abbrev), !is.na(length)) %>% 
        group_by(year, survey_abbrev) %>% 
        summarise(n = n(), n_trips = fishing_event_id %>% unique() %>% length()) %>% 
        dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    } else {
      HL <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_series_desc), !is.na(length)) %>% 
        dplyr::mutate(length_bin = cut(length, breaks = seq(10, 50), labels = FALSE) %>%
                        `-`(1) %>% `*`(1) %>% `+`(10)) %>%
        mutate(survey_abbrev = survey_series_desc) %>% group_by(year, length_bin, survey_abbrev) %>% 
        summarise(n = n())
      HL_trips <- gf[[1]] %>% dplyr::filter(grepl(surv, survey_series_desc), !is.na(length)) %>% 
        mutate(survey_abbrev = survey_series_desc) %>% group_by(year, survey_abbrev) %>% 
        summarise(n = n(), n_trips = fishing_event_id %>% unique() %>% length()) %>% 
        dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    }
    stopifnot(nrow(HL) > 0)
    if(figure) {
      ggplot(HL, aes(length_bin, n)) + geom_line() + geom_point() + facet_grid(year ~ survey_abbrev, scales = "free_y") +
        geom_label(data = HL_trips, x = 40, vjust = "inward", aes(y = max_n, label = paste0(n, " - ", n_trips))) +
        labs(x = "Length (cm)", y = "Number of samples") + 
        ggtitle(surv) + theme_bw()
    } else {
      list(samps = HL, trips = HL_trips)
    }
  }
  
  age_samps("HBLL INS")
  ggsave("inside/figures/data_s_CAA_HBLL.png", height = 8, width = 4)
  
  age_samps("Jig")
  ggsave("inside/figures/data_s_CAA_Jig.png", height = 6, width = 8)
  
  len_samps("HBLL")
  ggsave("inside/figures/data_s_CAL_HBLL.png", height = 8, width = 6)
  
  len_samps("Jig")
  ggsave("inside/figures/data_s_CAL_Jig.png", height = 6, width = 10)
  
  age_samps("HBLL INS", FALSE) %>% saveRDS("data-generated/inside_HBLL_age_samps.rds")
  age_samps("Area 12", FALSE) %>% saveRDS("data-generated/inside_Jig12_age_samps.rds")
  age_samps("Area 13", FALSE) %>% saveRDS("data-generated/inside_Jig13_age_samps.rds")
  age_samps("Area 18", FALSE) %>% saveRDS("data-generated/inside_Jig18_age_samps.rds")
  age_samps("Area 19", FALSE) %>% saveRDS("data-generated/inside_Jig19_age_samps.rds")
  
  len_samps("HBLL", FALSE) %>% saveRDS("data-generated/inside_HBLL_len_samps.rds")
  len_samps("Area 12", FALSE) %>% saveRDS("data-generated/inside_Jig12_len_samps.rds")
  len_samps("Area 13", FALSE) %>% saveRDS("data-generated/inside_Jig13_len_samps.rds")
  
  len_samps("Area 15", FALSE) %>% saveRDS("data-generated/inside_Jig15_len_samps.rds")
  len_samps("Area 16", FALSE) %>% saveRDS("data-generated/inside_Jig16_len_samps.rds")
  
  len_samps("Area 18", FALSE) %>% saveRDS("data-generated/inside_Jig18_len_samps.rds")
  len_samps("Area 19", FALSE) %>% saveRDS("data-generated/inside_Jig19_len_samps.rds")
  
  #do_ALK("HBLL")
  #ggsave("inside/figures/data_lengths_HBLL.png", height = 6, width = 6)
  #
  #do_ALK("Jig")
  #ggsave("inside/figures/data_lengths_Jig.png", height = 6, width = 6)
  
  
}
generate_survey_samples()

plot_comm_samples <- function() {
  age_samps <- function(gear = "HANDLINE", figure = TRUE) {
    HL <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age)) %>% group_by(year, age) %>% 
      summarise(n = n())
    HL_trips <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age)) %>% group_by(year) %>% 
      summarise(n = n(), n_trips = trip_id %>% unique() %>% length()) %>% 
      dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    
    if(figure) {
      ggplot(HL, aes(age, n)) + geom_hline(yintercept = 0, colour = "grey") + 
        geom_line() + geom_point() + facet_wrap(~year, scales = "free_y") +
        geom_label(data = HL_trips, x = 60, vjust = "inward", aes(y = max_n, label = paste0(n, " - ", n_trips))) +
        labs(x = "Age", y = "Number of samples") +
        ggtitle(gear) + theme_bw()
    } else {
      return(list(samps = HL, trips = HL_trips))
    }
  }
  
  len_samps <- function(gear = "HANDLINE", figure = TRUE) {
    HL <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(length)) %>% 
      dplyr::mutate(length_bin = cut(length, breaks = seq(10, 50), labels = FALSE) %>%
                      `-`(1) %>% `*`(1) %>% `+`(10)) %>%
      group_by(year, length_bin) %>% 
      summarise(n = n())
    HL_trips <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(length)) %>% group_by(year) %>% 
      summarise(n = n(), n_trips = trip_id %>% unique() %>% length()) %>% 
      dplyr::left_join(summarise(group_by(HL, year), max_n = max(n)))
    
    if(figure) {
      ggplot(HL, aes(length_bin, n)) + geom_hline(yintercept = 0, colour = "grey") + 
        geom_line() + geom_point() + facet_wrap(~year, scales = "free_y") +
        geom_label(data = HL_trips, x = 40, vjust = "inward", aes(y = max_n, label = paste0(n, " - ", n_trips))) +
        labs(x = "Length (cm)", y = "Number of samples") + 
        ggtitle(gear) + theme_bw()
    } else {
      return(list(samps = HL, trips = HL_trips))
    }
  }
  
  do_ALK <- function(gear = "HANDLINE", figure = TRUE) {
    #ALK <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age), !is.na(length)) %>% 
    #  dplyr::mutate(length_bin = cut(length, breaks = seq(12.5, 50, 2.5), labels = FALSE) %>%
    #                  `-`(1) %>% `*`(2.5) %>% `+`(12.5)) %>%
    #  group_by(year, age, length_bin) %>% summarise(n = n()) %>% group_by(year, length_bin) %>%
    #  dplyr::mutate(pal = n/sum(n)) %>% reshape2::acast(list("year", "age", "length_bin"), value.var = "pal", fill = 0)
    #
    #len_matrix <- gf[[2]] %>% dplyr::filter(gear_desc == gear, is.na(age), !is.na(length)) %>% 
    #  dplyr::mutate(length_bin = cut(length, breaks = seq(12.5, 50, 2.5), labels = FALSE) %>%
    #                  `-`(1) %>% `*`(2.5) %>% `+`(12.5)) %>%
    #  group_by(year, length_bin) %>% summarise(n = n()) %>% 
    #  reshape2::acast(list("year", "length_bin"), value.var = "n", fill = 0)
    
    ALK <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age), !is.na(length)) %>% 
      dplyr::mutate(length_bin = cut(length, breaks = seq(10, 50), labels = FALSE) %>%
                      `-`(1) %>% `*`(1) %>% `+`(10)) %>%
      group_by(year, age, length_bin) %>% summarise(n = n()) %>% group_by(year, length_bin) %>%
      dplyr::mutate(pal = n/sum(n)) %>% reshape2::acast(list("year", "age", "length_bin"), value.var = "pal", fill = 0)
    
    len_matrix <- gf[[2]] %>% dplyr::filter(gear_desc == gear, is.na(age), !is.na(length)) %>% 
      dplyr::mutate(length_bin = cut(length, breaks = seq(10, 50), labels = FALSE) %>%
                      `-`(1) %>% `*`(1) %>% `+`(10)) %>%
      group_by(year, length_bin) %>% summarise(n = n()) %>% 
      reshape2::acast(list("year", "length_bin"), value.var = "n", fill = 0)
    
    yy <- dplyr::intersect(dimnames(ALK)[[1]], rownames(len_matrix))
    stopifnot(length(yy) > 0)
    
    age_matrix <- lapply(yy, function(y) {
      yind <- dimnames(ALK)[[1]] == y
      yind2 <- rownames(len_matrix) == y
      
      len <- dplyr::intersect(dimnames(ALK)[[3]], colnames(len_matrix))
      
      lind <- dimnames(ALK)[[3]] %in% len
      lind2 <- colnames(len_matrix) %in% len
      ALK[yind, , ][, lind] %*% len_matrix[yind2, lind2]
    }) %>% do.call(cbind, .) %>% t() %>%
      structure(dimnames = list(year = yy, age = dimnames(ALK)[[2]]))
    
    age_df <- reshape2::melt(age_matrix, value.name = "n")
    age_samps <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age)) %>% group_by(year, age) %>% 
      summarise(n = n()) %>% rbind(age_df) %>% group_by(year, age) %>% summarise(n = sum(n))
    
    trips <- gf[[2]] %>% dplyr::filter(gear_desc == gear, !is.na(age) | !is.na(length)) %>% group_by(year) %>% 
      summarise(n = n(), n_trips = trip_id %>% unique() %>% length()) %>% 
      dplyr::left_join(summarise(group_by(age_samps, year), max_n = max(n)))
    
    if(figure) {
      ggplot(age_samps, aes(age, n)) + geom_hline(yintercept = 0, colour = "grey") + 
        geom_line() + geom_point() + facet_wrap(~year, scales = "free_y") +
        geom_label(data = trips, x = 60, vjust = "inward", hjust = "inward", aes(y = max_n, label = paste0(n, " - ", n_trips))) +
        labs(x = "Age", y = "Number of samples") + ggtitle(gear) + theme_bw()
    } else {
      return(list(samps = age_samps, trips = trips))
    }
  }
  
  age_samps("HANDLINE")
  ggsave("inside/figures/data_CAA_HL.png", height = 6, width = 6)
  
  age_samps("LONGLINE")
  ggsave("inside/figures/data_CAA_LL.png", height = 2, width = 4)
  
  age_samps("TROLL")
  ggsave("inside/figures/data_CAA_TR.png", height = 4, width = 4)
  
  len_samps("HANDLINE")
  ggsave("inside/figures/data_CAL_HL.png", height = 5, width = 6)
  
  len_samps("LONGLINE")
  ggsave("inside/figures/data_CAL_LL.png", height = 4, width = 4)
  
  len_samps("TROLL")
  ggsave("inside/figures/data_CAL_TR.png", height = 3, width = 4)
  
  do_ALK("HANDLINE")
  ggsave("inside/figures/data_CAA_ALK_HL.png", height = 6, width = 6)
  
  #do_ALK("LONGLINE")
  #ggsave("inside/figures/data_CAA_LL.png", height = 2, width = 4)
  
  do_ALK("TROLL")
  ggsave("inside/figures/data_CAA_ALK_TR.png", height = 4, width = 4)
  
  do_ALK("HANDLINE", FALSE) %>% saveRDS("data-generated/inside_handline_CAA.rds")
  do_ALK("TROLL", FALSE) %>% saveRDS("data-generated/inside_troll_CAA.rds")
  len_samps("LONGLINE", FALSE) %>% saveRDS("data-generated/inside_longline_CAL.rds")
  len_samps("HANDLINE", FALSE) %>% saveRDS("data-generated/inside_handline_CAL.rds")
  
  invisible()
}

plot_comm_samples()
