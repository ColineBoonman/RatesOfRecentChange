######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

climate_stats = function(clim_norm, A_B_C_special_sub.classes = FALSE, clim.resume_verbose = TRUE, class.nr = FALSE) {
  if (sum(is.na(clim_norm$Tm)) > 0 | sum(is.na(clim_norm$P)) > 
      0) 
    print("12 monthly values of temp. and prec. required!", 
          quote = F)
  else {
    T_warm_month <- max(clim_norm$Tm)
    T_cold_month <- min(clim_norm$Tm)
    T_avg <- mean(clim_norm$Tm)
    P_tot <- sum(clim_norm$P)
    wint_months <- order(clim_norm$Tm)[1:6]
    summ_months <- order(clim_norm$Tm)[7:12]
    if (1 %in% wint_months | 2 %in% wint_months) {
      autumn_months <- 9:11
      solst_months <- 5:7
      late.spring_early.summer_months <- 5:6
      late.summer_months <- 8:9
    }
    else {
      autumn_months <- 3:5
      solst_months <- c(11:12, 1)
      late.spring_early.summer_months <- 11:12
      late.summer_months <- 2:3
    }
    P_wint.half <- sum(clim_norm$P[clim_norm$month %in% wint_months])
    P_summ.half <- sum(clim_norm$P[clim_norm$month %in% summ_months])
    P_driest_month <- min(clim_norm$P)
    P_driest_month_summ.half <- min(clim_norm$P[clim_norm$month %in% 
                                                  summ_months])
    P_driest_month_wint.half <- min(clim_norm$P[clim_norm$month %in% 
                                                  wint_months])
    P_wettest_month <- max(clim_norm$P)
    P_wettest_month_summ.half <- max(clim_norm$P[clim_norm$month %in% 
                                                   summ_months])
    P_wettest_month_wint.half <- max(clim_norm$P[clim_norm$month %in% 
                                                   wint_months])
    T_4th_warm_month <- sort(clim_norm$Tm, decreasing = TRUE)[4]
    matr_indices <- round(data.frame(T_warm_month, T_cold_month, 
                                     T_avg, P_tot, P_wint.half, P_summ.half, P_driest_month, 
                                     P_driest_month_summ.half, P_driest_month_wint.half, 
                                     P_wettest_month, P_wettest_month_summ.half, P_wettest_month_wint.half, 
                                     T_4th_warm_month), 1)
    names(matr_indices) <- c("T_w.m", "T_c.m", "T_avg", "P_tot", 
                             "P_wint", "P_summ", "P_d.m", "P_d.m.summ", "P_d.m.wint", 
                             "P_w.m", "P_w.m.summ", "P_w.m.wint", "T_4th_w.m")
    type <- NA
    thresh <- NULL
    sub.type_1 <- NULL
    sub.type_2 <- NULL
    rain.reg <- NULL
    if (T_cold_month >= 18) 
      type <- "A"
    if (T_cold_month < 18 & T_cold_month > -3 & T_warm_month >= 
        10) 
      type <- "C"
    if (T_cold_month <= -3 & T_warm_month >= 10) 
      type <- "D"
    if (T_warm_month < 10) 
      type <- "E"
    if (P_summ.half >= 0.7 * P_tot & P_tot < 20 * T_avg + 
        280) {
      type <- "B"
      rain.reg <- "summ"
      thresh <- 20 * T_avg + 280
    }
    else if (P_wint.half >= 0.7 * P_tot & P_tot < 20 * T_avg) {
      type <- "B"
      rain.reg <- "wint"
      thresh <- 20 * T_avg
    }
    else if (P_summ.half < 0.7 * P_tot & P_wint.half < 0.7 * 
             P_tot & P_tot < 20 * T_avg + 140) {
      type <- "B"
      thresh <- 20 * T_avg + 140
    }
    if (type == "A") {
      if (P_driest_month >= 60) 
        sub.type_1 <- "f"
      if (P_driest_month < 60) 
        sub.type_1 <- "w"
      if (P_driest_month < 60 & P_driest_month >= 100 - 
          P_tot/25) 
        sub.type_1 <- "m"
      if (order(clim_norm$P)[12] %in% autumn_months) 
        sub.type_2 <- "w'"
      if (order(clim_norm$P)[1] %in% solst_months) 
        sub.type_2 <- "s"
      dry.4 <- order(clim_norm$P)[1:4]
      wet.4 <- order(clim_norm$P)[9:12]
      seas.1 <- c(12, 1, 2)
      seas.2 <- 3:5
      seas.3 <- 6:8
      seas.4 <- 9:11
      if (sum(clim_norm$P[dry.4]) * 5 < sum(clim_norm$P[wet.4]) & 
          ((sum(dry.4 %in% c(seas.1, seas.3)) == 4 & sum(wet.4 %in% 
                                                         c(seas.2, seas.4)) == 4) | (sum(dry.4 %in% 
                                                                                         c(seas.2, seas.4)) == 4 & sum(wet.4 %in% c(seas.1, 
                                                                                                                                    seas.3)) == 4))) 
        sub.type_2 <- noquote("w\"")
      if (A_B_C_special_sub.classes == TRUE) {
        if (T_warm_month - T_cold_month < 5) 
          sub.type_2 <- paste(sub.type_2, "i", sep = "")
        if (1 %in% wint_months & order(clim_norm$Tm)[12] <= 
            6 & sum(order(clim_norm$P)[11:12] %in% 7:9) == 
            2) 
          sub.type_2 <- paste(sub.type_2, "g", sep = "")
      }
    }
    if (type == "B") {
      if (P_tot < thresh/2) 
        sub.type_1 <- "W"
      else sub.type_1 <- "S"
      if (T_avg >= 18) 
        sub.type_2 <- "h"
      else {
        if (T_warm_month < 18) 
          sub.type_2 <- "k'"
        else sub.type_2 <- "k"
      }
      if (A_B_C_special_sub.classes == TRUE & !is.null(rain.reg)) {
        if (rain.reg == "summ") 
          sub.type_2 <- paste(sub.type_2, "w", sep = "")
        if (rain.reg == "wint") 
          sub.type_2 <- paste(sub.type_2, "s", sep = "")
      }
    }
    if (type == "C") {
      if (P_driest_month_summ.half < 30 & P_driest_month_summ.half < 
          P_wettest_month_wint.half/3) 
        sub.type_1 <- "s"
      else if (P_driest_month_wint.half < P_wettest_month_summ.half/10) 
        sub.type_1 <- "w"
      else sub.type_1 <- "f"
      if (T_warm_month >= 22) 
        sub.type_2 <- "a"
      else {
        if (T_4th_warm_month > 10) 
          sub.type_2 <- "b"
        else sub.type_2 <- "c"
      }
      if (A_B_C_special_sub.classes == TRUE) {
        if (T_warm_month - T_cold_month < 5) 
          sub.type_2 <- paste(sub.type_2, "i", sep = "")
        if (1 %in% wint_months & order(clim_norm$Tm)[12] <= 
            6 & sum(order(clim_norm$P)[11:12] %in% 7:9) == 
            2) 
          sub.type_2 <- paste(sub.type_2, "g", sep = "")
        if (order(clim_norm$P)[12] %in% late.spring_early.summer_months & 
            sum(clim_norm$P[late.spring_early.summer_months]) > 
            sum(clim_norm$P[late.summer_months]) * 1.3) 
          sub.type_2 <- paste(sub.type_2, "x", sep = "")
      }
    }
    if (type == "D") {
      if (P_driest_month_summ.half < 30 & P_driest_month_summ.half < 
          P_wettest_month_wint.half/3) 
        sub.type_1 <- "s"
      else if (P_driest_month_wint.half < P_wettest_month_summ.half/10) 
        sub.type_1 <- "w"
      else sub.type_1 <- "f"
      if (T_warm_month >= 22) 
        sub.type_2 <- "a"
      else {
        if (T_4th_warm_month > 10) 
          sub.type_2 <- "b"
        else sub.type_2 <- "c"
      }
      if (T_cold_month < -38) 
        sub.type_2 <- "d"
    }
    if (type == "E") {
      if (T_warm_month > 0) 
        sub.type_1 <- "T"
      else sub.type_1 <- "F"
      sub.type_2 <- NULL
    }
    if (class.nr == FALSE) {
      matr_climate <- data.frame(paste(type, sub.type_1, 
                                       sub.type_2, sep = ""))
    }
    else {
      types_numbers <- seq(10, 50, by = 10)
      types_letters <- c("A", "B", "C", "D", "E")
      type_n <- types_numbers[types_letters == type]
      if (type == "A") 
        types.2_letters <- c("f", "w", "m")
      if (type == "B") 
        types.2_letters <- c("W", "S")
      if (type == "C" | type == "D") 
        types.2_letters <- c("s", "w", "f")
      if (type == "E") 
        types.2_letters <- c("T", "F")
      types.2_numbers <- 1:length(types.2_letters)
      type.2_n <- types.2_numbers[types.2_letters == sub.type_1]
      matr_climate <- data.frame(type_n + type.2_n)
    }
    names(matr_climate) <- "class"
    if (clim.resume_verbose == TRUE) 
      matr_climate <- data.frame(matr_indices, matr_climate)
    return(matr_climate)
  }
}
