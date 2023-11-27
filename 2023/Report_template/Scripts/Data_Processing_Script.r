library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggthemes)
library(FLR4MFCL)
library(readr)
library(xtable)
library(data.table)


  rep_dat <- list()

  #yr1 <- 2014
  lst_yr <- 2022
  
  cnt_vec <- c("AS","CK","FJ","FM","GU","KI","MH","NC","NR","NU","PF","PG","PW","SB","TK","TO","TV","WF","WS","VU")
  # Exclude Guam?
  
  skj_reg_lst <- list("AS" = c(8),
                      "CK" = c(8),
                      "FJ" = c(8),
                      "FM" = c(3,4,5,6,7),
                      "GU" = c(3,4),
                      "KI" = c(7,8),
                      "MH" = c(4,7,8),
                      "NC" = c(6,7),
                      "NR" = c(7),
                      "NU" = c(8),
                      "PF" = c(8),
                      "PG" = c(6,7), # I ignored the tiny bit in R5 as no skj in there basically
                      "PW" = c(3,5),
                      "SB" = c(6,7,8),
                      "TK" = c(8),
                      "TO" = c(8),
                      "TV" = c(8),
                      "WF" = c(8),
                      "WS" = c(8),
                      "VU" = c(7,8))
  
  rep_dat$skj_reg_lst <- skj_reg_lst
  
  alb_reg_lst <- list("AS" = c(2),
                      "CK" = c(1,2),
                      "FJ" = c(2), # Ignored the minute slivers in 1 and 3
                      "FM" = c(NA), # Ignored FM sliver below equator as they are not typically included in SPA management
                      "GU" = c(NA),
                      "KI" = c(1,2),
                      "MH" = c(NA),
                      "NC" = c(2,3),
                      "NR" = c(NA), # Ignored NR sliver below equator as they are not typically included in SPA management
                      "NU" = c(2),
                      "PF" = c(1,2,3,4),
                      "PG" = c(1,2),
                      "PW" = c(NA),
                      "SB" = c(1,2),
                      "TK" = c(1,2),
                      "TO" = c(2,3),
                      "TV" = c(1,2),
                      "WF" = c(2),
                      "WS" = c(2),
                      "VU" = c(2))
  
  rep_dat$alb_reg_lst <- alb_reg_lst
  
  yft_reg_lst <- list("AS" = c(5), # Ignored the minute sliver in 4
                      "CK" = c(4,5),
                      "FJ" = c(5), # Ignored the minute sliver in 4
                      "FM" = c(1,2,3,4),
                      "GU" = c(1),
                      "KI" = c(4,5),
                      "MH" = c(1,4),
                      "NC" = c(5),
                      "NR" = c(4),
                      "NU" = c(5),
                      "PF" = c(5),
                      "PG" = c(3,4,5),
                      "PW" = c(2),
                      "SB" = c(3,4,5),
                      "TK" = c(4,5),
                      "TO" = c(5),
                      "TV" = c(4,5),
                      "WF" = c(5), # Ignored the minute sliver in 4
                      "WS" = c(5),
                      "VU" = c(5))
  
  rep_dat$yft_reg_lst <- yft_reg_lst
  
  bet_reg_lst <- list("AS" = c(6), # Ignored the minute sliver in 4
                      "CK" = c(4,6),
                      "FJ" = c(6), # Ignored the minute sliver in 4
                      "FM" = c(1,3,7,8),
                      "GU" = c(1),
                      "KI" = c(3,4,6),
                      "MH" = c(1,2,3,4),
                      "NC" = c(5,6),
                      "NR" = c(3),
                      "NU" = c(6),
                      "PF" = c(6),
                      "PG" = c(3,5,8),
                      "PW" = c(7),
                      "SB" = c(3,4,5,8),
                      "TK" = c(4,6),
                      "TO" = c(6),
                      "TV" = c(4,6),
                      "WF" = c(6), # Ignored the minute sliver in 4
                      "WS" = c(6),
                      "VU" = c(5,6))
  
  rep_dat$bet_reg_lst <- bet_reg_lst


  base_pth <- "./"
  
  alb_rep <- read.MFCLRep("./Data/ALB/plot-final3.par.rep")
  bet_rep <- read.MFCLRep("./Data/BET/plot-09.par.rep")
  skj_rep <- read.MFCLRep("./Data/SKJ/plot-09.par.rep")
  yft_rep <- read.MFCLRep("./Data/YFT/plot-14.par.rep")
  
  

  
  extract_depletion <- function(mod_folder = "S1M1D1R1G1", scl = "full",
                                rundir = "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/",
                                finalpar = "plot-final3.par.rep"){
    
    rawrep <- paste0(rundir, mod_folder, "/", finalpar)
    
    readrep <- read.MFCLRep(rawrep)
    
    if(scl == "full"){
      
      dep_yr <- as.data.frame(seasonMeans(areaSums(adultBiomass(readrep)))/seasonMeans(areaSums(adultBiomass_nofish(readrep))))
      
    } else{
      
      dep_yr <- as.data.frame(seasonMeans(adultBiomass(readrep))/seasonMeans(adultBiomass_nofish(readrep)))
      
    }
    
    dep_yr <- mutate(dep_yr, mod = mod_folder)
    
    return(dep_yr)
  }
  
  
  # Process albacore grid runs to extract depletion at full and region level
  tmpdir <- "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/"
  
  full_dep_alb <- map(list.files(path = tmpdir, pattern = "S", full.names = FALSE, ignore.case = TRUE),  extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-final3.par.rep")
  
  reg_dep_alb <- map(list.files(path = tmpdir, pattern = "S", full.names = FALSE, ignore.case = TRUE), extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-final3.par.rep")
  
  # Process bigeye grid runs to extract depletion at full and region level
  tmpdir <- "//penguin/assessments/bet/2023/model_runs/grid/full/"
  
  full_dep_bet <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[4:57], extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-final.par.rep")
  
  reg_dep_bet <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[4:57], extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-final.par.rep")
  
  # Process skipjack grid runs to extract depletion at full and region level
  tmpdir <- "//penguin/assessments/skj/2022/Assessment/Model_runs/Grid2022/SKJ_3_18July/"
  tmpfiles <- list.files(path = tmpdir, pattern = "T", full.names = FALSE, ignore.case = TRUE)
  
  full_dep_skj <- map(tmpfiles[tmpfiles != "T2G10.8.zip"], extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-09.par.rep")
  
  reg_dep_skj <- map(tmpfiles[tmpfiles != "T2G10.8.zip"], extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-09.par.rep")
  
  
  
  # Process yellowfin grid runs to extract depletion at full and region level
  tmpdir <- "//penguin/assessments/yft/2023/model_runs/grid/full/"
    
  full_dep_yft <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[2:55], extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-final.par.rep")
  
  reg_dep_yft <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[2:55], extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-final.par.rep")

  
  plot_depletion <- function(full_dat = full_dep_alb, reg_dat = reg_dep_alb,
                             reg_lst = alb_reg_lst, cnt = "CK", spp = "alb", ncol_set = 2){
      
    cnt_reg <- reg_lst[[cnt]]
      
    full_dt <- rbindlist(full_dat)
      
    full_pl <- full_dt %>% group_by(year) %>% summarise(dep = median(data), LL_dep = quantile(data, .1), UL_dep = quantile(data, .9))
      
  windows(4000,3000)
    pl <- ggplot(full_pl, aes(x = year, y = dep)) +
                 geom_ribbon(aes(ymin = LL_dep, ymax = UL_dep), fill = "dodgerblue", alpha=0.6) +
                 geom_line(linewidth = 2) +
                 scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) + xlab("") + ylab("Depletion (sb/sbf=0)") +
                 geom_hline(yintercept = 0.2, colour = alpha("red", 0.7), linetype = 2) +
                 theme_bw()
      print(pl)
      savePlot(paste0("Figures/", cnt, "/full_depletion_", spp, ".png"), type="png")
  dev.off()

  write.csv(full_pl, file = (paste0("Figures/", cnt, "/", "depletion_table_overall_", spp, ".csv")), row.names = FALSE) 
      
  reg_dt <- rbindlist(reg_dat)
      
  reg_pl <- reg_dt %>% group_by(year, area) %>%
                       summarise(med_dep = median(data), LL_dep = quantile(data, .1), UL_dep = quantile(data, .9))
      
  reg_pl$fcl_reg <- ifelse(as.numeric(reg_pl$area) %in% cnt_reg, "focal", "other")
      
  sub_dat <- filter(reg_pl, fcl_reg == "focal") %>% group_by(area) %>% filter(row_number() == 1)
      
      
  windows(4000,3000)
    pl <- ggplot(reg_pl, aes(x = year, y = med_dep)) +
                 geom_rect(data = sub_dat, fill = alpha("palegreen", 0.3), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
                 facet_wrap(~ area, ncol = ncol_set) + geom_ribbon(aes(ymin = LL_dep, ymax = UL_dep), fill = "dodgerblue", alpha=0.6) +
                 geom_line(linewidth = 1.5) +
                 scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) + xlab("") + ylab("Depletion (sb/sbf=0)") + geom_hline(yintercept = 0.2, colour = alpha("red", 0.7), linetype = 2) +
                 theme_bw()
    print(pl)
    savePlot(paste0("Figures/", cnt, "/regional_depletion_", spp, ".png"), type="png")
  dev.off()
  
  write.csv(reg_pl, file = (paste0("Figures/", cnt, "/", "depletion_table_regional_", spp, ".csv")), row.names = FALSE)
  
  }
  
  
  map(cnt_vec, plot_depletion, full_dat = full_dep_alb, reg_dat = reg_dep_alb, reg_lst = alb_reg_lst, spp = "alb", ncol_set = 2)
  map(cnt_vec, plot_depletion, full_dat = full_dep_bet, reg_dat = reg_dep_bet, reg_lst = bet_reg_lst, spp = "bet", ncol_set = 3)
  map(cnt_vec, plot_depletion, full_dat = full_dep_skj, reg_dat = reg_dep_skj, reg_lst = skj_reg_lst, spp = "skj", ncol_set = 3)
  map(cnt_vec, plot_depletion, full_dat = full_dep_yft, reg_dat = reg_dep_yft, reg_lst = yft_reg_lst, spp = "yft", ncol_set = 2)

  
#_______________________________________________________________________________    
# Process the catch statistics to create a table of catches for the eez vs region    
    
    wcpfc_dat <- read.csv(file = "./Data/Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv", header = TRUE)
    
    ez_dat <- read.csv(file = "./Data/Ann_Cat_EEZ_ACE.csv", header = TRUE) %>%
                       mutate(ez_id = recode(ez_id, NF = "AU", GL = "KI", LN = "KI", PX = "KI", MA = "NC"))
    
    yb_dat <- read.csv(file = "./Data/Ann_Cat_Oceans_YB_ACE.csv", header = TRUE)
    
    
    # Convert 5x5 coordinates into number and add 2.5 to put in cell centre for assignment to assessment regions
    wcpfc_dat %<>% mutate(lat = as.numeric(str_sub(lat_short,end = -2)), lon = as.numeric(str_sub(lon_short,end = -2)),
                          lat_hem = str_sub(lat_short,start = -1), lon_hem = str_sub(lon_short,start = -1),
                          lat = ifelse(lat_hem == "S", -lat, lat) + 2.5, lon = ifelse(lon_hem == "W", 360 - lon, lon) + 2.5)
    
    
    wcpfc_dat %<>% mutate(reg_yft = NA,
                          reg_yft = ifelse(lat > 20 & lat < 50 & lon > 120 & lon < 210, 1, reg_yft),
                          reg_yft = ifelse(lat > 10 & lat < 20 & lon > 140 & lon < 210, 1, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < 20 & lon > 110 & lon < 140, 2, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < 0 & lon > 140 & lon < 155, 3, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < -5 & lon > 155 & lon < 160, 3, reg_yft),
                          reg_yft = ifelse(lat > 0 & lat < 10 & lon > 140 & lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -5 & lat < 0 & lon > 155 & lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < -5 & lon > 160 & lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -40 & lat < -10 & lon > 140 & lon < 210, 5, reg_yft))
    
    
    wcpfc_dat %<>% mutate(reg_bet = NA,
                          reg_bet = ifelse(lat > 20 & lat < 50 & lon > 120 & lon < 170, 1, reg_bet),
                          reg_bet = ifelse(lat > 10 & lat < 20 & lon > 140 & lon < 170, 1, reg_bet),
                          reg_bet = ifelse(lat > 10 & lat < 50 & lon > 170 & lon < 210, 2, reg_bet),
                          reg_bet = ifelse(lat > 0 & lat < 10 & lon > 140 & lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -5 & lat < 0 & lon > 155 & lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < -5 & lon > 160 & lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 10 & lon > 170 & lon < 210, 4, reg_bet),
                          reg_bet = ifelse(lat > -40 & lat < -10 & lon > 140 & lon < 170, 5, reg_bet), # This includes 9 but gets overwritten
                          reg_bet = ifelse(lat > -40 & lat < -10 & lon > 170 & lon < 210, 6, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 20 & lon > 110 & lon < 140, 7, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 0 & lon > 140 & lon < 155, 8, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < -5 & lon > 155 & lon < 160, 8, reg_bet),
                          reg_bet = ifelse(lat > -20 & lat < -15 & lon > 140 & lon < 150, 9, reg_bet))
    
    wcpfc_dat %<>% mutate(reg_skj = NA,
                          reg_skj = ifelse(lat > 30 & lat < 50 & lon > 120 & lon < 140, 1, reg_skj),
                          reg_skj = ifelse(lat > 30 & lat < 35 & lon > 140 & lon < 145, 1, reg_skj),
                          reg_skj = ifelse(lat > 35 & lat < 50 & lon > 140 & lon < 145, 2, reg_skj),
                          reg_skj = ifelse(lat > 30 & lat < 50 & lon > 145 & lon < 210, 2, reg_skj),
                          reg_skj = ifelse(lat > 20 & lat < 30 & lon > 120 & lon < 130, 3, reg_skj),
                          reg_skj = ifelse(lat > 10 & lat < 30 & lon > 130 & lon < 145, 3, reg_skj),
                          reg_skj = ifelse(lat > 10 & lat < 30 & lon > 145 & lon < 210, 4, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 20 & lon > 110 & lon < 130, 5, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 10 & lon > 130 & lon < 140, 5, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < -5 & lon > 155 & lon < 160, 6, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 0 & lon > 140 & lon < 155, 6, reg_skj),
                          reg_skj = ifelse(lat > 0 & lat < 10 & lon > 140 & lon < 170, 7, reg_skj),
                          reg_skj = ifelse(lat > -5 & lat < 0 & lon > 155 & lon < 160, 7, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 0 & lon > 160 & lon < 170, 7, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 10 & lon > 170 & lon < 210, 8, reg_skj))
    
    wcpfc_dat %<>% mutate(reg_alb = NA,
                          reg_alb = ifelse(lat > -10 & lat < 0 & lon > 140 & lon < 210, 1, reg_alb),
                          reg_alb = ifelse(lat > -10 & lat < 0 & lon > 210 & lon < 230, 1, reg_alb),
                          reg_alb = ifelse(lat > -25 & lat < -10 & lon > 210 & lon < 230, 2, reg_alb),
                          reg_alb = ifelse(lat > -50 & lat < -25 & lon > 210 & lon < 230, 3, reg_alb),
                          reg_alb = ifelse(lat > -5 & lat < 0 & lon > 215 & lon < 230, 4, reg_alb),
                          reg_alb = ifelse(lat > -50 & lat < 0 & lon > 230 & lon < 290, 4, reg_alb))
    
    
    # Assign yearbook data to ocean areas, particularly to separate SPA south Pacific from WCPFC-CA ("WX")
    wc_tab <- yb_dat %>% filter(ocean_id == "WX", between(yy, lst_yr - 4, lst_yr)) %>% mutate(gear = ifelse(!gr_id %in% c("L","P","S"), "Other", gr_id)) %>%
                         group_by(yy, gear) %>% summarise(ALB = sum(alb_c), BET = sum(bet_c), SKJ = sum(skj_c), YFT = sum(yft_c)) %>%
                         group_by(gear) %>% summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
    
    sx_tab <- yb_dat %>% filter(ocean_id == "SX", between(yy, lst_yr - 4, lst_yr)) %>% mutate(gear = ifelse(!gr_id %in% c("L","P","S"), "Other", gr_id)) %>%
                         group_by(yy, gear) %>% summarise(ALB = sum(alb_c)) %>% group_by(gear) %>% summarise(ALB = mean(ALB))
    
    wc_tot <- yb_dat %>% filter(ocean_id == "WX", between(yy, lst_yr - 4, lst_yr)) %>% group_by(yy) %>%
                         summarise(ALB = sum(alb_c), BET = sum(bet_c), SKJ = sum(skj_c), YFT = sum(yft_c)) %>%
                         summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
    
    
    reg_df <- rbind(c('WCPFC-CA catch', round(sum(sx_tab$ALB)), round(wc_tot[, c("BET","SKJ","YFT")])),
                    c('5-year WCPFC-CA catch trend', 'Stable','Decreasing','Stable','Increasing'),
                    c('WCPFC-CA Longline catch', round(sx_tab[sx_tab$gear == "L", "ALB"]), round(wc_tab[wc_tab$gear == "L", c("BET","SKJ","YFT")])),
                    c('WCPFC-CA Purse seine catch', round(sx_tab[sx_tab$gear == "S", "ALB"]), round(wc_tab[wc_tab$gear == "S", c("BET","SKJ","YFT")])),
                    c('WCPFC-CA Pole and line catch', round(sx_tab[sx_tab$gear == "P", "ALB"]), round(wc_tab[wc_tab$gear == "P", c("BET","SKJ","YFT")])),
                    c('WCPFC-CA Other catch', round(sx_tab[sx_tab$gear == "Other", "ALB"]), round(wc_tab[wc_tab$gear == "Other", c("BET","SKJ","YFT")]))
                    )
    
    # Annoying and repetitive but trying this to format the numbers for the xtable
    reg_df_comma <- rbind(c('WCPFC-CA catch',  prettyNum(round(sum(sx_tab$ALB)), big.mark=","), prettyNum(round(wc_tot[, c("BET","SKJ","YFT")]), big.mark=",")),
                          c('5-year WCPFC-CA catch trend', 'Stable','Decreasing','Stable','Increasing'),
                          c('WCPFC-CA Longline catch', prettyNum(round(sx_tab[sx_tab$gear == "L", "ALB"]), big.mark=","), prettyNum(round(wc_tab[wc_tab$gear == "L", c("BET","SKJ","YFT")]), big.mark=",")),
                          c('WCPFC-CA Purse seine catch', prettyNum(round(sx_tab[sx_tab$gear == "S", "ALB"]), big.mark=","), prettyNum(round(wc_tab[wc_tab$gear == "S", c("BET","SKJ","YFT")]), big.mark=",")),
                          c('WCPFC-CA Pole and line catch', prettyNum(round(sx_tab[sx_tab$gear == "P", "ALB"]), big.mark=","), prettyNum(round(wc_tab[wc_tab$gear == "P", c("BET","SKJ","YFT")]), big.mark=",")),
                          c('WCPFC-CA Other catch', prettyNum(round(sx_tab[sx_tab$gear == "Other", "ALB"]), big.mark=","), prettyNum(round(wc_tab[wc_tab$gear == "Other", c("BET","SKJ","YFT")]), big.mark=","))
                          )
    
    
    # Need to note in table footnote that this is just Sth Pacific Albacore
    
    colnames(reg_df) <- c("Summary description","Albacore","Bigeye","Skipjack","Yellowfin")
    colnames(reg_df_comma) <- c("Summary description","Albacore","Bigeye","Skipjack","Yellowfin")
    
    
    # Function to produce the catch table for each country
    get_cnt_catches <- function(cnt = "CK"){
      
      alb_reg <- alb_reg_lst[[cnt]]
      bet_reg <- bet_reg_lst[[cnt]]
      skj_reg <- skj_reg_lst[[cnt]]
      yft_reg <- yft_reg_lst[[cnt]]
      
      
      ez_stat <- ez_dat %>% filter(ez_id == cnt, between(yy, lst_yr - 4, lst_yr)) %>%
                            group_by(yy) %>% summarise(ALB = sum(sum_alb_c), BET = sum(sum_bet_c), SKJ = sum(sum_skj_c), YFT = sum(sum_yft_c)) %>%
                            summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
      
      flg_stat <- ez_dat %>% filter(flag_id == cnt, between(yy, lst_yr - 4, lst_yr)) %>%
                             group_by(yy) %>% summarise(ALB = sum(sum_alb_c), BET = sum(sum_bet_c), SKJ = sum(sum_skj_c), YFT = sum(sum_yft_c)) %>%
                             summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
      
      
      reg_dat_alb <- wcpfc_dat %>% filter(reg_alb %in% alb_reg, between(yy, lst_yr - 4, lst_yr)) %>%
                                   group_by(yy) %>% summarise(ALB = sum(sum_alb_c)) %>% summarise(ALB = round(mean(ALB))) # Not sure we need the 2 summarise functions?
      
      reg_dat_bet <- wcpfc_dat %>% filter(reg_bet %in% bet_reg, between(yy, lst_yr - 4, lst_yr)) %>%
                                   group_by(yy) %>% summarise(BET = sum(sum_bet_c)) %>% summarise(BET = round(mean(BET)))

      reg_dat_skj <- wcpfc_dat %>% filter(reg_skj %in% skj_reg, between(yy, lst_yr - 4, lst_yr)) %>%
                                   group_by(yy) %>% summarise(SKJ = sum(sum_skj_c)) %>% summarise(SKJ = round(mean(SKJ)))
      
      reg_dat_yft <- wcpfc_dat %>% filter(reg_yft %in% yft_reg, between(yy, lst_yr - 4, lst_yr)) %>%
                                   group_by(yy) %>% summarise(YFT = sum(sum_yft_c)) %>% summarise(YFT = round(mean(YFT)))
      
      
      full_df <- rbind(reg_df,
                       c("","","","",""),
                       c("","","","",""),
                       c(paste('Catch in', cnt, 'model regions'), reg_dat_alb, reg_dat_bet, reg_dat_skj, reg_dat_yft),
                       c(paste('Percent of WCPFC-CA catch in', cnt, 'model regions'),
                         round(reg_dat_alb/reg_df[1, 2]*100, 1),
                         round(reg_dat_bet/reg_df[1, 3]*100, 1),
                         round(reg_dat_skj/reg_df[1, 4]*100, 1),
                         round(reg_dat_yft/reg_df[1, 5]*100, 1)),
                       c(paste('Catch in', cnt, 'EEZ'), round(ez_stat[1,])),
                       c(paste('Percent of WCPFC-CA catch taken in', cnt, 'EEZ'), round(ez_stat[1,]/reg_df[1, 2:5]*100, 1)),
                       c(paste('Percent of Catch in', cnt, 'model regions taken in', cnt, 'EEZ'), round(ez_stat[1,]/c(reg_dat_alb, reg_dat_bet, reg_dat_skj, reg_dat_yft)*100, 1)),
                       c(paste('Catch by', cnt, 'flagged in WCPFC-CA'), round(flg_stat[1,])),
                       c(paste('Percent of WCPFC-CA catch by', cnt, 'flagged vessels'), round(flg_stat[1,]/reg_df[1, 2:5]*100, 1))
                       )
      
      
      df_write <-apply(full_df, 2, as.character)
      write.csv(df_write, file = (paste0("Figures/", cnt, "/", "catch_summary_table.csv")), row.names = FALSE) 
      
      # Annoying and repetitive but trying this to format the numbers for the xtable
      full_df_comma <- rbind(reg_df_comma,
                       #c("","","","",""),
                       #c("","","","",""),
                       c(paste('Catch in', cnt, 'model regions'), prettyNum(reg_dat_alb, big.mark=","),
                               prettyNum(reg_dat_bet, big.mark=","), prettyNum(reg_dat_skj, big.mark=","), prettyNum(reg_dat_yft, big.mark=",")),
                       c(paste('Percent of WCPFC-CA catch in', cnt, 'model regions'),
                         prettyNum(round(reg_dat_alb/reg_df[1, 2]*100, 1), big.mark=","),
                         prettyNum(round(reg_dat_bet/reg_df[1, 3]*100, 1), big.mark=","),
                         prettyNum(round(reg_dat_skj/reg_df[1, 4]*100, 1), big.mark=","),
                         prettyNum(round(reg_dat_yft/reg_df[1, 5]*100, 1), big.mark=",")),
                       c(paste('Catch in', cnt, 'EEZ'), prettyNum(round(ez_stat[1,]), big.mark=",")),
                       c(paste('Percent of WCPFC-CA catch taken in', cnt, 'EEZ'), prettyNum(round(ez_stat[1,]/reg_df[1, 2:5]*100, 1), big.mark=",")),
                       c(paste('Percent of Catch in', cnt, 'model regions taken in', cnt, 'EEZ'), prettyNum(round(ez_stat[1,]/c(reg_dat_alb, reg_dat_bet, reg_dat_skj, reg_dat_yft)*100, 1), big.mark=",")),
                       c(paste('Catch by', cnt, 'flagged in WCPFC-CA'), prettyNum(round(flg_stat[1,]), big.mark=",")),
                       c(paste('Percent of WCPFC-CA catch by', cnt, 'flagged vessels'), prettyNum(round(flg_stat[1,]/reg_df[1, 2:5]*100, 1), big.mark=","))
                       )
      
      df_write_comma <-apply(full_df_comma, 2, as.character)
      
      t1 <- xtable(df_write_comma, caption = paste("Key stock and fishery catch statistics for the WCPFC convention area for the recent period of",
                                             lst_yr - 4, "--", lst_yr, "including catches in the", cnt, "EEZ, and by", cnt, "flagged or chartered vessels, if they exist"),
                   label = "cat_sum_tab", align = c("l","l",rep("c",dim(df_write)[2]-1)))
      
      print(t1, type = "latex", include.rownames = FALSE, tabular.environment = "longtable", caption.placement = "top",
            floating = FALSE, sanitize.text.function = identity, sanitize.colnames.function = NULL, format.args = list(big.mark = ","),
            hline.after = c(-1,0,dim(df_write)[1]-7), file = paste0("Figures/", cnt, "/", "catch_summary_table.tex"))

    }
    
    
    cnt_run <- map(cnt_vec, get_cnt_catches)
    
  # Output the summary data for dynamic use in the reports
  save.image(paste0("Data/Output_Summary_Data.Rdata"))
    
    
    

  
  
   