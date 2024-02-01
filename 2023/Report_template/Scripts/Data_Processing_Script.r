library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggthemes)
library(FLR4MFCL)
library(readr)
library(xtable)
library(data.table)

# Note that for sharing, this assumes you have an R project established at the appropriate path so that relative (e.g. ./) paths can be used

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
  
  # alb_rep <- read.MFCLRep("./Data/ALB/plot-final3.par.rep")
  # bet_rep <- read.MFCLRep("./Data/BET/plot-09.par.rep")
  # skj_rep <- read.MFCLRep("./Data/SKJ/plot-09.par.rep")
  # yft_rep <- read.MFCLRep("./Data/YFT/plot-14.par.rep")
  

  
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
  # Each of these paths to penguin/assessments will have to be modified depending on how it is mapped on your machine
  tmpdir <- "D:/alb/2021/backupCCJ/ALB21_Projections/"
  
  full_dep_alb <- map(list.files(path = tmpdir, pattern = "S", full.names = FALSE, ignore.case = TRUE),  extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-final3.par.rep")
  
  reg_dep_alb <- map(list.files(path = tmpdir, pattern = "S", full.names = FALSE, ignore.case = TRUE), extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-final3.par.rep")
  
  # Process bigeye grid runs to extract depletion at full and region level
  tmpdir <- "D:/bet/2023/model_runs/grid/full/"
   
  full_dep_bet <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[4:57], extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-final.par.rep")
  
  reg_dep_bet <- map(list.files(path = tmpdir, pattern = "_", full.names = FALSE, ignore.case = TRUE)[4:57], extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-final.par.rep")
  
  # Process skipjack grid runs to extract depletion at full and region level
  tmpdir <- "D:/skj/2022/Assessment/Model_runs/Grid2022/SKJ_3_18July/"
  
  tmpfiles <- list.files(path = tmpdir, pattern = "T", full.names = FALSE, ignore.case = TRUE)
  
  full_dep_skj <- map(tmpfiles[tmpfiles != "T2G10.8.zip"], extract_depletion, scl = "full",
                      rundir = tmpdir, finalpar = "plot-09.par.rep")
  
  reg_dep_skj <- map(tmpfiles[tmpfiles != "T2G10.8.zip"], extract_depletion, scl = "regional",
                     rundir = tmpdir, finalpar = "plot-09.par.rep")
  
  
  # Process yellowfin grid runs to extract depletion at full and region level
  tmpdir <- "D:/yft/2023/model_runs/grid/full/"
    
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
    
    
#____________________________________________________________________________________________________________
# The following produces the species catch maps and catch history bars for the full region and assessment sub-regions
#____________________________________________________________________________________________________________
  
  
#____________________________________________________________________________________________________________
  # This section has to be run in 32bit R
  
  library(RODBC)
  
  dat.pth <- "C:/GitRep/CntTFAR/2023/Report_template/Data/"
  
  channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                  SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                  SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")
  
  
  # All gear albacore catch by gear, flag and ocean ID by 5 x 5 from a_best
  dat <- sqlQuery(channellog, query = "SELECT gr_id, ocean_id, flag_id, yy, lat_short, lon_short, sch_id, sum(days), sum(sets), sum(hhooks), sum(stdeff),
                                       sum(alb_c), sum(alb_n), sum(bet_c), sum(bet_n), sum(skj_c), sum(skj_n), sum(yft_c), sum(yft_n) FROM a_best WHERE yy>=1950
                                       GROUP BY gr_id, ocean_id, flag_id, yy, lat_short, lon_short, sch_id", max = 0, stringsAsFactors = FALSE)
  
  write.csv(dat, file = paste0(dat.pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"), row.names = FALSE)
  
  
#____________________________________________________________________________________________________________
  # Can switch back to 64bit R for this bit
  
  library(tidyverse)
  library(magrittr)
  library(sf)
  library(maps)
  library(scatterpie)
  library(rnaturalearth)
  library(ggthemes)
  
  last_yr <- 2022
  
  theme_set(theme_bw())
  
  # Define characteristics of fishing gear
  gearspecs <- list(code=c("L","P","S","Z","SA","SU"), code2=c("ll","pl","ps","ot","sa","su"),
                    gearEN=c("Longline","Pole-and-line","Purse seine","Other","PS-associated","PS-free school"),
                    cls=c("forestgreen","firebrick3","dodgerblue2","yellow2","dodgerblue2","lightblue"))
  
  
  #####
  # Need to change these paths from absolute to relative
  #####
  
  dat.pth <- "C:/GitRep/CntTFAR/2023/Report_template/Data/"
  
  dir.pth <- "C:/GitRep/CntTFAR/2023/Report_template/Figures/Regional/"
  
  dat <- read.csv(file = paste0(dat.pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"))
  
  #eez <- st_read(paste0(dat.pth, "EEZ_Shape_Files/World_EEZ_v10_2018_0_360.shp"))
  eez <- st_read("C:/EEZs/Data/World_EEZ_v11/eez_v11_0_360.shp")
  
  world_shp_180 <- sf::st_as_sf(maps::map("world", wrap = c(0,360),  plot = FALSE, fill = TRUE))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  wcpfc.sf <- c("POLYGON((141 -30,141 -55,150 -55,150 -60,230 -60,230 -4,210 -4,210 50,100 50,100 -10,130 -10,130 -30,141 -30))") %>% 
    st_as_sfc(crs=st_crs(eez)) %>% 
    st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  r1.sf <- c("POLYGON((120 50, 170 50, 170 10, 140 10, 140 20, 120 20, 120 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r2.sf <- c("POLYGON((170 50, 210 50, 210 10, 170 10, 170 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r3.sf <- c("POLYGON((140 10, 170 10, 170 -10, 160 -10, 160 -5, 155 -5, 155 0, 140 0, 140 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r4.sf <- c("POLYGON((170 10, 210 10, 210 -10, 170 -10, 170 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r5.sf <- c("POLYGON((140 -10, 170 -10, 170 -40, 140 -40, 140 -20, 150 -20, 150 -15, 140 -15, 140 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r6.sf <- c("POLYGON((170 -10, 210 -10, 210 -40, 170 -40, 170 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r7.sf <- c("POLYGON((110 20, 140 20, 140 -10, 110 -10, 110 20))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r8.sf <- c("POLYGON((140 0, 155 0, 155 -5, 160 -5, 160 -10, 140 -10, 140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r9.sf <- c("POLYGON((140 -15, 150 -15, 150 -20, 140 -20, 140 -15))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) +
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r6.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r7.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r8.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r9.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    coord_sf(xlim = c(105,230), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    annotate("text", x = c(155,190,160,190,160,190,125,150,145), y = c(35,35,0,0,-30,-30,5,-5,-18), label = c("1","2","3","4","5","6","7","8","9"), size = 12, fontface = "bold") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal")
  #geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/2000/pi), x = 220, y = 35, labeller = function(x) round(pi*2000*x^2, -3)) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/YFT-BET_Region_Map.png"), type = "png")
  dev.off()
  
  
  r1.sf.5 <- c("POLYGON((120 50, 210 50, 210 10, 140 10, 140 20, 120 20, 120 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r2.sf.5 <- c("POLYGON((110 20, 140 20, 140 -10, 110 -10, 110 20))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r3.sf.5 <- c("POLYGON((140 0, 155 0, 155 -5, 160 -5, 160 -10, 140 -10, 140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r4.sf.5 <- c("POLYGON((140 10, 210 10, 210 -10, 160 -10, 160 -5, 155 -5, 155 0, 140 0, 140 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r5.sf.5 <- c("POLYGON((140 -10, 210 -10, 210 -40, 140 -40, 140 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    coord_sf(xlim = c(105,230), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    annotate("text", x = c(170,133,150,190,190), y = c(35,15,-5,5,-30), label = c("1","2","3","4","5"), size = 12, fontface = "bold") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal")
  print(pl)
  savePlot(filename = paste0(dir.pth, "/YFT_5_Region_Map.png"), type = "png")
  dev.off()
  
  
  dat %<>% mutate(Lat = as.numeric(str_extract(lat_short, "[0-9]+")), Lon = as.numeric(str_extract(lon_short, "[0-9]+")),
                  hem.lat = str_extract(lat_short, "[aA-zZ]+"), hem.lon = str_extract(lon_short, "[aA-zZ]+"),
                  Lat = ifelse(hem.lat == "S", -Lat + 2.5, Lat + 2.5), Lon = ifelse(hem.lon == "W", 360 - Lon + 2.5, Lon + 2.5))
  
  dat.pl <- dat %>% mutate(gr_id = ifelse(gr_id %in% c("L","P","S"), gr_id, "Oth"), Gear = recode(gr_id, L = "Longline", P = "Pole and line", S = "Purse seine", Oth = "Other"))
  
#____________________________  
  # Bigeye map
  # Recent period
  dat.pl.trunc <- dat.pl %>% filter(yy >= last_yr - 9, yy <= last_yr) %>% group_by(Lat, Lon, Gear) %>% summarise(Catch = sum(sum_bet_c)/length((last_yr - 9):last_yr))
  
  dat.pl.w <- dat.pl.trunc %>% pivot_wider(names_from = Gear, values_from = Catch)
  dat.pl.w[is.na(dat.pl.w)] <- 0
  dat.pl.w$Total <- apply(dat.pl.w[,-c(1:2)], 1, sum)
  
  dat.pl.w.S <- filter(dat.pl.w, Total > 0)
  max.circ <- max(dat.pl.w.S)/5
  
  
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r6.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r7.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r8.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r9.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_scatterpie(data=dat.pl.w.S, aes(x = Lon, y = Lat, r = sqrt(Total/100/pi)), cols = c("Pole and line","Purse seine","Longline","Other"), alpha = 0.8) +
    scale_fill_manual(values = c("firebrick","dodgerblue","forestgreen","yellow3")) +
    coord_sf(xlim = c(105,280), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal") +
    geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/100/pi), x = 260, y = 40, labeller = function(x) round(pi*100*x^2, -3), n = 3) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/BET_Regional_Recent_Catch_Map.png"), type = "png")
  dev.off()
  
#____________________________  
  # Yellowfin map
  # Recent period
  
  dat.pl.trunc <- dat.pl %>% filter(yy >= last_yr - 9, yy <= last_yr) %>% group_by(Lat, Lon, Gear) %>%
    summarise(Catch = sum(sum_yft_c)/length((last_yr - 9):last_yr))
  
  dat.pl.w <- dat.pl.trunc %>% pivot_wider(names_from = Gear, values_from = Catch)
  dat.pl.w[is.na(dat.pl.w)] <- 0
  dat.pl.w$Total <- apply(dat.pl.w[,-c(1:2)], 1, sum)
  
  dat.pl.w.S <- filter(dat.pl.w, Total > 0)
  max.circ <- max(dat.pl.w.S)/5
  
  
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r6.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r7.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r8.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r9.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_scatterpie(data=dat.pl.w.S, aes(x = Lon, y = Lat, r = sqrt(Total/1400/pi)), cols = c("Pole and line","Purse seine","Longline","Other"), alpha = 0.8) +
    scale_fill_manual(values = c("firebrick","dodgerblue","forestgreen","yellow3")) +
    coord_sf(xlim = c(105,280), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal") +
    geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/1400/pi), x = 260, y = 40, labeller = function(x) round(pi*1400*x^2, -3), n = 3) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/YFT_Regional_Recent_Catch_Map.png"), type = "png")
  dev.off()
  
  
  # 5 region version
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf.5, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_scatterpie(data=dat.pl.w.S, aes(x = Lon, y = Lat, r = sqrt(Total/1400/pi)), cols = c("Pole and line","Purse seine","Longline","Other"), alpha = 0.8) +
    scale_fill_manual(values = c("firebrick","dodgerblue","forestgreen","yellow3")) +
    coord_sf(xlim = c(105,280), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal") +
    geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/1400/pi), x = 260, y = 40, labeller = function(x) round(pi*1400*x^2, -3), n = 3) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/YFT_Regional_Recent_Catch_Map_5Reg.png"), type = "png")
  dev.off()
  
  
#____________________________ 
  # Skipjack map
  # Recent period  
  
  r1.sf <- c("POLYGON((120 50, 140 50, 140 35, 145 35, 145 30, 120 30, 120 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r2.sf <- c("POLYGON((140 50, 210 50, 210 30, 145 30, 145 35, 140 35, 140 50))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r3.sf <- c("POLYGON((120 30, 145 30, 145 10, 130 10, 130 20, 120 20, 120 30))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r4.sf <- c("POLYGON((145 30, 210 30, 210 10, 145 10, 145 30))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r5.sf <- c("POLYGON((110 20, 130 20, 130 10, 140 10, 140 -20, 110 -20, 110 20))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r6.sf <- c("POLYGON((140 0, 155 0, 155 -5, 160 -5, 160 -20, 140 -20, 140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r7.sf <- c("POLYGON((140 10, 170 10, 170 -20, 160 -20, 160 -5, 155 -5, 155 0, 140 0, 140 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r8.sf <- c("POLYGON((170 10, 210 10, 210 -20, 170 -20, 170 10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  
  dat.pl <- dat %>% mutate(gr_id = ifelse(gr_id %in% c("L","P","S"), gr_id, "Oth"), Gear = recode(gr_id, L = "Longline", P = "Pole and line", S = "Purse seine", Oth = "Other"))
  
  dat.pl.trunc <- dat.pl %>% filter(yy >= last_yr - 9, yy <= last_yr) %>% group_by(Lat, Lon, Gear) %>% summarise(Catch = sum(sum_skj_c)/length((last_yr - 9):last_yr))
  
  dat.pl.w <- dat.pl.trunc %>% pivot_wider(names_from = Gear, values_from = Catch)
  dat.pl.w[is.na(dat.pl.w)] <- 0
  dat.pl.w$Total <- apply(dat.pl.w[,-c(1:2)], 1, sum)
  
  dat.pl.w.S <- filter(dat.pl.w, Total > 0)
  max.circ <- max(dat.pl.w.S)/5
  
  
  windows(2800,2000)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r5.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r6.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r7.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r8.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_scatterpie(data=dat.pl.w.S, aes(x = Lon, y = Lat, r = sqrt(Total/1300/pi)), cols = c("Pole and line","Purse seine","Longline","Other"), alpha = 0.8) +
    scale_fill_manual(values = c("firebrick","dodgerblue","forestgreen","yellow3")) +
    coord_sf(xlim = c(105,280), ylim = c(-50,55)) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal") +
    geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/1300/pi), x = 260, y = 40, labeller = function(x) round(pi*100*x^2, -3), n = 3) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/SKJ_Regional_Recent_Catch_Map.png"), type = "png")
  dev.off()
  
  
#____________________________ 
  # Albacore map
  # Recent period  
  
  r1.sf <- c("POLYGON((140 0, 210 0, 210 -5, 230 -5, 230 -10, 140 -10, 140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r2.sf <- c("POLYGON((140 -10, 230 -10, 230 -25, 140 -25, 140 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r3.sf <- c("POLYGON((140 -25, 230 -25, 230 -50, 140 -50, 140 -25))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r4.sf <- c("POLYGON((210 0, 290 0, 290 -50, 230 -50, 230 -5, 210 -5, 210 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  dat.pl <- dat %>% mutate(gr_id = ifelse(gr_id %in% c("L","T"), gr_id, "Oth"), Gear = recode(gr_id, L = "Longline", T = "Troll", Oth = "Other"))
  
  dat.pl.trunc <- dat.pl %>% filter(yy >= last_yr - 9, yy <= last_yr) %>% group_by(Lat, Lon, Gear) %>% summarise(Catch = sum(sum_alb_c)/length((last_yr - 9):last_yr))
  
  dat.pl.w <- dat.pl.trunc %>% pivot_wider(names_from = Gear, values_from = Catch)
  dat.pl.w[is.na(dat.pl.w)] <- 0
  dat.pl.w$Total <- apply(dat.pl.w[,-c(1:2)], 1, sum)
  
  dat.pl.w.S <- filter(dat.pl.w, Total > 0)
  max.circ <- max(dat.pl.w.S)/5
  
  
  windows(2800,1500)
  pl <- ggplot() +
    geom_sf(data = eez, color = "black", size = 0.1) + #geom_sf(data = pac.eez, aes(fill=Territory1))
    geom_sf(data = world_shp_180, fill = alpha("wheat", 0.7), color = "black", size = 0.1) +
    geom_sf(data = r1.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r2.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r3.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_sf(data = r4.sf, colour = alpha("black", 0.9), fill = alpha("dodgerblue", 0.05), size = 0.8) +
    geom_scatterpie(data=dat.pl.w.S, aes(x = Lon, y = Lat, r = sqrt(Total/100/pi)), cols = c("Longline", "Other", "Troll"), alpha = 0.8) +
    scale_fill_manual(values = c("forestgreen","orange","yellow3")) +
    coord_sf(xlim = c(135,295), ylim = c(-55,5)) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_blank(), axis.text = element_text(size = 16), plot.title = element_text(size = 25),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 16), legend.direction="horizontal") +
    geom_scatterpie_legend(sqrt(dat.pl.w.S$Total/100/pi), x = 260, y = -45, labeller = function(x) round(pi*100*x^2, -3), n = 3) # Rounding labels to nearest 100...
  print(pl)
  savePlot(filename = paste0(dir.pth, "/ALB_Regional_Recent_Catch_Map.png"), type = "png")
  dev.off()
  
  
#____________________________________________________________________________________________________________  
  # Catch bar plots - bigeye and yellowfin
  
  dat <- read.csv(file = paste0(dat.pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"))
  
  dat %<>% mutate(Lat = as.numeric(str_extract(lat_short, "[0-9]+")), Lon = as.numeric(str_extract(lon_short, "[0-9]+")),
                  hem.lat = str_extract(lat_short, "[aA-zZ]+"), hem.lon = str_extract(lon_short, "[aA-zZ]+"),
                  Lat = ifelse(hem.lat == "S", -Lat + 2.5, Lat + 2.5), Lon = ifelse(hem.lon == "W", 360 - Lon + 2.5, Lon + 2.5),
                  gr_id = ifelse(gr_id %in% c("L","P","S"), gr_id, "Oth"))
  
  dat.pl <- dat %>% mutate(Gear = recode(gr_id, L = "Longline", P = "Pole-and-line", S = "PS-associated", Oth = "Other"),
                           Gear = ifelse(Gear == "PS-associated" & sch_id < 0, "PS-unclassified", Gear),
                           Gear = ifelse(Gear == "PS-associated" & sch_id %in% 1:2, "PS-free-school", Gear)) %>%
    filter(yy >= 1952, yy <= last_yr)
  
  dat.pl %<>% mutate(reg = NA,
                     reg = ifelse(Lat > 20 & Lat < 50 & Lon > 120 & Lon < 170, "Region 1", reg),
                     reg = ifelse(Lat > 10 & Lat < 20 & Lon > 140 & Lon < 170, "Region 1", reg),
                     reg = ifelse(Lat > 10 & Lat < 50 & Lon > 170 & Lon < 210, "Region 2", reg),
                     reg = ifelse(Lat > 0 & Lat < 10 & Lon > 140 & Lon < 170, "Region 3", reg),
                     reg = ifelse(Lat > -5 & Lat < 0 & Lon > 155 & Lon < 170, "Region 3", reg),
                     reg = ifelse(Lat > -10 & Lat < -5 & Lon > 160 & Lon < 170, "Region 3", reg),
                     reg = ifelse(Lat > -10 & Lat < 10 & Lon > 170 & Lon < 210, "Region 4", reg),
                     reg = ifelse(Lat > -40 & Lat < -10 & Lon > 140 & Lon < 170, "Region 5", reg), # This includes 9 but gets overwritten
                     reg = ifelse(Lat > -40 & Lat < -10 & Lon > 170 & Lon < 210, "Region 6", reg),
                     reg = ifelse(Lat > -10 & Lat < 20 & Lon > 110 & Lon < 140, "Region 7", reg),
                     reg = ifelse(Lat > -10 & Lat < 0 & Lon > 140 & Lon < 155, "Region 8", reg),
                     reg = ifelse(Lat > -10 & Lat < -5 & Lon > 155 & Lon < 160, "Region 8", reg),
                     reg = ifelse(Lat > -20 & Lat < -15 & Lon > 140 & Lon < 150, "Region 9", reg))
  
  
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")) %>% group_by(Year = yy, Gear) %>%
    summarise(BETc = sum(sum_bet_c), YFTc = sum(sum_yft_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")))
  
#____________________________ 
  # Bigeye total assessment region catch by gear
  windows(3500,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = BETc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    xlab("Year") + ylab("Bigeye catch (mt)") +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 5)) +
    scale_y_continuous(breaks = seq(0, 200000, 20000), limits = c(0,200000), labels = scales::comma) + theme_clean() +
    theme(legend.position = c(0.3,0.91), legend.direction = "horizontal", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid'))
  print(pl)
  savePlot(file = paste0(dir.pth, "/BET_Catch_ByGear_WholeArea_Within_Assessment.png"), type = "png")
  dev.off()
  
#____________________________   
  # Yellowfin total assessment region catch by gear
  windows(3500,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = YFTc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    xlab("Year") + ylab("Yellowfin catch (mt)") +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 5)) +
    scale_y_continuous(breaks = seq(0, 900000, 50000), limits = c(0,900000), labels = scales::comma) + theme_clean() +
    theme(legend.position = c(0.3,0.91), legend.direction = "horizontal", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid'))
  print(pl)
  savePlot(file = paste0(dir.pth, "/YFT_Catch_ByGear_WholeArea_Within_Assessment.png"), type = "png")
  dev.off()
  
  
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")) %>% group_by(Year = yy, Gear, Region = reg) %>%
    summarise(BETc = sum(sum_bet_c), YFTc = sum(sum_yft_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")))
  
  
#____________________________   
  # Bigeye individual region catch by gear  
  windows(3000,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = BETc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    facet_wrap(~ Region, ncol = 3, scales = "free_x") +
    xlab("Year") + ylab("Bigeye catch (mt)") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 15)) + theme_clean() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "top", legend.direction = "horizontal", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11), legend.background = element_rect(color = NA))
  print(pl)
  savePlot(file = paste0(dir.pth, "/BET_Catch_ByGear_ByRegion_Within_Assessment.png"), type = "png")
  dev.off()
  
  
#____________________________   
  # Yellowfin individual region catch by gear - 9 regions   
  windows(3000,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = YFTc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    facet_wrap(~ Region, ncol = 3, scales = "free_x") +
    xlab("Year") + ylab("Yellowfin catch (mt)") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 15)) + theme_clean() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "top", legend.direction = "horizontal", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11), legend.background = element_rect(color = NA))
  print(pl)
  savePlot(file = paste0(dir.pth, "/YFT_Catch_ByGear_ByRegion_Within_Assessment.png"), type = "png")
  dev.off()
  
  
  dat.pl <- dat %>% mutate(Gear = recode(gr_id, L = "Longline", P = "Pole-and-line", S = "PS-associated", Oth = "Other"),
                           Gear = ifelse(Gear == "PS-associated" & sch_id < 0, "PS-unclassified", Gear),
                           Gear = ifelse(Gear == "PS-associated" & sch_id %in% 1:2, "PS-free-school", Gear)) %>%
    filter(yy >= 1952, yy <= last_yr)
  
  dat.pl %<>% mutate(reg = NA,
                     reg = ifelse(Lat > 20 & Lat < 50 & Lon > 120 & Lon < 210, "Region 1", reg),
                     reg = ifelse(Lat > 10 & Lat < 20 & Lon > 140 & Lon < 210, "Region 1", reg),
                     reg = ifelse(Lat > -10 & Lat < 20 & Lon > 110 & Lon < 140, "Region 2", reg),
                     reg = ifelse(Lat > -10 & Lat < 0 & Lon > 140 & Lon < 155, "Region 3", reg),
                     reg = ifelse(Lat > -10 & Lat < -5 & Lon > 155 & Lon < 160, "Region 3", reg),
                     reg = ifelse(Lat > 0 & Lat < 10 & Lon > 140 & Lon < 210, "Region 4", reg),
                     reg = ifelse(Lat > -5 & Lat < 0 & Lon > 155 & Lon < 210, "Region 4", reg),
                     reg = ifelse(Lat > -10 & Lat < -5 & Lon > 160 & Lon < 210, "Region 4", reg),
                     reg = ifelse(Lat > -40 & Lat < -10 & Lon > 140 & Lon < 210, "Region 5", reg))
  
  
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")) %>% group_by(Year = yy, Gear, Region = reg) %>%
    summarise(BETc = sum(sum_bet_c), YFTc = sum(sum_yft_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")))
  
#____________________________   
  # Yellowfin individual region catch by gear - 5 regions     
  windows(3000,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = YFTc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    facet_wrap(~ Region, ncol = 3, scales = "free_x") +
    xlab("Year") + ylab("Yellowfin catch (mt)") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 15)) + theme_clean() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "top", legend.direction = "horizontal", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11), legend.background = element_rect(color = NA))
  print(pl)
  savePlot(file = paste0(dir.pth, "/YFT_Catch_ByGear_ByRegion_Within_Assessment_5Reg.png"), type = "png")
  dev.off()
  
  
#____________________________________________________________________________________________________________  
  # Catch bar plots - skipjack
  
  dat <- read.csv(file = paste0(dat.pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"))
  
  dat %<>% mutate(Lat = as.numeric(str_extract(lat_short, "[0-9]+")), Lon = as.numeric(str_extract(lon_short, "[0-9]+")),
                  hem.lat = str_extract(lat_short, "[aA-zZ]+"), hem.lon = str_extract(lon_short, "[aA-zZ]+"),
                  Lat = ifelse(hem.lat == "S", -Lat + 2.5, Lat + 2.5), Lon = ifelse(hem.lon == "W", 360 - Lon + 2.5, Lon + 2.5),
                  gr_id = ifelse(gr_id %in% c("L","P","S"), gr_id, "Oth"))
  
  dat.pl <- dat %>% mutate(Gear = recode(gr_id, L = "Longline", P = "Pole-and-line", S = "PS-associated", Oth = "Other"),
                           Gear = ifelse(Gear == "PS-associated" & sch_id < 0, "PS-unclassified", Gear),
                           Gear = ifelse(Gear == "PS-associated" & sch_id %in% 1:2, "PS-free-school", Gear)) %>%
    filter(yy >= 1960, yy <= last_yr)
  
  dat.pl %<>% mutate(reg = NA,
                     reg = ifelse(Lat > 30 & Lat < 50 & Lon > 120 & Lon < 140, "Region 1", reg),
                     reg = ifelse(Lat > 30 & Lat < 35 & Lon > 140 & Lon < 145, "Region 1", reg),
                     reg = ifelse(Lat > 35 & Lat < 50 & Lon > 140 & Lon < 210, "Region 2", reg),
                     reg = ifelse(Lat > 30 & Lat < 35 & Lon > 145 & Lon < 210, "Region 2", reg),
                     reg = ifelse(Lat > 20 & Lat < 30 & Lon > 120 & Lon < 145, "Region 3", reg),
                     reg = ifelse(Lat > 10 & Lat < 20 & Lon > 130 & Lon < 145, "Region 3", reg),
                     reg = ifelse(Lat > 10 & Lat < 30 & Lon > 145 & Lon < 210, "Region 4", reg),
                     reg = ifelse(Lat > -20 & Lat < 20 & Lon > 110 & Lon < 130, "Region 5", reg),
                     reg = ifelse(Lat > -20 & Lat < 10 & Lon > 130 & Lon < 140, "Region 5", reg),
                     reg = ifelse(Lat > -20 & Lat < 0 & Lon > 140 & Lon < 155, "Region 6", reg),
                     reg = ifelse(Lat > -20 & Lat < -5 & Lon > 155 & Lon < 160, "Region 6", reg),
                     reg = ifelse(Lat > 0 & Lat < 10 & Lon > 140 & Lon < 170, "Region 7", reg),
                     reg = ifelse(Lat > -5 & Lat < 0 & Lon > 155 & Lon < 170, "Region 7", reg),
                     reg = ifelse(Lat > -20 & Lat < -5 & Lon > 160 & Lon < 170, "Region 7", reg),
                     reg = ifelse(Lat > -20 & Lat < 10 & Lon > 170 & Lon < 210, "Region 8", reg))
  
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")) %>% group_by(Year = yy, Gear) %>%
    summarise(SKJc = sum(sum_skj_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")))
  
  
#____________________________ 
  # Skipjack total assessment region catch by gear
  windows(3500,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = SKJc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    xlab("Year") + ylab("Skipjack catch (mt)") +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 5)) +
    scale_y_continuous(breaks = seq(0, 2500000, 500000), limits = c(0,2500000), labels = scales::comma) + theme_clean() +
    theme(legend.position = c(0.3,0.91), legend.direction = "horizontal", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid'))
  print(pl)
  savePlot(file = paste0(dir.pth, "/SKJ_Catch_ByGear_WholeArea_Within_Assessment.png"), type = "png")
  dev.off()
  
  
#____________________________   
  # Skipjack individual region catch by gear - 5 regions   
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")) %>% group_by(Year = yy, Gear, Region = reg) %>%
    summarise(SKJc = sum(sum_skj_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Pole-and-line","PS-associated","PS-free-school","Other")))
  
  
  windows(3000,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = SKJc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    facet_wrap(~ Region, ncol = 3, scales = "free_x") +
    xlab("Year") + ylab("Skipjack catch (mt)") +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE, reverse = TRUE)) +
    scale_fill_manual(values = alpha(c("yellow2","lightblue","dodgerblue2","firebrick3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 15)) + theme_clean() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = c(0.83,0.15), legend.direction = "horizontal", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11), legend.background = element_rect(color = NA))
  print(pl)
  savePlot(file = paste0(dir.pth, "/SKJ_Catch_ByGear_ByRegion_Within_Assessment.png"), type = "png")
  dev.off()
  
  
  
#____________________________________________________________________________________________________________
  # Catch bar plots - albacore
  
  dat <- read.csv(file = paste0(dat.pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"))
  
  dat %<>% mutate(Lat = as.numeric(str_extract(lat_short, "[0-9]+")), Lon = as.numeric(str_extract(lon_short, "[0-9]+")),
                  hem.lat = str_extract(lat_short, "[aA-zZ]+"), hem.lon = str_extract(lon_short, "[aA-zZ]+"),
                  Lat = ifelse(hem.lat == "S", -Lat + 2.5, Lat + 2.5), Lon = ifelse(hem.lon == "W", 360 - Lon + 2.5, Lon + 2.5),
                  gr_id = ifelse(gr_id %in% c("L","T"), gr_id, "Oth"))
  
  dat.pl <- dat %>% mutate(Gear = recode(gr_id, L = "Longline", T = "Troll", Oth = "Other")) %>%
    filter(yy >= 1960, yy <= last_yr)
  
  
  r1.sf <- c("POLYGON((140 0, 210 0, 210 -5, 230 -5, 230 -10, 140 -10, 140 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r2.sf <- c("POLYGON((140 -10, 230 -10, 230 -25, 140 -25, 140 -10))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r3.sf <- c("POLYGON((140 -25, 230 -25, 230 -50, 140 -50, 140 -25))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  r4.sf <- c("POLYGON((210 0, 290 0, 290 -50, 230 -50, 230 -5, 210 -5, 210 0))") %>% st_as_sfc(crs=st_crs(eez)) %>% st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
  
  
  dat.pl %<>% mutate(reg = NA,
                     reg = ifelse(Lat > -10 & Lat < 0 & Lon > 140 & Lon < 210, "Region 1", reg),
                     reg = ifelse(Lat > -10 & Lat < -5 & Lon > 210 & Lon < 230, "Region 1", reg),
                     reg = ifelse(Lat > -25 & Lat < -10 & Lon > 140 & Lon < 230, "Region 2", reg),
                     reg = ifelse(Lat > -50 & Lat < -25 & Lon > 140 & Lon < 230, "Region 3", reg),
                     reg = ifelse(Lat > -5 & Lat < 0 & Lon > 210 & Lon < 230, "Region 4", reg),
                     reg = ifelse(Lat > -50 & Lat < 0 & Lon > 230 & Lon < 290, "Region 4", reg))
  
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Troll","Other")) %>% group_by(Year = yy, Gear) %>%
    summarise(ALBc = sum(sum_alb_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Troll","Other")))
  
#____________________________ 
  # Albacore total assessment region catch by gear
  windows(3500,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = ALBc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    xlab("Year") + ylab("Albacore catch (mt)") +
    scale_fill_manual(values = alpha(c("orange","yellow3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 5)) +
    scale_y_continuous(breaks = seq(0, 100000, 10000), limits = c(0,100000), labels = scales::comma) + theme_clean() +
    theme(legend.position = c(0.3,0.91), legend.direction = "horizontal", axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid'))
  print(pl)
  savePlot(file = paste0(dir.pth, "/ALB_Catch_ByGear_WholeArea_Within_Assessment.png"), type = "png")
  dev.off()
  
  
#____________________________   
  # Albacore individual region catch by gear - 5 regions   
  dat.tb <- dat.pl %>% filter(!is.na(reg), Gear %in% c("Longline","Troll","Other")) %>% group_by(Year = yy, Gear, Region = reg) %>%
    summarise(ALBc = sum(sum_alb_c))
  
  dat.tb$Gear = factor(dat.tb$Gear, levels = rev(c("Longline","Troll","Other")))
  
  
  windows(3000,2000)
  pl <- ggplot(dat.tb, aes(x = Year, y = ALBc, fill = Gear)) + geom_bar(stat = "identity", colour = "black", width = 1) +
    facet_wrap(~ Region, ncol = 2, scales = "free_x") +
    xlab("Year") + ylab("Albacore catch (mt)") +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
    scale_fill_manual(values = alpha(c("orange","yellow3","forestgreen"),0.8)) +
    scale_x_continuous(breaks = seq(min(dat.tb$Year), max(dat.tb$Year), 15)) + theme_clean() +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "top", legend.direction = "horizontal", axis.title = element_text(size = 14),
          axis.text = element_text(size = 11), legend.background = element_rect(color = NA))
  print(pl)
  savePlot(file = paste0(dir.pth, "/ALB_Catch_ByGear_ByRegion_Within_Assessment.png"), type = "png")
  dev.off()
  
  
#____________________________________________________________________________________________________________  
  
  