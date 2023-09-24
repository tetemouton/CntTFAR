library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggthemes)
library(FLR4MFCL)
library(readr)


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


  base_pth <- "./"
  
  alb_rep <- read.MFCLRep("./Data/ALB/plot-final3.par.rep")
  bet_rep <- read.MFCLRep("./Data/BET/plot-09.par.rep")
  skj_rep <- read.MFCLRep("./Data/SKJ/plot-09.par.rep")
  yft_rep <- read.MFCLRep("./Data/YFT/plot-14.par.rep")
  
  
  #cnt <- cnt_vec[3]

  plot_regional_depletion <- function(rep = skj_rep, reg_lst = skj_reg_lst, cnt = "CK", spp = "skj"){
    
    cnt_reg <- reg_lst[[cnt]]
    
    sb <- rep@adultBiomass
    sbf0 <- rep@adultBiomass_nofish
    dep <- sb/sbf0
    
    dep_yr <- as.data.frame(dep) %>% mutate(qtr = as.numeric(season)/4 - .125, yrqtr = year + qtr, region = paste("Region", area)) %>%
              group_by(year, area, region) %>% summarise(data = mean(data))
    
    
    dep_yr$fcl_reg <- ifelse(as.numeric(dep_yr$area) %in% cnt_reg, "focal", "other")
    
    
    sub_dat <- filter(dep_yr, fcl_reg == "focal")
    
    windows(3000,2000)
      pl <- ggplot(dep_yr, aes(x = year, y = data)) + geom_line(linewidth = 1) +
                   geom_rect(data = sub_dat, fill = alpha("springgreen4", 0.005), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + facet_wrap(~ region, ncol = 3) +
                   scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) + xlab("") + ylab("sb/sbf=0") + geom_hline(yintercept = 0.2, colour = alpha("red", 0.7), linetype = 2) +
                   theme_clean()
      print(pl)
      
      savePlot(paste0("Figures/", cnt, "/regional_depletion_", spp, ".png"), type="png")
    dev.off()
    
    }
  
    
    map(cnt_vec, plot_regional_depletion, rep = skj_rep, reg_lst = skj_reg_lst, spp = "skj")
    map(cnt_vec, plot_regional_depletion, rep = bet_rep, reg_lst = bet_reg_lst, spp = "bet")
    map(cnt_vec, plot_regional_depletion, rep = yft_rep, reg_lst = yft_reg_lst, spp = "yft")
    map(cnt_vec, plot_regional_depletion, rep = alb_rep, reg_lst = alb_reg_lst, spp = "alb")
  
  
    
#_______________________________________________________________________________    
# Process the catch statistics to create a table of catches for the eez vs region    
    
    
    wcpfc_dat <- read.csv(file = "./Data/Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv", header = TRUE)
    
    ez_dat <- read.csv(file = "./Data/Ann_Cat_EEZ_ACE.csv", header = TRUE)
    
    yb_dat <- read.csv(file = "./Data/Ann_Cat_Oceans_YB_ACE.csv", header = TRUE)
    
    
    # Convert 5x5 coordinates into number and add 2.5 to put in cell centre for assignment to assessment regions
    wcpfc_dat %<>% mutate(lat = as.numeric(str_sub(lat_short,end = -2)), lon = as.numeric(str_sub(lon_short,end = -2)),
                          lat_hem = str_sub(lat_short,start = -1), lon_hem = str_sub(lon_short,start = -1),
                          lat = ifelse(lat_hem == "S", -lat, lat) + 2.5, lon = ifelse(lon_hem == "W", 360 - lon, lon) + 2.5)
    
    
    wcpfc_dat %<>% mutate(reg_yft = NA,
                          reg_yft = ifelse(lat > 20 & lat < 50 & Lon > 120 & Lon < 210, 1, reg_yft),
                          reg_yft = ifelse(lat > 10 & lat < 20 & Lon > 140 & Lon < 210, 1, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < 20 & Lon > 110 & Lon < 140, 2, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < 0 & Lon > 140 & Lon < 155, 3, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < -5 & Lon > 155 & Lon < 160, 3, reg_yft),
                          reg_yft = ifelse(lat > 0 & lat < 10 & Lon > 140 & Lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -5 & lat < 0 & Lon > 155 & Lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -10 & lat < -5 & Lon > 160 & Lon < 210, 4, reg_yft),
                          reg_yft = ifelse(lat > -40 & lat < -10 & Lon > 140 & Lon < 210, 5, reg_yft))
    
    
    wcpfc_dat %<>% mutate(reg_bet = NA,
                          reg_bet = ifelse(lat > 20 & lat < 50 & Lon > 120 & Lon < 170, 1, reg_bet),
                          reg_bet = ifelse(lat > 10 & lat < 20 & Lon > 140 & Lon < 170, 1, reg_bet),
                          reg_bet = ifelse(lat > 10 & lat < 50 & Lon > 170 & Lon < 210, 2, reg_bet),
                          reg_bet = ifelse(lat > 0 & lat < 10 & Lon > 140 & Lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -5 & lat < 0 & Lon > 155 & Lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < -5 & Lon > 160 & Lon < 170, 3, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 10 & Lon > 170 & Lon < 210, 4, reg_bet),
                          reg_bet = ifelse(lat > -40 & lat < -10 & Lon > 140 & Lon < 170, 5, reg_bet), # This includes 9 but gets overwritten
                          reg_bet = ifelse(lat > -40 & lat < -10 & Lon > 170 & Lon < 210, 6, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 20 & Lon > 110 & Lon < 140, 7, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < 0 & Lon > 140 & Lon < 155, 8, reg_bet),
                          reg_bet = ifelse(lat > -10 & lat < -5 & Lon > 155 & Lon < 160, 8, reg_bet),
                          reg_bet = ifelse(lat > -20 & lat < -15 & Lon > 140 & Lon < 150, 9, reg_bet))
    
    wcpfc_dat %<>% mutate(reg_skj = NA,
                          reg_skj = ifelse(lat > 30 & lat < 50 & Lon > 120 & Lon < 140, 1, reg_skj),
                          reg_skj = ifelse(lat > 30 & lat < 35 & Lon > 140 & Lon < 145, 1, reg_skj),
                          reg_skj = ifelse(lat > 35 & lat < 50 & Lon > 140 & Lon < 145, 2, reg_skj),
                          reg_skj = ifelse(lat > 30 & lat < 50 & Lon > 145 & Lon < 210, 2, reg_skj),
                          reg_skj = ifelse(lat > 20 & lat < 30 & Lon > 120 & Lon < 130, 3, reg_skj),
                          reg_skj = ifelse(lat > 10 & lat < 30 & Lon > 130 & Lon < 145, 3, reg_skj),
                          reg_skj = ifelse(lat > 10 & lat < 30 & Lon > 145 & Lon < 210, 4, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 20 & Lon > 110 & Lon < 130, 5, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 10 & Lon > 130 & Lon < 140, 5, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < -5 & Lon > 155 & Lon < 160, 6, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 0 & Lon > 140 & Lon < 155, 6, reg_skj),
                          reg_skj = ifelse(lat > 0 & lat < 10 & Lon > 140 & Lon < 170, 7, reg_skj),
                          reg_skj = ifelse(lat > -5 & lat < 0 & Lon > 155 & Lon < 160, 7, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 0 & Lon > 160 & Lon < 170, 7, reg_skj),
                          reg_skj = ifelse(lat > -20 & lat < 10 & Lon > 170 & Lon < 210, 8, reg_skj))
    
    
    
    
    
    wcpfc.sf <- c("POLYGON((140 -20,140 -50,230 -50,230 -5,210 -5,210 0,140 0,140 -20))") %>% 
      st_as_sfc(crs=st_crs(eez)) %>% 
      st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
    
    epo.sf <- c("POLYGON((230 -50,230 -5,210 -5,210 0,290 0,290 -50,230 -50))") %>% 
      st_as_sfc(crs=st_crs(eez)) %>% 
      st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
    
    r2.sf <- c("POLYGON((230 -25,230 -10,140 -10,140 -25,230 -25))") %>% 
      st_as_sfc(crs=st_crs(eez)) %>% 
      st_sf(field=c('x','y'), geoms = ., stringsAsFactors=FALSE)
    
    overlap.sf <- c("POLYGON((210 -50,230 -50,230 -5,210 -5,210 -50))") 
    
    
    
    
    
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
    
    
    
    
    get_cnt_catches <- function(cnt = "CK"){
      
      
      
      ez_stat <- ez_dat %>% filter(ez_id == cnt, between(yy, lst_yr - 4, lst_yr)) %>%
                            group_by(yy) %>% summarise(ALB = sum(sum_alb_c), BET = sum(sum_bet_c), SKJ = sum(sum_skj_c), YFT = sum(sum_yft_c)) %>%
                            summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
      
      flg_stat <- ez_dat %>% filter(flag_id == cnt, between(yy, lst_yr - 4, lst_yr)) %>%
                             group_by(yy) %>% summarise(ALB = sum(sum_alb_c), BET = sum(sum_bet_c), SKJ = sum(sum_skj_c), YFT = sum(sum_yft_c)) %>%
                             summarise(ALB = mean(ALB), BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
      

      full_df <- rbind(reg_df,
                       c(paste('Catch in', cnt, 'EEZ'), round(ez_stat[1,])),
                       c(paste('Catch by', cnt, 'flagged in WCPFC-CA'), round(flg_stat[1,])),
                       c(paste('Percent of WCPFC-CA catch by', cnt, 'flagged vessels'), round(flg_stat[1,]/reg_df[1, 2:5]*100, 1))
      )
      
      
      df_write <-apply(full_df, 2, as.character)
      write.csv(df_write, file = (paste0("Figures/", cnt, "/", "catch_summary_table.csv")), row.names = FALSE) 
      
      # xtable(df_write, elthethethe)
      
    }
    
    
    map(cnt_vec, get_cnt_catches)
    
    
    
    
    
  #  save(paste0("Figures/", cnt, "/", "dlhehte.RData"))
    
    
    
    
    
    
    
    
# archiving the below for now as think superseeded by the new function above    
 #  load(file = paste0(base_pth, "Data/yb_ocean_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and ocean ID
 #  load(file = paste0(base_pth, "Data/yb_eez_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and EEZ
 #  
 #  
 #  
 #  trp_tab <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
 #                        summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))
 # 
 # 
 #  trp_tab_gr <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% mutate(gear = ifelse(gear %in% c("L","P","S"), gear, "Other"), gear = factor(gear, levels = c("L","S","P","Other"))) %>%
 #                           group_by(yy, gear) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
 #                           group_by(gear) %>% summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT)) %>% arrange(gear)
 # 
 #  
 # 
 #  make_catch_table <- function(cnt = "CK"){
 #    
 #    #for(i in 1:length(cnt_vec)){
 #  
 #  all_tab_eez <- yb_ez_dat %>% filter(eez == cnt_vec[i], between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
 #                               summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
 #  
 # 
 #  all_tab_flg_tmp <- yb_dat %>% filter(flag == cnt, between(yy, yr1, yr2))
 #  
 #  if(dim(all_tab_flg_tmp)[1] > 0){
 #    
 #    all_tab_flg <- all_tab_flg_tmp %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
 #                                       summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
 #    
 #  } else{
 #    
 #    all_tab_flg <- data.frame(BET = 0, SKJ = 0, YFT = 0, ALB = 0)
 #    
 #  }
 #  
 # write_csv(all_tab_flg_tmp, path = (paste0("Figures/", cnt, "/", "all_tab_flg.csv"))) 
 # }
 #  
 #  
 #  tmp <- map(cnt_vec, make_catch_table)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   