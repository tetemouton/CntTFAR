library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggthemes)
library(FLR4MFCL)
library(readr)


  #yr1 <- 2014
  lst_yr <- 2022
  
  cnt_vec <- c("CK","FJ","NC","TO")
  
  
  skj_reg_lst <- list("CK" = c(1,2),
                      "FJ" = c(3),
                      "NC" = c(4,5),
                      "TO" = c(2,3))
  
  alb_reg_lst <- list("CK" = c(1,2),
                      "FJ" = c(2),
                      "NC" = c(2,3),
                      "TO" = c(2,3))
  
  yft_reg_lst <- list("CK" = c(4,6),
                      "FJ" = c(6),
                      "NC" = c(5),
                      "TO" = c(6))
  
  bet_reg_lst <- list("CK" = c(4,5),
                      "FJ" = c(5),
                      "NC" = c(5),
                      "TO" = c(5))


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
      
    }
    
    
    map(cnt_vec, get_cnt_catches)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  load(file = paste0(base_pth, "Data/yb_ocean_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and ocean ID
  load(file = paste0(base_pth, "Data/yb_eez_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and EEZ
  
  
  
  trp_tab <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
                        summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))


  trp_tab_gr <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% mutate(gear = ifelse(gear %in% c("L","P","S"), gear, "Other"), gear = factor(gear, levels = c("L","S","P","Other"))) %>%
                           group_by(yy, gear) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
                           group_by(gear) %>% summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT)) %>% arrange(gear)

  
 
  make_catch_table <- function(cnt = "CK"){
    
    #for(i in 1:length(cnt_vec)){
  
  all_tab_eez <- yb_ez_dat %>% filter(eez == cnt_vec[i], between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
                               summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
  

  all_tab_flg_tmp <- yb_dat %>% filter(flag == cnt, between(yy, yr1, yr2))
  
  if(dim(all_tab_flg_tmp)[1] > 0){
    
    all_tab_flg <- all_tab_flg_tmp %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
                                       summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
    
  } else{
    
    all_tab_flg <- data.frame(BET = 0, SKJ = 0, YFT = 0, ALB = 0)
    
  }
  
 write_csv(all_tab_flg_tmp, path = (paste0("Figures/", cnt, "/", "all_tab_flg.csv"))) 
 }
  
  
  tmp <- map(cnt_vec, make_catch_table)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   