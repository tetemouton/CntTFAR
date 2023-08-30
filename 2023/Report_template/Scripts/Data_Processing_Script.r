library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggthemes)
library(FLR4MFCL)
library(readr)


  yr1 <- 2014
  yr2 <- 2022
  
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
  
  
  
  cnt <- cnt_vec[3]
  
  plot_regional_depletion <- function(rep = alb_rep, reg_lst = alb_reg_lst, cnt = "CK", spp = "alb")
  plot_regional_depletion <- function(rep = bet_rep, reg_lst = bet_reg_lst, cnt = "CK", spp = "bet")
  plot_regional_depletion <- function(rep = yft_rep, reg_lst = yft_reg_lst, cnt = "CK", spp = "yft")
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
  
  
  load(file = paste0(base_pth, "Data/yb_ocean_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and ocean ID
  load(file = paste0(base_pth, "Data/yb_eez_gear_dat.RData"), verbose = TRUE)   # Read-in yearbook annual estimates for each species by gear and EEZ
  
  
  
  trp_tab <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
                        summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT))


  trp_tab_gr <- yb_dat %>% filter(ocean == "WX", between(yy, yr1, yr2)) %>% mutate(gear = ifelse(gear %in% c("L","P","S"), gear, "Other"), gear = factor(gear, levels = c("L","S","P","Other"))) %>%
                           group_by(yy, gear) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt)) %>%
                           group_by(gear) %>% summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT)) %>% arrange(gear)

  
 
  all_tab_eez <- yb_ez_dat %>% filter(eez == cnt, between(yy, yr1, yr2)) %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
                               summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
  

  all_tab_flg_tmp <- yb_dat %>% filter(flag == "CK", between(yy, yr1, yr2))
  
  if(dim(all_tab_flg_tmp)[1] > 0){
    
    all_tab_flg <- all_tab_flg_tmp %>% group_by(yy) %>% summarise(BET = sum(bet_mt), SKJ = sum(skj_mt), YFT = sum(yft_mt), ALB = sum(alb_mt)) %>%
                                       summarise(BET = mean(BET), SKJ = mean(SKJ), YFT = mean(YFT), ALB = mean(ALB))
    
  } else{
    
    all_tab_flg <- data.frame(BET = 0, SKJ = 0, YFT = 0, ALB = 0)
    
  }
  
 write_csv(all_tab_flg_tmp, path = (paste0("Figures/", cnt, "/", "all_tab_flg.csv"))) 
  
  
  
  
  
  
  
  
  
   