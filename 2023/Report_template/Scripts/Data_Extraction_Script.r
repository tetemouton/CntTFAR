# Run in 32bit R
library(RODBC)


  dat_pth <- "P:/OFPSAM/Cty_status_reports/ISNRs/Country_TFAR/2023/Report_template/Data/"


  dbcon <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                             SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                             SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")


  tp <- sqlTables(dbcon)
  
  yb_tb <- sqlColumns(dbcon, "a_yearbook")
  yb_ez_tb <- sqlColumns(dbcon, "a_yb_ez")
  
  # Get yearbook annual estimates for each species by gear and ocean ID
  yb_dat <- sqlQuery(dbcon, "SELECT gr_id as gear, yy, flag_id as flag, fleet_id as fleet, ocean_id as ocean, sum(alb_c) as alb_mt, sum(bet_c) as bet_mt, sum(skj_c) as skj_mt,
                            sum(yft_c) as yft_mt, sum(bum_c) as bum_mt, sum(mls_c) as mls_mt, sum(swo_c) as swo_mt, sum(fal_c) as fal_mt, sum(ocs_c) as ocs_mt,
                            sum(mak_c) as mak_mt, sum(bsh_c) as bsh_mt
                            FROM a_yearbook 
                            WHERE yy >= 2010 GROUP BY yy, flag_id, fleet_id, gr_id, ocean_id",
                     stringsAsFactors = FALSE)
  
  save(yb_dat, file = paste0(dat_pth, "yb_ocean_gear_dat.RData"))

  
  # Get yearbook annual estimates for each species by gear and EEZ
  yb_ez_dat <- sqlQuery(dbcon, "SELECT gr_id as gear, yy, flag_id as flag, fleet_id as fleet, ez_id as eez, sum(alb_c) as alb_mt, sum(bet_c) AS bet_mt, sum(skj_c) as skj_mt, sum(yft_c) AS yft_mt,
                               sum(bum_c) AS bum_mt, sum(mls_c) AS mls_mt, sum(swo_c) AS swo_mt
                               FROM a_yb_ez WHERE yy >= 2010 GROUP BY yy, flag_id, fleet_id, gr_id, ez_id",
                        stringsAsFactors = FALSE)
  
  yb_ez_dat$eez <- ifelse(yb_ez_dat$eez %in% c("GL","LN","PX"), "KI", yb_ez_dat$eez)
  yb_ez_dat$eez <- ifelse(yb_ez_dat$eez %in% c("MA","NC"), "NC", yb_ez_dat$eez)

  save(yb_ez_dat, file = paste0(dat_pth, "yb_eez_gear_dat.RData"))


  odbcCloseAll()









