# Run in 32bit R
library(RODBC)

# Need to set working directory to where the Data folder is in your git repo e.g.:

  dat_pth <- "C:/GitRep/CntTFAR/2023/Report_template/Data/"

  channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                   SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                   SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")

  
  
  # All gear tuna catch by gear, flag and ocean ID by 5 x 5 from a_best
  dat_yb <- sqlQuery(channellog, query = "SELECT * FROM a_yearbook", max = 0, stringsAsFactors = FALSE)
  
  write.csv(dat_yb, file = paste0(dat_pth, "Ann_Cat_Oceans_YB_ACE.csv"), row.names = FALSE)
  
  
# All gear tuna catch by gear, flag and ocean ID by 5 x 5 from a_best
  dat_abest <- sqlQuery(channellog, query = "SELECT gr_id, ocean_id, flag_id, yy, lat_short, lon_short, sch_id, sum(days), sum(sets), sum(hhooks), sum(stdeff),
                                       sum(alb_c), sum(alb_n), sum(bet_c), sum(bet_n), sum(skj_c), sum(skj_n), sum(yft_c), sum(yft_n) FROM a_best WHERE yy>=1950
                                       GROUP BY gr_id, ocean_id, flag_id, yy, lat_short, lon_short, sch_id", max = 0, stringsAsFactors = FALSE)

  write.csv(dat_abest, file = paste0(dat_pth, "Ann_All-Gear_Cat_Effort_Flg_5x5_AllOceans.csv"), row.names = FALSE)


# All gear catches in eezs from yearbook by eez table
  dat_ez <- sqlQuery(channellog, query = "SELECT yy, gr_id, ez_id, flag_id, in_arch, sum(alb_c), sum(bet_c), sum(skj_c), sum(yft_c)  FROM a_yb_ez WHERE yy > 1999 GROUP BY yy, 
                                          gr_id, ez_id, flag_id, in_arch", max = 0, stringsAsFactors = FALSE)

  write.csv(dat_ez, file = paste0(dat_pth, "Ann_Cat_EEZ_ACE.csv"), row.names = FALSE)











