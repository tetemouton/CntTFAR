library(FLR4MFCL)
library(tidyverse)
library(data.table)

# Albacore 2021

rundir <- "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/" 

gridruns <- list.files(path=rundir, pattern="S", full.names=F, ignore.case=TRUE)


  
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
  
   
   
   full_dep <- map(gridruns, extract_depletion, scl = "full",
                   rundir = "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/",
                   finalpar = "plot-final3.par.rep")
   
   full_dep_df <- rbindlist(full_dep)
   
   full_dat <- full_dep_df %>% group_by(year) %>% summarise(dep = median(data))
   
   
   windows(4000,3000)
     ggplot(full_dat, aes(x = year, y = dep)) + geom_line(linewidth = 2)
   
   
   reg_dep <- map(gridruns, extract_depletion, scl = "regional",
                   rundir = "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/",
                   finalpar = "plot-final3.par.rep")
   
   reg_dep_df <- rbindlist(reg_dep)

   
   
    
    
    
   
   
   
   
   
   
   
   
   
   
   
   
   

#######################
###BIGEYE 2023
#####################
rundir<-"Z:/bet/2023/model_runs/grid/full/"
gridruns <- list.files(path=rundir, pattern="_", full.names=F, ignore.case=T)

gridruns<-gridruns[4:57]

counter=0
for (i in gridruns){
  counter=counter+1
  rawrep <- paste0(rundir,i, "/plot-final.par.rep")
  readrep <- read.MFCLRep(rawrep)
  dep1<-qts(areaSums(adultBiomass(readrep)))/qts(areaSums(adultBiomass_nofish(readrep)))
  if (counter==1) {
    v1<-as.vector(dimnames(dep1)[2])
    bet.dep<-as.data.frame(cbind(as.numeric(v1$year),dep1,i))
    
  } else {
    bet.dep<-rbind(bet.dep,as.data.frame(cbind(as.numeric(v1$year),dep1,i)))
  }
} #end i
colnames(bet.dep)<-c("yr","Depl","Model")
bet.dep$yr<-as.numeric(as.character(bet.dep$yr))
bet.dep$Depl<-as.numeric(as.character(bet.dep$Depl))
bet.dep$Model<-as.character(bet.dep$Model)
write.csv(bet.dep,paste0(savdir,"data/bet/BETDepletionTable.csv"))

#######################
###SKIPJACK
#####################
rundir<-"Z:/skj/2022/Assessment/Model_runs/Grid2022/SKJ_3_18July/"
gridruns <- list.files(path=rundir, pattern="T", full.names=F, ignore.case=T)
#kludge to remove a ,zip dir
gridruns<-gridruns[gridruns != "T2G10.8.zip"]
counter=0
for (i in gridruns){
  counter=counter+1
  rawrep <- paste0(rundir,i, "/plot-09.par.rep")
  readrep <- read.MFCLRep(rawrep)
  dep1<-qts(areaSums(adultBiomass(readrep)))/qts(areaSums(adultBiomass_nofish(readrep)))
  if (counter==1) {
    v1<-as.vector(dimnames(dep1)[2])
    skj.dep<-as.data.frame(cbind(as.numeric(v1$year),dep1,i))
    
  } else {
    skj.dep<-rbind(skj.dep,as.data.frame(cbind(as.numeric(v1$year),dep1,i)))
  }
} #end i
colnames(skj.dep)<-c("yr","Depl","Model")
skj.dep$yr<-as.numeric(as.character(skj.dep$yr))
skj.dep$Depl<-as.numeric(as.character(skj.dep$Depl))
skj.dep$Model<-as.character(skj.dep$Model)
write.csv(skj.dep,paste0(savdir,"data/skj/SKJDepletionTable.csv"))

#######################
###YELLOWFIN 2023
#####################
rundir<-"Z:/yft/2023/model_runs/grid/full/"
gridruns <- list.files(path=rundir, pattern="_", full.names=F, ignore.case=T)

gridruns<-gridruns[2:55]
counter=0
for (i in gridruns){
  counter=counter+1
  rawrep <- paste0(rundir,i, "/plot-final.par.rep")
  readrep <- read.MFCLRep(rawrep)
  dep1<-qts(areaSums(adultBiomass(readrep)))/qts(areaSums(adultBiomass_nofish(readrep)))
  if (counter==1) {
    v1<-as.vector(dimnames(dep1)[2])
    yft.dep<-as.data.frame(cbind(as.numeric(v1$year),dep1,i))
    
  } else {
    yft.dep<-rbind(yft.dep,as.data.frame(cbind(as.numeric(v1$year),dep1,i)))
  }
} #end i
colnames(yft.dep)<-c("yr","Depl","Model")
yft.dep$yr<-as.numeric(as.character(yft.dep$yr))
yft.dep$Depl<-as.numeric(as.character(yft.dep$Depl))
yft.dep$Model<-as.character(yft.dep$Model)
write.csv(yft.dep,paste0(savdir,"data/yft/YFTDepletionTable.csv"))

