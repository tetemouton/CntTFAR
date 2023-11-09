library(FLR4MFCL)

# Albacore 2021

rundir <- "//penguin/assessments/alb/2021/backupCCJ/ALB21_Projections/" 

gridruns <- list.files(path=rundir, pattern="S", full.names=F, ignore.case=TRUE)

counter = 0

for (i in gridruns){
  
  counter = counter+1
  
  rawrep <- paste0(rundir, i, "/plot-final3.par.rep")
  
  #  readrep <- read.rep(rawrep)
  # dep1<-rowSums(readrep$AdultBiomass)/rowSums(readrep$AdultBiomass.nofish)
  readrep <- read.MFCLRep(rawrep)
  
  # Use seasonMeans to get annual values as opposed to quarterly values
  dep1 <- qts(areaSums(adultBiomass(readrep)))/qts(areaSums(adultBiomass_nofish(readrep)))
  yearSums()
  seasonSums()
  seasonMeans()
  
  
  dep2 <- qts(adultBiomass(readrep))/qts(adultBiomass_nofish(readrep))
  head(as.data.frame(dep2))
  
  
  if (counter==1) {
    # alb.dep<-as.data.frame(cbind(readrep$yrs,dep1,i))
    v1<-as.vector(dimnames(dep1)[2])
    alb.dep<-as.data.frame(cbind(as.numeric(v1$year),dep1,i))
    
  } else {
    alb.dep<-rbind(alb.dep,as.data.frame(cbind(as.numeric(v1$year),dep1,i)))
  }
} # end i
colnames(alb.dep)<-c("yr","Depl","Model")
alb.dep$yr<-as.numeric(as.character(alb.dep$yr))
alb.dep$Depl<-as.numeric(as.character(alb.dep$Depl))
alb.dep$Model<-as.character(alb.dep$Model)
write.csv(alb.dep,paste0(savdir,"data/alb/ALBDepletionTable.csv"))

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

