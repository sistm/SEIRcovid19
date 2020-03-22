rm(list = ls())
date<-"20200322"
  
#Get the data by region
data_region_by_region<-get_data_covid19_bylevel(level = "region", source3="SPF",update_from_source = TRUE)

#Concatenate all dataset
multi_join <- function(list_of_loaded_data, join_func, ...){
  
  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
  
  return(output)
}
data_region<-multi_join(data_region_by_region,rbind)

#population size from https://fr.wikipedia.org/wiki/R%C3%A9gion_fran%C3%A7aise
# names<-paste("REG-",c("84","27","53","24","94","44","32","11","28","75","76","52","93","01","02","03","04","06"),sep="")
# goodid<-c("AURA","BFC","Bretagne","Centre","Corse","GrandEst","HDF","IDF","Normandie","NAquitaine","Occitanie","PaysLoire","PACA","Guadeloupe","Martinique","Guyane","Reunion","Mayotte")
# size<-c(8026685,2795301,3329395,2566759,339178,5518188,5978266,12213364,3319067,5987014,5892817,3786545,5059473,382704,364354,296711,866506,270372)
# ICUnb<-c(559,198,162,180,18,465,438,1147,240,412,474,181,460,10,10,10,10,10)
# region_size<-data.frame(names, size,goodid,ICUnb)
# write.table(region_size,file="./monolix/Chiffres.txt",sep="\t",row.names = F,quote=F)

region_size<-read.table("./monolix/Chiffres.txt",sep="\t",header=TRUE)

#Create Suitable variables
for(i in 1:length(data_region[,"maille_code"])){
  data_region$popsize[i]<-region_size[which(region_size$names==as.character(data_region$maille_code[i])),"size"]
  data_region$goodID[i]<-as.character(region_size[which(region_size$names==as.character(data_region$maille_code[i])),"goodid"])
  if(data_region$day[i]==0){
    data_region$initinfectious[i]<-data_region$cas_confirmes_incident[i]
  }else{
    data_region$initinfectious[i]<-data_region$initinfectious[i-1]
  }
}
for(i in 1:length(data_region[,"maille_code"])){
  data_region$cumul[i]<-sum(data_region$cas_confirmes_incident[which(data_region$goodID==data_region$goodID[i])])
} 
head(data_region)
#Supress regions for which I0=0 à t=0
supression<-unique(data_region$goodID[which(data_region$initinfectious==0)])
print(cat("Regions supprimees:", supression))
data_region<-data_region[which(!data_region$goodID%in%supression),]
#Supress regions for which I0=0 à t=0
supression<-unique(data_region$goodID[which(data_region$cumul<50)])
print(cat("Regions supprimees:", supression))
data_region<-data_region[which(!data_region$goodID%in%supression),]
data_region$cas_confirmes_incident<-round(ifelse(data_region$cas_confirmes_incident<0,0,data_region$cas_confirmes_incident),0)
write.table(data_region,file=paste("./monolix/data_region_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)

data_region_monolix2y<-data_region[1,]
data_region_monolix2y$observation[1]<-data_region_monolix2y$cas_confirmes_incident[1]
data_region_monolix2y$observationid[1]<-1
if((!is.na(data_region$hospitalisation_incident[1]))){
    data_region_monolix2y[2,]<-c(data_region[1,],data_region_monolix2y$hospitalisation_incident[1],2)
}else{
  if((data_region$day[1]==0)){
    data_region_monolix2y[2,]<-c(data_region[1,],0,2)
  }else{
  data_region_monolix2y[2,]<-NA
  }
}
k<-2
for (i in 3:(2*length(data_region$date))){
  if(i%%2==0){
    if((!is.na(data_region$hospitalisation_incident[k]))){
    data_region_monolix2y[i,]<-c(data_region[k,],data_region$hospitalisation_incident[k],2)
    }else{
      if((data_region$day[k]==0)){
        data_region_monolix2y[i,]<-c(data_region[k,],0,2)
      }else{
      data_region_monolix2y[i,]<-NA}
    }
    k<-k+1
  }else{
    data_region_monolix2y[i,]<-c(data_region[k,],data_region$cas_confirmes_incident[k],1)
  }
}
data_region_monolix2y<-data_region_monolix2y[which(!is.na(data_region_monolix2y$goodID)),]
head(data_region_monolix2y,n=20)
for(i in 1:length(data_region_monolix2y[,"maille_code"])){
  if(data_region_monolix2y$day[i]==0){
    if(is.na(data_region_monolix2y$hospitalisation_incident[i])|(data_region_monolix2y$hospitalisation_incident[i]==0)){
      data_region_monolix2y$initH[i]<-1
    }else{
      data_region_monolix2y$initH[i]<-data_region$hospitalisation_incident[i]
    }
  }else{
    data_region_monolix2y$initH[i]<-data_region_monolix2y$initH[i-1]
  }
}

write.table(data_region_monolix2y,file=paste("./monolix/data_region_2y_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)
