pollutantmean<-function(directory,pollutant,id=1:332){
  wd<-paste(getwd(),"/",directory,sep="")
  d_file<-NULL
  d_data<-NULL
  file_no<-length(id)
  for (i in 1:file_no)
  {
    if(id[i]<10)
      d_file<-cbind(d_file,paste(wd,"/00",id[i],".csv",sep=""))
    else 
      if (id[i]<100)
        d_file<-cbind(d_file,paste(wd,"/0",id[i],".csv",sep=""))
    else
      d_file<-cbind(d_file,paste(wd,"/",id[i],".csv",sep=""))
  }
  for (i in 1:file_no)
  {
    d_data<-rbind(d_data,read.table(d_file[i],header=TRUE,sep=","))
  }
  round(mean(d_data[pollutant][,],na.rm=TRUE),3)
}