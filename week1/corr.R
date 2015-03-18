corr<-function(directory,threshold=0){
  wd<-paste(getwd(),"/",directory,sep="")
  d_file<-NULL
  d_data<-NULL
  d_comp<-NULL
  vc<-NULL
  file_no<-332   ##length(id)
  for (i in 1:file_no)
  {
    if(i<10)
      d_file<-cbind(d_file,paste(wd,"/00",i,".csv",sep=""))
    else 
      if (i<100)
        d_file<-cbind(d_file,paste(wd,"/0",i,".csv",sep=""))
    else
      d_file<-cbind(d_file,paste(wd,"/",i,".csv",sep=""))
  }
  for (i in 1:file_no)
  {
    d_data<-rbind(d_data,read.table(d_file[i],header=TRUE,sep=","))
  }
  d_complete<-na.omit(d_data)
  for (i in 1:file_no)
   {
   d_comp<- rbind(d_comp,data.frame(ID=i,nobs=length(subset(d_complete,ID==i)[,1])))
   }
  ds_file<-subset(d_comp,nobs>=threshold)
  tt<-length(ds_file[,1])
  if (tt>0)
  {
    for (i in 1:tt)
    {
      fileid<-ds_file[i,1]
      sub_set<-subset(d_complete,ID==fileid)
      co<-cor(sub_set[,2],sub_set[,3])
      vc<-cbind(vc,co)
    }
    vc<-as.vector(vc)
  }
 else 
   vc<-as.vector(0)
vc
}