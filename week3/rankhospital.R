rankhospital<-function(state=NULL,outcome=NULL,num="best")
{
  options(warn=-1)
  r_data<-read.table("C:/Users/DS/Documents/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",header= TRUE,sep=",",colClasses = "character")
  
  r_rank<-subset(r_data,State==state)
  if(length(r_rank[,1])==0)
  {
    stop(paste("Error in best(\"",state,"\",\"",outcome,"\"):invalid state",sep=""))
    
  }
  else
  if (tolower(outcome)=="heart attack"||tolower(outcome)=="heart failure" ||tolower(outcome)=="pneumonia")
    {
    m_rank<-r_rank[,c(2,7,11,17,23)]
    
    m_rank[,3]<-as.numeric(m_rank[,3])
    m_rank[,4]<-as.numeric(m_rank[,4])
    m_rank[,5]<-as.numeric(m_rank[,5])
    if(num=="best")
      num<-1
    if (tolower(outcome)=="heart attack")
    {
      n_rank<-na.omit(m_rank[,c(1,2,3)])
      if(num=="worst")
        num<-length(n_rank[,1])
      hosp<-n_rank[order(n_rank[,3],n_rank[,1]),][num,1]
      hosp
    }  
    if (tolower(outcome)=="heart failure")
    {
      n_rank<-na.omit(m_rank[,c(1,2,4)])
      if(num=="worst")
        num<-length(n_rank[,1])
      hosp<-n_rank[order(n_rank[,3],n_rank[,1]),][num,1]
      hosp
    }
    if (tolower(outcome)=="pneumonia")
    {
      n_rank<-na.omit(m_rank[,c(1,2,5)])
      if(num=="worst")
        num<-length(n_rank[,1])
      hosp<-n_rank[order(n_rank[,3],n_rank[,1]),][num,1]
      hosp
    }
  }
  else{
    stop(paste("Error in best(\"",state,"\",\"",outcome,"\"):invalid outcome",sep=""))
    
  }
  hosp
}
