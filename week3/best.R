best<-function(state=NULL,outcome=NULL)
{
  options(warn=-1)
  r_data<-read.table("C:/Users/DS/Documents/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",header= TRUE,sep=",")
  
  r_rank<-subset(r_data,State==state)
  if(length(r_rank[,1])==0)
  {
    error_message<-paste("Error in best(\"",state,"\",\"",outcome,"\"):invalid state",sep="")
    message(error_message)
    stop
  }
  else
  if (tolower(outcome)=="heart attack"||tolower(outcome)=="heart failure" ||tolower(outcome)=="pneumonia")
    {
    m_rank<-r_rank[,c(2,7,11,17,23)]
  ##  for (i in 1:length(m_rank[,1]))
  ##  {
  ##    m_rank[i,3]<-as.numeric(as.character(m_rank[i,3]))
  ##    m_rank[i,4]<-as.numeric(as.character(m_rank[i,4]))
  ##    m_rank[i,5]<-as.numeric(as.character(m_rank[i,5]))
  ##  }
    if (tolower(outcome)=="heart attack")
        hosp<-as.character(m_rank[order(m_rank[,3],na.last=TRUE),][1,1])
    if (tolower(outcome)=="heart failure")
      hosp<-as.character(m_rank[order(m_rank[,4],na.last=TRUE),][1,1])
    if (tolower(outcome)=="pneumonia")
      hosp<-as.character(m_rank[order(m_rank[,5],na.last=TRUE),][1,1])
    hosp
  }
  else{
    error_message<-paste("Error in best(\"",state,"\",\"",outcome,"\"):invalid outcome",sep="")
    message(error_message)
  }
}
