rankall<-function(outcome=NULL,num="best")
{
  options(warn=-1)
  r_data<-read.table("C:/Users/DS/Documents/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",header= TRUE,sep=",",colClasses = "character")
  rank_a<-NULL
  if (tolower(outcome)=="heart attack"||tolower(outcome)=="heart failure" ||tolower(outcome)=="pneumonia")
    {
    m_rank<-r_data[,c(2,7,11,17,23)]
    m_state<-m_rank[!duplicated(m_rank$State),][,2]
    m_state<-m_state[order(m_state)]
    
    m_rank[,3]<-as.numeric(m_rank[,3])
    m_rank[,4]<-as.numeric(m_rank[,4])
    m_rank[,5]<-as.numeric(m_rank[,5])
    if(num=="best")
      num<-1
    if (tolower(outcome)=="heart attack")
    {
      n_rank<-na.omit(m_rank[,c(1,2,3)])
      n_rank<-n_rank[order(n_rank[,2],n_rank[,3],n_rank[,1]),]
      for( i in 1:length(m_state))
      {
        if(num=="worst")
          inde<-length(subset(n_rank,State==m_state[i])[,1])
        hosp<-subset(n_rank,State==m_state[i])[inde,1]
        rank_a<-rbind(rank_a,data.frame(hospital=hosp,state=m_state[i]))
        
      }
    }  
    if (tolower(outcome)=="heart failure")
    {
      n_rank<-na.omit(m_rank[,c(1,2,4)])
      n_rank<-n_rank[order(n_rank[,2],n_rank[,3],n_rank[,1]),]
      for( i in 1:length(m_state))
      {
        if(num=="worst")
          inde<-length(subset(n_rank,State==m_state[i])[,1])
        hosp<-subset(n_rank,State==m_state[i])[inde,1]
        rank_a<-rbind(rank_a,data.frame(hospital=hosp,state=m_state[i]))
        
      }
    }
    if (tolower(outcome)=="pneumonia")
    {
      n_rank<-na.omit(m_rank[,c(1,2,5)])
      n_rank<-n_rank[order(n_rank[,2],n_rank[,3],n_rank[,1]),]
      for( i in 1:length(m_state))
      {
        if(num=="worst")
          inde<-length(subset(n_rank,State==m_state[i])[,1])
        hosp<-subset(n_rank,State==m_state[i])[inde,1]
        rank_a<-rbind(rank_a,data.frame(hospital=hosp,state=m_state[i]))
        
      }
    }
  }
  else{
    stop(paste("Error in best(\"",state,"\",\"",outcome,"\"):invalid outcome",sep=""))
    
  }
  rank_a
}
