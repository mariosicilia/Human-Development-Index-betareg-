#parametro dati =dataset
#parametro 2 estremi intervallo
#parametro 3 se T-> mostra correlazione con la risposta
require(reshape2)
require(dplyr)
correlazionevariabili<-function(dati, v=0.85 , HDI=TRUE){
  matricecorr<-cor(dati)
  variabili_correlate<-subset(melt(matricecorr), value > v  & value!=1 | value < -v)
  variabili_correlate<-variabili_correlate[!duplicated(variabili_correlate$value),]
  #variabili correlate alla risposta
  variabili_correlate_a_risposta<-subset(variabili_correlate, Var1=="Human.Development.Index.HDI.2014" | Var2 == "Human.Development.Index.HDI.2014" )
  #variabili correlate tra loro
  altre_variabili_correlate_tra_loro<-subset(variabili_correlate, Var1!= "Human.Development.Index.HDI.2014" & Var2 != "Human.Development.Index.HDI.2014" )
  x<-altre_variabili_correlate_tra_loro
  if(HDI) x<-rbind(variabili_correlate_a_risposta, x)
  return(x)
}

