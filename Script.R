###########Pacotes ###########
library(gsynth)
library(readxl)
library(panelView)
require(Rcpp)
require(ggplot2)
require(GGally)
require(foreach)
require(doParallel)
require(abind)
library(dplyr)
###########Importando os dados ###########
Painel<-read_xlsx("Painel.xlsx")
Painel=as.data.frame(Painel)
Painel[is.na(Painel)]=0 
Painelc<-Painel%>%filter(Ano>=1991)
###########Visualizando os dados #########
panelView(Pobreza~Tratamento,data=Painel,index=c('ID','Ano'))

#############Estimando Resultados ##########
######### Pobreza 
########Parametrico 
###### Escolaridade
gsynth1<-gsynth(Pobreza~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade,data=Painel,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=3000,inference='parametric',parallel=TRUE)
plot(gsynth1)
plot(gsynth1,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth1,type="loadings")
write.csv(gsynth1$wgt.implied,"gsynth1wgt.csv")
write.csv(gsynth1$est.beta,"gsynth1beta.csv")
write.csv(gsynth1$est.att,"gsynth1att.csv")
write.csv(gsynth1$est.avg,"gsynth1avg.csv")
###### Escolaridade + Desemprego 
gsynth2<-gsynth(Pobreza~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade+Desemprego,data=Painelc,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='parametric',parallel=TRUE)
plot(gsynth2)
plot(gsynth2,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth2,type="loadings")
write.csv(gsynth2$wgt.implied,"gsynth2wgt.csv")
write.csv(gsynth2$est.beta,"gsynth2beta.csv")
write.csv(gsynth2$est.att,"gsynth2att.csv")
write.csv(gsynth2$est.avg,"gsynth2avg.csv")
######### Não Parametrico
######Escolaridade
gsynth3<-gsynth(Pobreza~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade,data=Painel,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='nonparametric',parallel=TRUE)
plot(gsynth3)
plot(gsynth3,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth3,type="loadings")
write.csv(gsynth3$wgt.implied,"gsynth3wgt.csv")
write.csv(gsynth3$est.beta,"gsynth3beta.csv")
write.csv(gsynth3$est.att,"gsynth3att.csv")
write.csv(gsynth3$est.avg,"gsynth3avg.csv")
###### Escolaridade + Desemprego 
gsynth4<-gsynth(Pobreza~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade+Desemprego,data=Painelc,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='nonparametric',parallel=TRUE)
plot(gsynth4)
plot(gsynth4,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth4,type="loadings")
write.csv(gsynth4$wgt.implied,"gsynth4wgt.csv")
write.csv(gsynth4$est.beta,"gsynth4beta.csv")
write.csv(gsynth4$est.att,"gsynth4att.csv")
write.csv(gsynth4$est.avg,"gsynth4avg.csv")
#########Extrema Pobreza 
########Parametrico 
###### Escolaridade
gsynth5<-gsynth(`Ext. Pobreza`~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade,data=Painel,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='parametric',parallel=TRUE)
plot(gsynth5)
plot(gsynth5,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth5,type="loadings")
write.csv(gsynth5$wgt.implied,"gsynth5wgt.csv")
write.csv(gsynth5$est.beta,"gsynth5beta.csv")
write.csv(gsynth5$est.att,"gsynth5att.csv")
write.csv(gsynth5$est.avg,"gsynth5avg.csv")
###### Escolaridade + Desemprego 
gsynth6<-gsynth(`Ext. Pobreza`~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade+Desemprego,data=Painelc,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='parametric',parallel=TRUE)
plot(gsynth6)
plot(gsynth6,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth6,type="loadings")
write.csv(gsynth6$wgt.implied,"gsynth6wgt.csv")
write.csv(gsynth6$est.beta,"gsynth6beta.csv")
write.csv(gsynth6$est.att,"gsynth6att.csv")
write.csv(gsynth6$est.avg,"gsynth6avg.csv")
######### Não Parametrico
######Escolaridade
gsynth7<-gsynth(`Ext. Pobreza`~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade,data=Painel,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=10000,inference='nonparametric',parallel=TRUE)
plot(gsynth7)
plot(gsynth7,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth7,type="loadings")
write.csv(gsynth7$wgt.implied,"gsynth7wgt.csv")
write.csv(gsynth7$est.beta,"gsynth7beta.csv")
write.csv(gsynth7$est.att,"gsynth7att.csv")
write.csv(gsynth7$est.avg,"gsynth7avg.csv")
###### Escolaridade + Desemprego 
gsynth8<-gsynth(`Ext. Pobreza`~Tratamento+Desigualdade+´Crescimento de Renda´+Escolaridade+Desemprego,data=Painelc,index=c('ID','Ano'),force='two-way',EM=TRUE,CV=TRUE,se=TRUE,nboots=100000,inference='nonparametric',parallel=TRUE)
plot(gsynth8)
plot(gsynth8,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(gsynth8,type="loadings")
write.csv(gsynth8$wgt.implied,"gsynth8wgt.csv")
write.csv(gsynth8$est.beta,"gsynth8beta.csv")
write.csv(gsynth8$est.att,"gsynth8att.csv")
write.csv(gsynth8$est.avg,"gsynth8avg.csv")
