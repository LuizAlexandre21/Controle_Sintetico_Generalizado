############################### Controle Sintetico ################################
################ Pacotes #########################
library(Synth)
library(gsynth)
library(readxl)
library(dplyr)
require(Rcpp)
require(ggplot2)
require(GGally)
require(foreach)
require(doParallel)
require(abind)

############## Manipulando os Dados ###############
local="/home/alexandre/Documentos/Controle Sintetico/Sintetico Robusto/Painel.xlsx"
dados=read_xlsx(local)
dados[is.na(dados)]<-0
dados<-as.data.frame(dados)
data<-dados%>%filter(ID %in% c("11","12","13","14","15","16","29","23","24","35","41","43","42"))
data$ID<-as.numeric(data$ID)

data2<-dados%>%filter(ID %in% c("11","12","13","14","15","16","23","35","41","43","42") & Ano>=1991)

######### Visualizando os Dados #############
panelView(Pobreza~Tratamento,data=dados,index=c("ID","Ano"))
panelView(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`+Escolaridade+Desemprego,data=data,index=c("ID","Ano"))

############## Controle Sintetico ##################

################### Pobreza

############### Desigualdade 
sint1<-dataprep(foo=data,predictors =c("Desigualdade"),predictors.op = "mean",dependent = "Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint1<-synth(data.prep.obj = sint1,optimxmethod = "BFGS")
path.plot(dataprep.res = sint1,synth.res = Cont_sint1)
Cont_tab1<-synth.tab(dataprep.res = sint1,synth.res = Cont_sint1)

############### Desigualdade + Crescimento da Renda 
sint2<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda"),predictors.op = "mean",dependent = "Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint2<-synth(data.prep.obj = sint2,optimxmethod = "BFGS")
path.plot(dataprep.res = sint2,synth.res =Cont_sint2 )
Cont_tab2<-synth.tab(dataprep.res = sint2,synth.res = Cont_sint2)

############### Desigualdade + Crescimento da Renda + Escolaridade
sint3<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda","Escolaridade"),predictors.op = "mean",dependent = "Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint3<-synth(data.prep.obj = sint3,optimxmethod = "BFGS")
path.plot(dataprep.res = sint3,synth.res = Cont_sint3)
Cont_tab3<-synth.tab(dataprep.res = sint3,synth.res = Cont_sint3)

############### Todas as variaveis 
Sint4<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda","Escolaridade","Desemprego"),predictors.op = "mean",dependent = "Pobreza",unit.variable ="ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = c("Estado"),time.predictors.prior = c(1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))
Cont_sint4<-synth(data.prep.obj = Sint4,optimxmethod = "BFGS")
path.plot(dataprep.res = Sint4,synth.res = Cont_sint4)
Cont_tab<-synth.tab(dataprep.res = Sint4,synth.res = Cont_sint4)

####################Extremamente Pobres 
################# Desigualdade
sint5<-dataprep(foo=data,predictors =c("Desigualdade"),predictors.op = "mean",dependent = "Ext. Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint5<-synth(data.prep.obj = sint5,optimxmethod = "BFGS")
path.plot(dataprep.res = sint5,synth.res = Cont_sint5)
Cont_tab5<-synth.tab(dataprep.res = sint5,synth.res = Cont_sint5)

############### Desigualdade + Crescimento da Renda 
sint6<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda"),predictors.op = "mean",dependent = "Ext. Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint6<-synth(data.prep.obj = sint6,optimxmethod = "BFGS")
path.plot(dataprep.res = sint6,synth.res =Cont_sint6)
Cont_tab6<-synth.tab(dataprep.res = sint6,synth.res = Cont_sint6)

############### Desigualdade + Crescimento da Renda + Escolaridade
sint7<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda","Escolaridade"),predictors.op = "mean",dependent = "Ext. Pobreza",unit.variable = "ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = "Estado",time.predictors.prior = (1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))                                                                
Cont_sint7<-synth(data.prep.obj = sint7,optimxmethod = "BFGS")
path.plot(dataprep.res = sint7,synth.res = Cont_sint7)
Cont_tab7<-synth.tab(dataprep.res = sint7,synth.res = Cont_sint7)

############### Todas as variaveis 
sint8<-dataprep(foo=data,predictors =c("Desigualdade","Crescimento de Renda","Escolaridade","Desemprego"),predictors.op = "mean",dependent = "Ext. Pobreza",unit.variable ="ID",time.variable = "Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = c("Estado"),time.predictors.prior = c(1981:2003),time.optimize.ssr = c(1981:2014),time.plot = c(1981:2014))
Cont_sint8<-synth(data.prep.obj = sint8,optimxmethod = "BFGS")
path.plot(dataprep.res=sint8,synth.res = Cont_sint8)
Cont_tab8<-synth.tab(dataprep.res = sint8,synth.res = Cont_sint8)



############## Controle Sintetico Generalizado ###############
########### Pobreza
######## Desigualdade 
out1<-gsynth(Pobreza~Tratamento+Desigualdade,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out1)
plot(out1,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out1,type="loadings")
outw1<-out1$wgt.implied
outb1<-out1$est.beta
outat1<-out1$est.att
outav1<-out1$est.avg
outv1<-out1$sigma2

####### Desigualdade + Crescimento na Renda 
out2<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out2)
plot(out2,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out2,type="loadings")
outw2<-out2$wgt.implied
outb2<-out2$est.beta
outat2<-out2$est.att
outav2<-out2$est.avg

####### Desigualdade + Crescimento na Renda + Escolaridade 
out3<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`+Escolaridade,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out3)
plot(out3,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out3,type="loadings")
outw3<-out3$wgt.implied
outb3<-out3$est.beta
outat3<-out3$est.att
outav3<-out3$est.avg

####### Desigualdade + Crescimento na Renda + Escolaridade + Desemprego
out4<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`+Escolaridade +Desemprego,data=data2,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out4)
plot(out4,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out4,type="loadings")
outw4<-out4$wgt.implied
outb4<-out4$est.beta
outat4<-out4$est.att
outav4<-out4$est.avg

########### Extr. Pobreza 
######## Desigualdade
out5<-gsynth(Pobreza~Tratamento+Desigualdade,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out5)
plot(out5,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out5,type="loadings")
outw5<-out5$wgt.implied
outb5<-out5$est.beta
outat5<-out5$est.att
outav5<-out5$est.avg

######### Desigualdade +Crescimento de Renda
out6<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out6)
plot(out6,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out6,type="loadings")
outw6<-out6$wgt.implied
outb6<-out6$est.beta
outat6<-out6$est.att
outav6<-out6$est.avg

########## Desigualdade + Crescimento de Renda + Escolaridade 
out7<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`+Escolaridade,data=data,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out7)
plot(out7,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out7,type="loadings")
outw7<-out7$wgt.implied
outb7<-out7$est.beta
outat7<-out7$est.att
outav7<-out7$est.avg

########## Desigualdade + Crescimento de Renda + Escolaridade + Desemprego
out8<-gsynth(Pobreza~Tratamento+Desigualdade+`Crescimento de Renda`+Escolaridade+Desemprego,data=data2,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
plot(out8)
plot(out8,type="counterfactual",raw="band",xlab="Time",ylim=c(-5,100))
plot(out8,type="loadings")
outw8<-out8$wgt.implied
outb8<-out8$est.beta
outat8<-out8$est.att
outav8<-out8$est.avg