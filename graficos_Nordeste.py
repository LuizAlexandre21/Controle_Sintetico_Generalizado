############ Graficos ############
import pandas as pd
import matplotlib.pyplot as plt 
import  numpy as np 

######## Importando os dados #########
local="Bolsa Familia.xlsx"
dados=pd.read_excel(local,sheet_name='Sheet3')
dados['Proporção']=dados['Valor total de Beneficos']/dados['Numero de Beneficios']
######## Grupos de comparação ########
Maranhão=dados[dados['ID']==21]
Piauí=dados[dados['ID']==22]
Ceará=dados[dados['ID']==23]
Rio_Grande=dados[dados['ID']==24]
Paraiba=dados[dados['ID']==25]
Pernambuco=dados[dados['ID']==26]
Alagoas=dados[dados['ID']==27]
Sergipe=dados[dados['ID']==28]
Bahia=dados[dados['ID']==29]
Minimos=[80,80,80,80,80,80,80,80,80,80,80]
######## Graficos 
#### Maranhão 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Maranhão x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Maranhão['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Maranhão['Numero de Beneficios'])
axes[1,0].plot(ind,Maranhão['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Maranhão','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Maranhão")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Maranhão['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Maranhão','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Piauí 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Piauí x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Piauí['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Piauí['Numero de Beneficios'])
axes[1,0].plot(ind,Piauí['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Piauí','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Piauí")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Piauí['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Piauí','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Rio Grande do Norte
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Rio Grande do Norte x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Rio_Grande['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Rio_Grande['Numero de Beneficios'])
axes[1,0].plot(ind,Rio_Grande['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Rio Grande do Norte','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Pernambuco")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Rio_Grande['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Rio Grande do Norte','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Paraiba 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Paraiba x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Paraiba['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Paraiba['Numero de Beneficios'])
axes[1,0].plot(ind,Paraiba['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Paraiba','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Paraiba")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Paraiba['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Paraiba','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Pernambuco 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Pernambuco x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Pernambuco['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Pernambuco['Numero de Beneficios'])
axes[1,0].plot(ind,Pernambuco['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Pernambuco','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Pernambuco")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Pernambuco['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Pernambuco','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Alagoas 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Alagoas x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Alagoas['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Alagoas['Numero de Beneficios'])
axes[1,0].plot(ind,Alagoas['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Alagoas','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Alagoas")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Alagoas['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Alagoas','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Segipe 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia - Sergipe x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Sergipe['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Sergipe['Numero de Beneficios'])
axes[1,0].plot(ind,Sergipe['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Sergipe','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Sergipe")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Sergipe['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Sergipe','Ceará','Valor minimo do bolsa familia'])
plt.show()

#### Bahia 
width=0.35
ind=np.arange(11)
fig=plt.figure()
fig.suptitle("Repasses do Bolsa Familia -Bahia x Ceará")
axes=fig.subplots(nrows=2,ncols=2)
axes[0,0].set_title("Valor Total de Beneficios")
axes[0,0].set_xlabel("Anos")
axes[0,0].set_ylabel("Valor em 1e08 R$") 
axes[0,0].set_xticks(ind)
axes[0,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,0].bar(ind,height=Bahia['Valor total de Beneficos'])
axes[0,1].set_title("Numero de Beneficios")
axes[0,1].set_xlabel("Anos")
axes[0,1].set_ylabel("Quantidade de Pessoas")
axes[0,1].set_xticks(ind)
axes[0,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[0,1].bar(ind,Bahia['Numero de Beneficios'])
axes[1,0].plot(ind,Bahia['Proporção'],ind,Minimos,'r--')
axes[1,0].set_title("Proporção")
axes[1,0].set_xlabel("Anos")
axes[1,0].set_ylabel("Valor per Familia")
axes[1,0].legend()
axes[1,0].set_xticks(ind)
axes[1,0].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,0].legend(['Bahia','Valor minimo do bolsa familia'])
axes[1,1].set_title("Proporção - Ceará x Bahia")
axes[1,1].set_xlabel("Anos")
axes[1,1].set_ylabel("Valor per Familia")
axes[1,1].set_xticks(ind)
axes[1,1].plot(ind,Bahia['Proporção'],ind,Ceará['Proporção'],ind,Minimos,'r--')
axes[1,1].set_xticklabels(('2004', '2005', '2006', '2007', '2008','2009','2010','2011','2012','2013','2014'))
axes[1,1].legend(['Bahia','Ceará','Valor minimo do bolsa familia'])
plt.show()

