#########################################
# Initial Test
# Claudio R. Lucinda
# FEA-RP/USP
#########################################

library(xlsx)
library(rbcb)
library(measurements)
library(xts)
library(PortfolioAnalytics)
source("BDT_Fcns.R")
source("Yield_Curve.R")
source("Petro_Losses.R")

USGULF<-read.xlsx("./USGulf.xlsx",sheetName="Planilha1",stringsAsFactors=F)
USGULF<-xts(USGULF, order.by=USGULF$NA.)[,-1]

CBIO<-get_series(10813)
CBIO<-xts(CBIO,order.by=CBIO$date)[,-1]
Data<-merge(USGULF,CBIO, join="inner")
mode(Data)<-"numeric"

conv_factor<-conv_unit(1, "us_gal", "l")

# Fonte: http://www.mme.gov.br/documents/1138769/0/Relat%C3%B3rio+mensal+de+mercado+abr-18+148.pdf/009d4da0-688b-47d0-aa1c-23805bc8fb41
Custo_Inter<-.0533

Opp_Cost<-Data$US.Gulf.Diesel*Data$X10813/conv_factor
Opp_Cost$US.Gulf.Diesel.Plus<-Opp_Cost$US.Gulf.Diesel+Custo_Inter

df_returns <- function(.df) {
  df_temp<-.df
  for (nom in colnames(.df)) {
    df_temp[,nom]<-Return.calculate(df_temp[,nom])
    
  }
  return(df_temp)
}

Returns.OppCost<-df_returns(Opp_Cost)[-1,]

# Preço Diesel Hoje Petro 
# Fonte:
# http://www.petrobras.com.br/pt/produtos-e-servicos/composicao-de-precos-de-venda-as-distribuidoras/gasolina-e-diesel/


Vol_dia<-StdDev(Returns.OppCost)

S0<-Opp_Cost$US.Gulf.Diesel.Plus["2018-05-21"]
# Alternativa: 
# S0<-2.1016
Vol_dia<-Vol_dia[1,2]

Big_T<-30
Nr_Period<-10

Coefs_ANBIMA<-c(0.132017444,-0.063730418,-0.0894560710070474,-0.0302417251630926,1.180928869,-0.0302417251630926)

venda_mensal_Diesel<-4128.80*1e6

save(Vol_dia,Coefs_ANBIMA,venda_mensal_Diesel, file="Add_Data.RDS")

Losses_Litro<-Petro_Losses(.Big_T=Big_T,.Nr_Period = Nr_Period,.Coefs_YC = Coefs_ANBIMA,.Vol_dia = Vol_dia,.desconto =.46,.S0=2.1016)
