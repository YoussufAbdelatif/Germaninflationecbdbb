setwd("C:/Users/Yussuf Schwarz/OneDrive/Desktop/UniWiwi/710 - Applied Econometrics/TermPaper")

library(readxl)
library(zoo)
library(ggplot2)
library(gridExtra)
library(bruceR)
library(xtable)
library(stargazer)
library(gmm)
library(dplyr)
library("ivreg")

#######################DATA#####################################################

#Loading data:
#2 time periods: dbb for Deutsche Bank and ecb for European Central Bank

CPI = read.csv("CPIg.csv")
names(CPI)=c("Date","CPI Germany")
CPI$`CPI Germany`=gsub(CPI$`CPI Germany`,pattern = ",",replacement = ".")
CPI$`CPI Germany`=as.numeric(CPI$`CPI Germany`)
#CPI$`CPI Germany`= c((diff(log(CPI$`CPI Germany`),differences = 1))*100,NA)
CPI=na.omit(CPI)
CPIddb = CPI[121:468,]
CPIecb = CPI[469:720,]

GDP_norm = read.csv("GDP_normalized_Germany.csv",sep = ",",header = T)
names(GDP_norm)=c("Date","GDP_norm_Germany")
GDP_norm$GDP_norm_Germany=GDP_norm$GDP_norm_Germany-100
GDP_norm_dbb = GDP_norm[120:467,]
GDP_norm_ecb = GDP_norm[468:719,]

spread=read.csv("Interest_rate_spread_ Germany.csv",header = T)
names(spread)=c("Date","Spread")
spreaddbb=spread[121:468,]
spreadecb=spread[469:720,]

rateecb=read_excel("Leitzins_Hauptref_EZB.xlsx")
names(rateecb)=c("Date","Interest Rate ECB")
rateecb$`Interest Rate ECB`=gsub(rateecb$`Interest Rate ECB`,pattern = ",",replacement = ".")
rateecb=rateecb[1:252,]

ratedbb=read_excel("Leitzins_Lombard_DBB.xlsx")
names(ratedbb)=c("Date","Interest Rate DBB")
ratedbb$`Interest Rate DBB`=gsub(ratedbb$`Interest Rate DBB`,pattern = ",",replacement = ".")
ratedbb=ratedbb[259:606,]

M2_absolute_dbb = read.csv("M2_absolute_germany_69-98.csv",sep = ",",header = T)
names(M2_absolute_dbb)=c("Date","M2_absolute")
M2_growth_dbb=data.frame(cbind(M2_absolute_dbb$Date[-360],(diff(M2_absolute_dbb$M2_absolute,differences = 1)/M2_absolute_dbb$M2_absolute[-360])*100))
names(M2_growth_dbb)=c("Date","M2_growth_dbb")
M2_growth_dbb=M2_growth_dbb[12:359,]

M2_growth_ecb=read.csv("M2_growth_rate_euro_area.csv",sep = ",",header = F)
M2_growth_ecb=M2_growth_ecb[,-c(3,4)]
names(M2_growth_ecb)=c("Date","M2_growth_ecb")
M2_growth_ecb=M2_growth_ecb[order(nrow(M2_growth_ecb):1),]
rownames(M2_growth_ecb)=NULL
M2_growth_ecb=M2_growth_ecb[217:468,]

prodindex=read_excel("Produktionsindex_Industrie.xlsx")
prodindex=prodindex[-3]
names(prodindex)=c("Date","Productionindex")
prodindex$Productionindex=gsub(prodindex$Productionindex,pattern = ",",replacement = ".")
prodindex$Productionindex=as.numeric(prodindex$Productionindex)
prodindex$Productionindex= c(diff(prodindex$Productionindex)/prodindex$Productionindex[873],NA)
prodindexdbb=prodindex[241:588,]
prodindexecb=prodindex[589:840,]

unemployment=read_excel("Unemployment Rate.xlsx")
names(unemployment)=c("Date","Unemployment Rate")
unemployment$`Unemployment Rate`=gsub(unemployment$`Unemployment Rate`,pattern = ",",replacement = ".")
unemploymentdbb=unemployment[242:589,]
unemploymentecb=unemployment[590:841,]

################################################################################

#######################GRAPHS###################################################

#Main variables:

ratege = c(ratedbb$`Interest Rate DBB`,rateecb$`Interest Rate ECB`)
datge = data.frame(cbind(CPI[121:720,]$Date,CPI[121:720,]$`CPI Germany`,ratege,GDP_norm[120:719,]$GDP_norm_Germany,as.numeric(c(prodindexdbb$Productionindex,prodindexecb$Productionindex)),as.numeric(c(spreaddbb$Spread,spreadecb$Spread)),as.numeric(c(M2_growth_dbb$M2_growth_dbb,M2_growth_ecb$M2_growth_ecb)),as.numeric(c(unemploymentdbb$`Unemployment Rate`,unemploymentecb$`Unemployment Rate`))))
names(datge)=c("Datum","CPI","RR","gap","prod","spread","M2","unem")
datge$CPI=as.numeric(datge$CPI)
datge$RR=as.numeric(datge$RR)
datge$gap=as.numeric(datge$gap)
datge$Datum=as.Date(as.yearmon(datge$Datum))

plot1=ggplot(datge, aes(x = Datum)) +
  geom_line(aes(y = CPI, colour = "black"), size = 0.8) +
  geom_vline(aes(xintercept=Datum[349],color="blue"),show.legend =F)+
  geom_hline(aes(yintercept=0,color="red"))+
  scale_color_identity(name = NULL, 
                       labels = c(red="0",black = "CPI",blue="ECB replaces DBB: 1999-01"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Consumer Price Index All Products Germany", subtitle=" 2015 = 100",
       y     = "CPI",x     = "Date")

plot2=ggplot(datge, aes(x = Datum)) +
  geom_line(aes(y = RR, colour = "black"), size = 0.8) +
  geom_vline(aes(xintercept=Datum[349],color="blue"),show.legend =F)+
  geom_hline(aes(yintercept=0,color="red"))+
  scale_color_identity(name = NULL, 
                       labels = c(red="0",black = "Interest Rate", blue="ECB replaces DBB: 1999-01"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Interest Rate", subtitle="1970-1998: DBB Lombard Rate; 1999-2019: ECB Main Refi Rate",
       y     = "Interest Rate in %",
       x     = "Date")

plot3=ggplot(datge, aes(x = Datum)) +
  geom_line(aes(y = gap, col = "black"), size = 0.8) +
  geom_hline(aes(yintercept=0,color="red"))+
  geom_vline(aes(xintercept=Datum[349],color="blue"),show.legend =F )+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Output gap",red="0",blue="ECB replaces DBB: 1999-01"),guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Output Gap", subtitle="Gross Domestic Product (GDP): Normalized for Germany",
       y     = "Index",
       x     = "Date")

g <- arrangeGrob(plot1, plot2, plot3, ncol=3)
ggsave(file="plot1.png", g,scale = 1,width = 18,height = 5,device='png', dpi=500)

#Descriptive statistics:

datdbb = data.frame(cbind(as.numeric(CPIddb$`CPI Germany`),GDP_norm_dbb$GDP_norm_Germany,spreaddbb$Spread,as.numeric(ratedbb$`Interest Rate DBB`),as.numeric(M2_growth_dbb$M2_growth_dbb),as.numeric(prodindexdbb$Productionindex),as.numeric(unemploymentdbb$`Unemployment Rate`)))
names(datdbb)=c("CPI","Output Gap","Spread","Interest Rate","M2_growth","Productionindex","Unemployment")

stargazer(datdbb,out = "Summarydbb.html",title = "Summary Statistics Deutsche Bank \\n\ 1970-1998",digits = 2)

datecb = data.frame(cbind((CPIecb$`CPI Germany`),GDP_norm_ecb$GDP_norm_Germany,as.numeric(spreadecb$Spread),as.numeric(rateecb$`Interest Rate ECB`),M2_growth_ecb$M2_growth_ecb,as.numeric(prodindexecb$Productionindex),as.numeric(unemploymentecb$`Unemployment Rate`)))
names(datecb)=c("CPI","Output Gap","Spread","Interest Rate","M2_growth","Productionindex","Unemployment")

stargazer(datecb,out = "Summaryecb.html",title = "Summary Statistics European Central Bank 1999-2019",digits = 2)


#Instruments:

plot4=ggplot(datge,aes(x = datge$Datum)) +
  geom_line(aes(y = as.numeric( datge$spread), colour = "black"), size = 0.8) +
  geom_vline(aes(xintercept=datge$Datum[349],color="blue"),show.legend =F)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Spread",blue="ECB replaces DBB: 1999-01"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Spread Germany", subtitle="",
       y     = "Spread",x     = "Date")

plot5=ggplot(datge, aes(x = datge$Datum)) +
  geom_line(aes(y = as.numeric(datge$M2), colour = "black"), size = 0.8) +
  geom_vline(aes(xintercept=datge$Datum[349],color="blue"),show.legend =F)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "M2 Growth", blue="ECB replaces DBB: 1999-01"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "M2 Growth", subtitle="1970-1998: DBB; 1999-2019: ECB",
       y     = "M2 Growth in %",
       x     = "Date")

plot6=ggplot(datge, aes(x = datge$Datum)) +
  geom_line(aes(y = as.numeric(datge$prod), col = "black"), size = 0.8) +
  geom_vline(aes(xintercept=Datum[349],color="blue"),show.legend =F )+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Productionindex",blue="ECB replaces DBB: 1999-01"),guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Productionindex",
       y     = "Index",
       x     = "Date")

plot7=ggplot(datge, aes(x = datge$Datum)) +
  geom_line(aes(y = as.numeric(datge$unem), colour = "black"), size = 0.8) +
  geom_vline(aes(xintercept=datge$Datum[349],color="blue"),show.legend =F)+
  scale_color_identity(name = NULL, 
                       labels = c(black = "Unemployment", blue="ECB replaces DBB: 1999-01"),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Unemployment", subtitle="1970-1998: DBB; 1999-2019: ECB",
       y     = "Unemployment in %",
       x     = "Date")

d <- arrangeGrob(plot4, plot5, ncol=2)
ggsave(file="plot2.png", d,scale = 1,width = 18,height = 5,device='png', dpi=500)

e <- arrangeGrob(plot6,plot7, ncol=2)
ggsave(file="plot3.png", e,scale = 1,width = 18,height = 5,device='png', dpi=500)

###########################################################################################

#######################Relevance of Instruments####################################################

datge1=data.frame(cbind(c(CPIddb$`CPI Germany`,CPIecb$`CPI Germany`),c(ratedbb$`Interest Rate DBB`,rateecb$`Interest Rate ECB`),c(GDP_norm_dbb$GDP_norm_Germany,GDP_norm_ecb$GDP_norm_Germany),c(spreaddbb$Spread,spreadecb$Spread),c(prodindexdbb$Productionindex,prodindexecb$Productionindex),c(unemploymentdbb$`Unemployment Rate`,unemploymentecb$`Unemployment Rate`),c(M2_growth_dbb$M2_growth_dbb,M2_growth_ecb$M2_growth_ecb)))
names(datge1)=c("Inflation","FundsRate","OutputGap","Spread","Productionindex","Unemployment","M2Growth")

datge11=as.data.frame(mutate(datge1,InflationLead=lead(Inflation),OutputGapLead=lead(OutputGap),InflationLag1=lag(Inflation,1),OutputGapLag1=lag(OutputGap,1),M2GrowthLag1=lag(M2Growth,1),SpreadLag1=lag(Spread,1),ProductionindexLag1=lag(Productionindex,1),UnemploymentLag1=lag(Unemployment,1)))
datge11=na.omit(datge11)
datge11=data.frame(datge11)

for (i in 1:length(colnames(datge11))) {
  datge11[,i]=as.numeric(datge11[,i])
}
datge11=data.matrix(datge11)
cormat1 = cov(datge11)

ggsave("Anhang1.png",plot = ggcorrplot1,scale = 1,width = 12,height = 5,device='png', dpi=500)

stargazer(cormat1[10:15,c(8,9)],type="text",out="corr.html",title = "Covariance of Independent Variables and First Lag of Instruments",style = "aer") #Abbildung1

###########################################################################################
#######################ESTIMATION####################################################

names(datdbb)=c("INFL","GAP","SPREAD","FFR","RM2","COM_P","UNEM")

dat1=mutate(datdbb,FFR=lag(FFR,0),INFLF=lead(INFL),GAPF=lead(GAP), 
          FFRL1=lag(FFR,1),FFRL2=lag(FFR,2), FFRL3=lag(FFR,3), FFRL4=lag(FFR,4),FFRL5 = lag(FFR,5), FFR6=lag(FFR,6), FFR7=lag(FFR,7), FFR8=lag(FFR,8),FFR9=lag(FFR,9),FFR10=lag(FFR,10),FFR11=lag(FFR,11), FFR12=lag(FFR,12),
           INFLL1=lag(INFL,1), INFLL2=lag(INFL,2), INFLL3=lag(INFL,3), INFLL4=lag(INFL,4), INFLL5=lag(INFL,5),INFLL6=lag(INFL,6),INFLL7=lag(INFL,7),INFLL8=lag(INFL,8),INFLL9=lag(INFL,9),INFLL10=lag(INFL,10),INFLL11=lag(INFL,11),INFLL12=lag(INFL,12),
           SPREADL1=lag(SPREAD,1),SPREADL2=lag(SPREAD,2),SPREADL3=lag(SPREAD,3),SPREADL4=lag(SPREAD,4),SPREADL5=lag(SPREAD,5),SPREADL6=lag(SPREAD,6),SPREADL7=lag(SPREAD,7),SPREADL8=lag(SPREAD,8),SPREADL9=lag(SPREAD,9),SPREADL10=lag(SPREAD,10),SPREADL11=lag(SPREAD,11),SPREADL12=lag(SPREAD,12),
          GAPL1=lag(GAP,1), GAPL2=lag(GAP,2),GAPL3=lag(GAP,3),GAPL4=lag(GAP,4),GAPL5=lag(GAP,5),GAPL6=lag(GAP,6),GAPL7=lag(GAP,7),GAPL8=lag(GAP,8),GAPL9=lag(GAP,9),GAPL9=lag(GAP,9),GAPL10=lag(GAP,10),GAPL11=lag(GAP,11),GAPL12=lag(GAP,12),
          RML1=lag(RM2,1),RML2=lag(RM2,2),RML3=lag(RM2,3),RML4=lag(RM2,4),RML5=lag(RM2,5),RML6=lag(RM2,6),RML7=lag(RM2,7),RML8=lag(RM2,8),RML9=lag(RM2,9),RML10=lag(RM2,10),RML11=lag(RM2,11),RML12=lag(RM2,12),
           COMPL1=lag(COM_P,1),COMPL2=lag(COM_P,2),COMPL3=lag(COM_P,3),COMPL4=lag(COM_P,4),COMPL5=lag(COM_P,5),COMPL6=lag(COM_P,6),COMPL7=lag(COM_P,7),COMPL8=lag(COM_P,8),COMPL9=lag(COM_P,9),COMPL10=lag(COM_P,10),COMPL11=lag(COM_P,11),COMPL12=lag(COM_P,12),
          UNEML1=lag(UNEM,1),UNEML2=lag(UNEM,2),UNEML3=lag(UNEM,3),UNEML4=lag(UNEM,4), UNEML5=lag(UNEM,5),UNEML6=lag(UNEM,6),UNEML7=lag(UNEM,7),UNEML8=lag(UNEM,8), UNEML9=lag(UNEM,9),UNEML10=lag(UNEM,10),UNEML11=lag(UNEM,11),UNEML12=lag(UNEM,12))

dat1=na.omit(dat1)
dat1=as.matrix(dat1)

gmmestimation <- function(omega,x=NULL) {
  
  
  ## reaction function
  reac_f=omega[1]+omega[2]*x[,8]+omega[3]*x[,9]
  m1=x[,4]-((1-omega[4]-omega[5])*reac_f)-omega[4]*x[,10]-omega[5]*x[,11]
  m2=m1*x[,22]
  m3=m1*x[,23]
  m4=m1*x[,34]
  m5=m1*x[,35]
  m6=m1*x[,46]
  m7=m1*x[,47]
  m8=m1*x[,58]
  m9=m1*x[,59]
  m10=m1*x[,92]
  m11=m1*x[,93]
  m12=m1*x[,80]
  m13=m1*x[,81]
  
  return(cbind(m2,m8,m5,m13,m9,m6))
  
}


start.vals = c(1.5,1.5,0.5,0.4,0.4)
names(start.vals) = c("alpha", "beta","gamma","rho1","rho2")
CGG.mom = gmmestimation(start.vals,dat1)
head(CGG.mom)
colMeans(CGG.mom)


CGG.gmm.fit1 = gmm(gmmestimation, x=dat1, 
                   t0=start.vals, type="iterative", prewhite = 1,
                   wmatrix="optimal", vcov="HAC",optfct="nlminb")
summary(CGG.gmm.fit1)


#####################################################################################
####################################################################################

names(datecb)=c("INFL","GAP","SPREAD","FFR","RM2","COM_P","UNEM")

dat1=mutate(datecb,FFR=lag(FFR,0),INFLF=lead(INFL,3),GAPF=lead(GAP,3), 
            FFRL1=lag(FFR,1),FFRL2=lag(FFR,2), FFRL3=lag(FFR,3), FFRL4=lag(FFR,4),FFRL5 = lag(FFR,5), FFR6=lag(FFR,6), FFR7=lag(FFR,7), FFR8=lag(FFR,8),FFR9=lag(FFR,9),FFR10=lag(FFR,10),FFR11=lag(FFR,11), FFR12=lag(FFR,12),
            INFLL1=lag(INFL,1), INFLL2=lag(INFL,2), INFLL3=lag(INFL,3), INFLL4=lag(INFL,4), INFLL5=lag(INFL,5),INFLL6=lag(INFL,6),INFLL7=lag(INFL,7),INFLL8=lag(INFL,8),INFLL9=lag(INFL,9),INFLL10=lag(INFL,10),INFLL11=lag(INFL,11),INFLL12=lag(INFL,12),
            SPREADL1=lag(SPREAD,1),SPREADL2=lag(SPREAD,2),SPREADL3=lag(SPREAD,3),SPREADL4=lag(SPREAD,4),SPREADL5=lag(SPREAD,5),SPREADL6=lag(SPREAD,6),SPREADL7=lag(SPREAD,7),SPREADL8=lag(SPREAD,8),SPREADL9=lag(SPREAD,9),SPREADL10=lag(SPREAD,10),SPREADL11=lag(SPREAD,11),SPREADL12=lag(SPREAD,12),
            GAPL1=lag(GAP,1), GAPL2=lag(GAP,2),GAPL3=lag(GAP,3),GAPL4=lag(GAP,4),GAPL5=lag(GAP,5),GAPL6=lag(GAP,6),GAPL7=lag(GAP,7),GAPL8=lag(GAP,8),GAPL9=lag(GAP,9),GAPL9=lag(GAP,9),GAPL10=lag(GAP,10),GAPL11=lag(GAP,11),GAPL12=lag(GAP,12),
            RML1=lag(RM2,1),RML2=lag(RM2,2),RML3=lag(RM2,3),RML4=lag(RM2,4),RML5=lag(RM2,5),RML6=lag(RM2,6),RML7=lag(RM2,7),RML8=lag(RM2,8),RML9=lag(RM2,9),RML10=lag(RM2,10),RML11=lag(RM2,11),RML12=lag(RM2,12),
            COMPL1=lag(COM_P,1),COMPL2=lag(COM_P,2),COMPL3=lag(COM_P,3),COMPL4=lag(COM_P,4),COMPL5=lag(COM_P,5),COMPL6=lag(COM_P,6),COMPL7=lag(COM_P,7),COMPL8=lag(COM_P,8),COMPL9=lag(COM_P,9),COMPL10=lag(COM_P,10),COMPL11=lag(COM_P,11),COMPL12=lag(COM_P,12),
            UNEML1=lag(UNEM,1),UNEML2=lag(UNEM,2),UNEML3=lag(UNEM,3),UNEML4=lag(UNEM,4), UNEML5=lag(UNEM,5),UNEML6=lag(UNEM,6),UNEML7=lag(UNEM,7),UNEML8=lag(UNEM,8), UNEML9=lag(UNEM,9),UNEML10=lag(UNEM,10),UNEML11=lag(UNEM,11),UNEML12=lag(UNEM,12))

dat1=na.omit(dat1)
dat1=as.matrix(dat1)

gmmestimation <- function(omega,x=NULL) {
  
  
  ## reaction function
  reac_f=omega[1]+omega[2]*x[,8]+omega[3]*x[,9]
  m1=x[,4]-((1-omega[4]-omega[5])*reac_f)-omega[4]*x[,10]-omega[5]*x[,11]
  m2=m1*x[,22]
  m3=m1*x[,23]
  m4=m1*x[,34]
  m5=m1*x[,35]
  m6=m1*x[,46]
  m7=m1*x[,47]
  m8=m1*x[,58]
  m9=m1*x[,59]
  m10=m1*x[,92]
  m11=m1*x[,93]
  m12=m1*x[,80]
  m13=m1*x[,81]
  
  return(cbind(m2,m5,m8,m9,m10,m13))
  
}



start.vals = c(1.5,1.5,0.5,0.4,0.4)
names(start.vals) = c("alpha", "beta","gamma","rho1","rho2")
CGG.mom = gmmestimation(start.vals,dat1)
head(CGG.mom)
colMeans(CGG.mom)


CGG.gmm.fit2 = gmm(gmmestimation, x=dat1, 
                   t0=start.vals, type="twoStep", prewhite = 1,
                   wmatrix="optimal", vcov="HAC",optfct="nlminb")
summary(CGG.gmm.fit2)


stargazer(CGG.gmm.fit1,CGG.gmm.fit2,out = "estimation.html",title = "Regression Estimates",dep.var.labels = "Policy Rate",flip = T,type = "text",align=T,column.labels = c("Deutsche Bundesbank","European Central Bank"))




