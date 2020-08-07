###Outcome variable
CIN_N$outcome_var=NULL
CIN_N$outcome_var[CIN_N$outcome=="Absconded"|CIN_N$outcome=="Referred"]=0
CIN_N$outcome_var[CIN_N$outcome=="Alive"]=1
CIN_N$outcome_var[CIN_N$outcome=="Dead"]=2
#CIN_N$outcome_var=factor(CIN_N$outcome_var,levels = 1:3, labels =c("Absconded/Referred","Alive","Died"))
#CIN_N$outcome_var=factor(CIN_N$outcome_var)
###
CIN_N$los=NULL
CIN_N$los=as.Date(CIN_N$date_discharge)-as.Date(CIN_N$date_adm)
CIN_N$los=as.numeric(as.character(CIN_N$los))
CIN_N$los[CIN_N$los<0|CIN_N$los>14]=NA
CIN_N$los[CIN_N$los==0 & !is.na(CIN_N$los)]=(CIN_N$los+0.0001)[CIN_N$los==0& !is.na(CIN_N$los)]
###Competing risk analysis
library(timereg)
model_data=na.omit(as.data.frame(with(CIN_N, cbind(los,outcome_var,bwt_cat))))

model_data$bwt_cat=factor(model_data$bwt_cat)
model <- comp.risk(Event(los,outcome_var) ~ const(bwt_cat), data=model_data,n.sim = 500,cause=2, model = 'prop',Nit=50)

#library(survival)
#model2 <- finegray(Surv(outcome_var,los) ~ bwt_cat, data=model_data)
