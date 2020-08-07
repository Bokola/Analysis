rm(list=ls())
setwd('F:\\CDY possible work\\Descriptive neonatal paper\\New Neonatal Analysis')
###Reading datasets:
kilifi_data=read.csv('Kilifi_data.csv')
CIN_P_data=read.csv('CIN_P_data.csv')
CIN_N_data=read.csv('CIN_N_data.csv')

###Limiting all the data to discharges after April 2018:
kilifi=subset(kilifi_data, as.Date(as.character(date_disch), "%d%B%Y") >= as.Date(as.character('2018-04-01')))
CIN_P1=subset(CIN_P_data, as.Date(as.character(date_discharge)) >= as.Date(as.character('2018-04-01')))
CIN_N1=subset(CIN_N_data, as.Date(as.character(date_discharge)) >= as.Date(as.character('2018-04-01')))

CIN_P=subset(CIN_P1, hosp_id!="World Friends/Ruaraka Neema Hospital"&hosp_id!="Karatina District Hospital"&hosp_id!="Homabay County Referral Hospital")
CIN_N=subset(CIN_N1, hosp_id!="World Friends/Ruaraka Neema Hospital"&hosp_id!="Karatina District Hospital"&hosp_id!="Homabay County Referral Hospital"&hosp_id!="Nakuru Level 5 Hospital")



####Generating age for those for CIN P data:
##Note : -1 also denotes missing and needs to be recoded to NA
CIN_P$age_mths<-as.numeric(as.character(CIN_P$age_mths))
CIN_P$age_years[CIN_P$age_years==-1|CIN_P$age_years>15]<- NA 
CIN_P$age_years<-as.numeric(as.character(CIN_P$age_years))
CIN_P$age_mths[CIN_P$age_mths==-1|CIN_P$age_mths>12]<- NA

CIN_P$age_days<-as.numeric(as.character(CIN_P$age_days))
CIN_P$age_days[CIN_P$age_days==-1|CIN_P$age_days>30]<- NA
#data$age_mths_rcd<-NULL
newage<-with(CIN_P,data.frame(cbind(age_days,age_mths,age_years)))
newage$age_days[is.na(newage$age_days)]<-0
newage$age_mths[is.na(newage$age_mths)]<-0
newage$age_years[is.na(newage$age_years)]<-0

#create/derive a new age variable in terms of months from the three age variables expressed in days,months and years:
CIN_P$age_mths_rcd<-with(newage,(age_days/30)+age_mths+(age_years*12))
##Recode anything less than 0 to NA:
CIN_P$age_mths_rcd[CIN_P$age_mths_rcd<=0]<-NA

####Exclusions in CIN Paediatrics:
table(CIN_P$age_mths_rcd>1)

##################################################ANALYSIS OF NBU DATA#############################################################################
####Characteristics of NBU admissions 
####---setting Empty, -1 and '' to NA
CIN_N[CIN_N=='Empty'|CIN_N==-1|CIN_N=='']=NA
CIN_N$hosp_id=as.factor(as.character(CIN_N$hosp_id))

###########################################################Variable  recoding:
CIN_N$birth_wt[CIN_N$birth_wt>400 &!is.na(CIN_N$birth_wt)]=(CIN_N$birth_wt/1000)[CIN_N$birth_wt>400 &!is.na(CIN_N$birth_wt)]
CIN_N$birth_wt[CIN_N$birth_wt>7 &!is.na(CIN_N$birth_wt)]=NA

######bwt categories:
CIN_N$bwt_cat=NULL
CIN_N$bwt_cat[CIN_N$birth_wt<1]=1
CIN_N$bwt_cat[CIN_N$birth_wt>=1 & CIN_N$birth_wt<1.5]=2
CIN_N$bwt_cat[CIN_N$birth_wt>=1.5 & CIN_N$birth_wt<2]=3
CIN_N$bwt_cat[CIN_N$birth_wt>=2 & CIN_N$birth_wt<2.5]=4
CIN_N$bwt_cat[CIN_N$birth_wt>=2.5 & CIN_N$birth_wt<=4]=5
CIN_N$bwt_cat[CIN_N$birth_wt>4]=6

CIN_N$bwt_cat=factor(CIN_N$bwt_cat, levels = 1:6, labels = c('< 1 Kg','1 - < 1.5 Kg','1.5 - < 2 Kg','2 - < 2.5 Kg','2.5 - 4 Kg','> 4 Kg'))

###Age at admission:

CIN_N$age_cat=NULL
CIN_N$age_cat[CIN_N$age_less_than_24hrs=='Yes'|CIN_N$age_days==0]=1
CIN_N$age_cat[CIN_N$age_days==1]=2
CIN_N$age_cat[CIN_N$age_days>1 & CIN_N$age_days<=6]=3
CIN_N$age_cat[CIN_N$age_days>6]=4
CIN_N$age_cat=factor(CIN_N$age_cat, levels = 1:4, labels = c('Admitted on day of birth','1 day','<= 6 days','> 6 days'))

###Gestation at birth:
CIN_N$gest[CIN_N$gest<0]=NA
CIN_N$gest[CIN_N$gest>46]=NA

CIN_N$gest_cat=NULL
CIN_N$gest_cat[CIN_N$gest<28]=1
CIN_N$gest_cat[CIN_N$gest>=28 & CIN_N$gest< 34]=2
CIN_N$gest_cat[CIN_N$gest>=34 & CIN_N$gest< 37]=3
CIN_N$gest_cat[CIN_N$gest>=37 & CIN_N$gest<= 42]=4
CIN_N$gest_cat[CIN_N$gest>42]=5

CIN_N$gest_cat=factor(CIN_N$gest_cat, levels = 1:5, labels = c('< 28 weeks','28 - < 34 weeks','34 - < 37 weeks','37 - 42 weeks','> 42 weeks'))

###Childsex
CIN_N$child_sex_cat=NULL
CIN_N$child_sex_cat[CIN_N$child_sex=='Female']=1
CIN_N$child_sex_cat[CIN_N$child_sex=='Male']=2
CIN_N$child_sex_cat=factor(CIN_N$child_sex_cat, levels = 1:2, labels = c('Female','Male'))

###Maternal HIV status:
CIN_N$maternal_hiv_cat=NULL
CIN_N$maternal_hiv_cat[CIN_N$maternal_hiv=='Positive']=1
CIN_N$maternal_hiv_cat[CIN_N$maternal_hiv=='Negative']=2
CIN_N$maternal_hiv_cat=factor(CIN_N$maternal_hiv_cat, levels = 1:2, labels = c('Positive','Negative'))

###Mode of delivery:
CIN_N$delivery_cat=NULL
CIN_N$delivery_cat[CIN_N$delivery=="Assisted vaginal (Includes Forceps,Vacuum)"]=1
CIN_N$delivery_cat[CIN_N$delivery=="Breech"]=2
CIN_N$delivery_cat[CIN_N$delivery=="Caesarean section (C/S)"]=3
CIN_N$delivery_cat[CIN_N$delivery=="Spontaneous vaginal (SVD)"]=4
CIN_N$delivery_cat=factor(CIN_N$delivery_cat, levels = 1:4, labels = c("Assisted vaginal (Includes Forceps,Vacuum)","Breech","Caesarean section (C/S)", "Spontaneous vaginal (SVD)"))

##Born before arriving at the hospital
CIN_N$bba_cat=NULL
CIN_N$bba_cat[CIN_N$bba=='No']=0
CIN_N$bba_cat[CIN_N$bba=='Yes']=1
CIN_N$bba_cat=factor(CIN_N$bba_cat, levels = 0:1, labels = c('No','Yes'))

#Outborn
CIN_N$outborn=NULL
CIN_N$outborn[CIN_N$bba_cat=='Yes' | CIN_N$referred_to_hospital=='Yes']=1
CIN_N$outborn[is.na(CIN_N$outborn)]=0
CIN_N$outborn=factor(CIN_N$outborn, levels = 0:1, labels = c('No','Yes'))
#Hypothermia:36.5
CIN_N$hypothermia=NULL
CIN_N$hypothermia[CIN_N$temperature_degrees_celciu<36.5]=1
CIN_N$hypothermia[CIN_N$temperature_degrees_celciu>=36.5]=0
CIN_N$hypothermia=factor(CIN_N$hypothermia,levels = 0:1, labels = c('No','Yes'))

###Age:
CIN_N$age_at_adm=NULL
CIN_N$age_at_adm= as.Date(CIN_N$date_adm) - as.Date(CIN_N$date_of_birth)
CIN_N$age_at_adm[CIN_N$age_at_adm<0|CIN_N$age_at_adm>30]=NA
CIN_N$age_at_adm=as.numeric(as.character(CIN_N$age_at_adm))
CIN_N$revised_age=NULL
CIN_N$revised_age[CIN_N$age_at_adm==0]=0
CIN_N$revised_age[CIN_N$age_at_adm==1]=1
CIN_N$revised_age[CIN_N$age_at_adm>1 & CIN_N$age_at_adm<=6]=2
CIN_N$revised_age[CIN_N$age_at_adm>6]=3
CIN_N$revised_age=factor(CIN_N$revised_age, levels = 0:3, labels = c('Less than 24 hours','1 day','1 - 6 days','> 6 days'))


CIN_N$apgar_5min=factor(CIN_N$apgar_5min)
######
descriptive_function = function (datum=CIN_N)
{
  total_n=cbind("Total admissions",nrow(datum))
  childsex_n=table(datum$child_sex_cat)['Female']
  childsex_p=round((prop.table(table(datum$child_sex_cat))['Female'])*100)
  childsex_p[is.na(childsex_p)]=0
  childsex_res=rbind(cbind('',paste0('n = ',sum(table(datum$child_sex_cat)))),cbind('Female',paste0(childsex_n,' (',childsex_p,")")))
  
  age_n=table(datum$revised_age)
  age_p=round((prop.table(table(datum$revised_age)))*100)
  age_p[is.na(age_p)]=0
  age_res=rbind(cbind('',paste0('n = ',sum(table(datum$revised_age)))),cbind(names(age_n),paste0(age_n,' (',age_p,")")))
  
  gest_n=table(datum$gest_cat)
  gest_p=round((prop.table(table(datum$gest_cat)))*100)
  gest_p[is.na(gest_p)]=0
  gest_p[is.na(gest_p)]=0
  gest_res=rbind(cbind('',paste0('n = ',sum(table(datum$gest_cat)))),cbind(names(gest_n),paste0(gest_n,' (',gest_p,")")))
  
  delivery_n=table(datum$delivery_cat)
  delivery_p=round((prop.table(table(datum$delivery_cat)))*100)
  delivery_p[is.na(delivery_p)]=0
  delivery_res=rbind(cbind('',paste0('n = ',sum(table(datum$delivery_cat)))),cbind(names(delivery_n),paste0(delivery_n,' (',delivery_p,")")))
  
  bwt_n=table(datum$bwt_cat)
  bwt_p=round((prop.table(table(datum$bwt_cat)))*100)
  bwt_p[is.na(bwt_p)]=0
  bwt_res=rbind(cbind('',paste0('n = ',sum(table(datum$bwt_cat)))),cbind(names(bwt_n),paste0(bwt_n,' (',bwt_p,")")))
  
  bba_n=table(datum$outborn)['Yes']
  bba_p=round((prop.table(table(datum$outborn))['Yes'])*100)
  bba_p[is.na(bba_p)]=0
  bba_res=rbind(cbind('',paste0('n = ',sum(table(datum$outborn)))),cbind('Outborn',paste0(bba_n,' (',bba_p,")")))

  ####Outborn:
  hypo_n=table(datum$hypothermia)['Yes']
  hypo_p=round((prop.table(table(datum$hypothermia))['Yes'])*100)
  hypo_p[is.na(hypo_p)]=0
  hypo_res=rbind(cbind('',paste0('n = ',sum(table(datum$hypothermia)))),cbind('Neonates with hypothermia at admission',paste0(hypo_n,' (',hypo_p,")")))
  
  hiv_n=table(datum$maternal_hiv_cat)['Positive']
  hiv_p=round((prop.table(table(datum$maternal_hiv_cat))['Positive'])*100)
  hiv_p[is.na(hiv_p)]=0
  hiv_res=rbind(cbind('',paste0('n = ',sum(table(datum$maternal_hiv_cat)))),cbind('HIV positive',paste0(hiv_n,' (',hiv_p,")")))
  
  
  apgar_n=table(datum$apgar_5min)
  apgar_p=round((prop.table(table(datum$apgar_5min)))*100)
  apgar_p[is.na(apgar_p)]=0
  apgar_res=rbind(cbind('',paste0('n = ',sum(table(datum$apgar_5min)))),cbind(names(apgar_n),paste0(apgar_n,' (',apgar_p,")")))
  results=rbind(total_n,childsex_res,age_res,gest_res,delivery_res,bwt_res,bba_res,hypo_res,apgar_res,hiv_res)
  return (results)
}

###Hospital specific datasets:
CIN_N$hosp_id=factor(CIN_N$hosp_id)

eval(parse(text=paste0('H',1:length(names(table(CIN_N$hosp_id))),"= subset(CIN_N, hosp_id=='",names(table(CIN_N$hosp_id)),"')", sep = "\n")))

overall_desc=descriptive_function()
eval(parse(text=paste0('Hosp_desc', 1:length(names(table(CIN_N$hosp_id))), "= descriptive_function(H",1:length(names(table(CIN_N$hosp_id))),")", sep ='\n')))

all_descriptive_res=cbind(overall_desc,Hosp_desc1[,-1],Hosp_desc2[,-1],Hosp_desc3[,-1],Hosp_desc4[,-1],Hosp_desc5[,-1],Hosp_desc6[,-1],Hosp_desc7[,-1],Hosp_desc8[,-1],Hosp_desc9[,-1],
                          Hosp_desc10[,-1],Hosp_desc11[,-1],Hosp_desc12[,-1],Hosp_desc13[,-1],Hosp_desc14[,-1],Hosp_desc15[,-1],Hosp_desc16[,-1])



#write.csv(all_descriptive_res, file = 'all_descriptive_res.csv')

####Missing data analysis:
variables=c('child_sex_cat','age_cat','gest_cat','delivery_cat','bwt_cat','bba','referred_to_hospital','temperature_degrees_celciu',
            'maternal_hiv_cat','apgar_5min')
missing_data_function=function (var = 'child_sex_cat')
{
  tapply(CIN_N[,var],CIN_N$hosp_id, function(x)
  {
    paste0(round((prop.table(table(is.na(x)))['TRUE'])*100),'%')
  })}

missing_data_output=t(sapply(variables,missing_data_function))

###Overall missingness:
prop.table(table(is.na(CIN_N$child_sex_cat)))['TRUE']
prop.table(table(is.na(CIN_N$age_cat)))['TRUE']
prop.table(table(is.na(CIN_N$gest_cat)))['TRUE']
prop.table(table(is.na(CIN_N$delivery_cat)))['TRUE']
prop.table(table(is.na(CIN_N$bwt_cat)))['TRUE']
prop.table(table(is.na(CIN_N$bba)))['TRUE']
prop.table(table(is.na(CIN_N$referred_to_hospital)))['TRUE']
prop.table(table(is.na(CIN_N$temperature_degrees_celciu)))['TRUE']
prop.table(table(is.na(CIN_N$maternal_hiv_cat)))['TRUE']
prop.table(table(is.na(CIN_N$apgar_5min)))['TRUE']

###--------Analysis od mortality
###New birthweight Weight categories: <1500gm,  1500-<2000kg and  over 2000gramsWeight categories: <1500gm,  1500-<2000kg and  over 2000grams
# CIN_N$new_bwt_cat=NULL
# CIN_N$new_bwt_cat[CIN_N$birth_wt<1.5]=1
# CIN_N$new_bwt_cat[CIN_N$birth_wt>=1.5 & CIN_N$birth_wt<2]=2
# CIN_N$new_bwt_cat[CIN_N$birth_wt>=2]=3
# CIN_N$new_bwt_cat=factor(CIN_N$new_bwt_cat,levels = 1:3, labels = c('<1.5 Kg','1.5 - <2 Kg','>=2 Kg'))
# ##mortality
# under_1point5_mortality=table(CIN_N$outcome[CIN_N$new_bwt_cat=='<1.5 Kg'], CIN_N$hosp_id[CIN_N$new_bwt_cat=='<1.5 Kg'])['Dead',]
# onepoint5to2_mortality=table(CIN_N$outcome[CIN_N$new_bwt_cat=='1.5 - <2 Kg'], CIN_N$hosp_id[CIN_N$new_bwt_cat=='1.5 - <2 Kg'])['Dead',]
# morethan2_mortality=table(CIN_N$outcome[CIN_N$new_bwt_cat=='>=2 Kg'], CIN_N$hosp_id[CIN_N$new_bwt_cat=='>=2 Kg'])['Dead',]
# ###Totals
# under_1point5_total=table(CIN_N$new_bwt_cat, CIN_N$hosp_id)['<1.5 Kg',]
# onepoint5to2_total=table(CIN_N$new_bwt_cat, CIN_N$hosp_id)['1.5 - <2 Kg',]
# morethan2_total=table(CIN_N$new_bwt_cat, CIN_N$hosp_id)['>=2 Kg',]
# 
# under_1point5_data=data.frame(hosp=paste0('H',1:16),d=under_1point5_total,n=under_1point5_mortality)
# onepoint5to2_data=data.frame(hosp=paste0('H',1:16),d=onepoint5to2_total,n=onepoint5to2_mortality)
# morethan2_data=data.frame(hosp=paste0('H',1:16),d=morethan2_total,n=morethan2_mortality)
# 
# ###overall mortality per hospital
# overall_mortality=table(CIN_N$outcome, CIN_N$hosp_id)['Dead',]
# total_data=c(nrow(H1),nrow(H2),nrow(H3),nrow(H4),nrow(H5),nrow(H6),nrow(H7),nrow(H8),
#              nrow(H9),nrow(H10),nrow(H11),nrow(H12),nrow(H13),nrow(H14),nrow(H15),nrow(H16))
# all_data=data.frame(hosp=paste0('H',1:16),d=total_data,n=overall_mortality)
# 
# ###Data to export:
# all_mortality_data=cbind(Hospital=paste0('H',1:16),with(all_data,n/d),with(under_1point5_data,n/d),with(onepoint5to2_data,n/d),with(morethan2_data,n/d))
# 
# 
# ##funnel plots:
# library(funnelR)
# library(ggplot2)
# library(gridExtra)
# library(cowplot)
# funnel_plot_function=function(datum=all_data, foot='Total admissions for each hospital')
# {
# my_limits=fundata(input=datum, 
#                        benchmark=with(datum,median(c(n/d))), 
#                        alpha=0.95, 
#                        alpha2=0.95, 
#                        method='approximate', 
#                        step=1)
# 
# my_plot=funplot(input=datum, 
#                        fundata=my_limits)
# my_plot_mod <- my_plot + labs(x=foot, y="Proportion (%) of mortality") #+
#   #geom_text(aes(label=hosp), colour="black", size=4, nudge_x=15) 
# my_plot_mod
# }
# 
# ###Calling the funnel function:
# plot_grid(
# funnel_plot_function(),
# funnel_plot_function(datum=under_1point5_data, foot='Under 1500g birthweight admissions for each hospital'),
# funnel_plot_function(datum=onepoint5to2_data, foot='1500 to <2000g birthweight admissions for each hospital'),
# funnel_plot_function(datum=morethan2_data, foot='>= 2000g birthweight admissions for each hospital')
# ,labels = "AUTO")

##mortality
# < 1 Kg
# 1 - < 1.5 Kg
# 1.5 - < 2 Kg
# 2 - < 2.5 Kg
# 2.5 - 4 Kg
# > 4 Kg

under1_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='< 1 Kg'], CIN_N$hosp_id[CIN_N$bwt_cat=='< 1 Kg'])['Dead',]
onetoless1.5_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='1 - < 1.5 Kg'], CIN_N$hosp_id[CIN_N$bwt_cat=='1 - < 1.5 Kg'])['Dead',]
onepoint5toless2_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='1.5 - < 2 Kg'], CIN_N$hosp_id[CIN_N$bwt_cat=='1.5 - < 2 Kg'])['Dead',]
twotoless2.5_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='2 - < 2.5 Kg'], CIN_N$hosp_id[CIN_N$bwt_cat=='2 - < 2.5 Kg'])['Dead',]
twopoint5andabove_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=="> 4 Kg"], CIN_N$hosp_id[CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=="> 4 Kg"])['Dead',]
###Totals
under1_total=table(CIN_N$bwt_cat, CIN_N$hosp_id)['< 1 Kg',]
onetoless1.5_total=table(CIN_N$bwt_cat, CIN_N$hosp_id)['1 - < 1.5 Kg',]
onepoint5toless2_total=table(CIN_N$bwt_cat, CIN_N$hosp_id)['1.5 - < 2 Kg',]
twotoless2.5_total=table(CIN_N$bwt_cat, CIN_N$hosp_id)['2 - < 2.5 Kg',]
twopoint5andabove_total=colSums(table(CIN_N$bwt_cat, CIN_N$hosp_id)[c('2.5 - 4 Kg','> 4 Kg'),])

under1_data=data.frame(hosp=paste0('H',1:16),d=c(under1_total),n=c(under1_mortality))
onetoless1.5_data=data.frame(hosp=paste0('H',1:16),d=c(onetoless1.5_total),n=c(onetoless1.5_mortality))
onepoint5toless2_data=data.frame(hosp=paste0('H',1:16),d=c(onepoint5toless2_total),n=c(onepoint5toless2_mortality))
twotoless2.5_data=data.frame(hosp=paste0('H',1:16),d=c(twotoless2.5_total),n=c(twotoless2.5_mortality))
twopoint5andabove_data=data.frame(hosp=paste0('H',1:16),d=c(twopoint5andabove_total),n=c(twopoint5andabove_mortality))
###overall mortality per hospital
overall_mortality=table(CIN_N$outcome, CIN_N$hosp_id)['Dead',]
total_data=c(nrow(H1),nrow(H2),nrow(H3),nrow(H4),nrow(H5),nrow(H6),nrow(H7),nrow(H8),
             nrow(H9),nrow(H10),nrow(H11),nrow(H12),nrow(H13),nrow(H14),nrow(H15),nrow(H16))
all_data=data.frame(hosp=paste0('H',1:16),d=c(total_data),n=c(overall_mortality))

###Data to export:
all_mortality_data=cbind(Hospital=paste0('H',1:16),with(all_data,n/d),with(under1_data,n/d),with(onetoless1.5_data,n/d),with(onepoint5toless2_data,n/d),
                         with(twotoless2.5_data,n/d),with(twopoint5andabove_data,n/d))
##funnel plots:
library(funnelR)
library(ggplot2)
library(gridExtra)
library(cowplot)
funnel_plot_function=function(datum=all_data, foot='Total admissions for each hospital')
{
  my_limits=fundata(input=datum, 
                    benchmark=with(datum,median(c(n/d))), 
                    alpha=0.95, 
                    alpha2=0.95, 
                    method='approximate', 
                    step=1)
  
  my_plot=funplot(input=datum, 
                  fundata=my_limits)
  my_plot_mod <- my_plot + labs(x=foot, y="Proportion (%) of mortality") #+
  #geom_text(aes(label=hosp), colour="black", size=4, nudge_x=15) 
  my_plot_mod
}

###Calling the funnel function:
plot_grid(
  funnel_plot_function(),
  funnel_plot_function(datum=under1_data, foot='< 1000g birthweight admissions for each hospital'),
  funnel_plot_function(datum=onetoless1.5_data, foot='1000 - < 1500g birthweight admissions for each hospital'),
  funnel_plot_function(datum=onepoint5toless2_data, foot='1500 - < 2000g birthweight admissions for each hospital'),
  funnel_plot_function(datum=twotoless2.5_data, foot='2000 - < 2500g birthweight admissions for each hospital'),
  funnel_plot_function(datum=twopoint5andabove_data, foot='>=2500g birthweight admissions for each hospital')
  ,labels = "AUTO")






#####Cumulative mortality by length of stay:
library(cmprsk)
###Outcome variable
CIN_N$outcome_var=NULL
CIN_N$outcome_var[CIN_N$outcome=="Absconded"|CIN_N$outcome=="Referred"]=1
CIN_N$outcome_var[CIN_N$outcome=="Alive"]=2
CIN_N$outcome_var[CIN_N$outcome=="Dead"]=3
CIN_N$outcome_var=factor(CIN_N$outcome_var,levels = 1:3, labels =c("Absconded/Referred","Alive","Died"))
###
CIN_N$los=NULL
CIN_N$los=as.Date(CIN_N$date_discharge)-as.Date(CIN_N$date_adm)
CIN_N$los[CIN_N$los<0|CIN_N$los>14]=NA
CIN_N$los=as.numeric(as.character(CIN_N$los))



###Limited funnel plots to los <= 2 days

#source('limited_funnel.R')



# ####Plotting the cumulative probabilities of mortality:
# CI.overall <- cuminc(ftime=CIN_N$los, fstatus=CIN_N$outcome_var)
# CI.less1.5 <- cuminc(ftime=CIN_N$los[CIN_N$new_bwt_cat=='<1.5 Kg'], fstatus=CIN_N$outcome_var[CIN_N$new_bwt_cat=='<1.5 Kg'])
# CI.less1.5to2 <- cuminc(ftime=CIN_N$los[CIN_N$new_bwt_cat=='1.5 - <2 Kg'], fstatus=CIN_N$outcome_var[CIN_N$new_bwt_cat=='1.5 - <2 Kg'])
# CI.more2 <- cuminc(ftime=CIN_N$los[CIN_N$new_bwt_cat=='>=2 Kg'], fstatus=CIN_N$outcome_var[CIN_N$new_bwt_cat=='>=2 Kg'])
# 
# #par(mfrow=c(2,2))
# plot(CI.overall$`1 Died`$time,CI.overall$`1 Died`$est, type = 'l',lty=1, ylim = c(0,1),lwd=2, ylab='Probability of mortality', col=1, xlab = 'Days after admission (truncated to 2 months)')
# lines(CI.less1.5$`1 Died`$time,CI.less1.5$`1 Died`$est, lty=2, col=2,lwd=2)
# lines(CI.less1.5to2$`1 Died`$time,CI.less1.5to2$`1 Died`$est, lty=3, col=3,lwd=2)
# lines(CI.more2$`1 Died`$time,CI.more2$`1 Died`$est, type = 'l', col=4, lty=4,lwd=2)
# 
# legend(40,.9, legend=c("Total", "Birthweight: < 1500g",'Birthweight:1500 - 2000g','Birthweight:>2000g'),
#        col=1:4, lty=1:4, cex=1,lwd=2)

####Plotting the cumulative probabilities of mortality:
CI.overall <- cuminc(ftime=CIN_N$los, fstatus=CIN_N$outcome_var)
CI.less1 <- cuminc(ftime=CIN_N$los[CIN_N$bwt_cat=='< 1 Kg'], fstatus=CIN_N$outcome_var[CIN_N$bwt_cat=='< 1 Kg'])
CI.1to1.5 <- cuminc(ftime=CIN_N$los[CIN_N$bwt_cat=='1 - < 1.5 Kg'], fstatus=CIN_N$outcome_var[CIN_N$bwt_cat=='1 - < 1.5 Kg'])
CI.1.5to2 <- cuminc(ftime=CIN_N$los[CIN_N$bwt_cat=='1.5 - < 2 Kg'], fstatus=CIN_N$outcome_var[CIN_N$bwt_cat=='1.5 - < 2 Kg'])
CI.2to2.5 <- cuminc(ftime=CIN_N$los[CIN_N$bwt_cat=='2 - < 2.5 Kg'], fstatus=CIN_N$outcome_var[CIN_N$bwt_cat=='2 - < 2.5 Kg'])
CI.more2.5 <- cuminc(ftime=CIN_N$los[CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=='> 4 Kg'], fstatus=CIN_N$outcome_var[CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=='> 4 Kg'])

#plotting:
plot(CI.overall$`1 Died`$time,CI.overall$`1 Died`$est, type = 'l',lty=1, ylim = c(0,1),lwd=2, ylab='Probability of mortality', col=1, xlab = 'Days after admission (truncated to 2 weeks)')
lines(CI.less1$`1 Died`$time,CI.less1$`1 Died`$est, lty=2, col=2,lwd=2)
lines(CI.1to1.5$`1 Died`$time,CI.1to1.5$`1 Died`$est, lty=3, col=3,lwd=2)
lines(CI.1.5to2$`1 Died`$time,CI.1.5to2$`1 Died`$est, lty=4, col=4,lwd=2)
lines(CI.2to2.5$`1 Died`$time,CI.2to2.5$`1 Died`$est, lty=5, col=5,lwd=2)
lines(CI.more2.5$`1 Died`$time,CI.more2.5$`1 Died`$est, lty=6, col=6,lwd=2)

legend(8,0.7, legend=c("Total", "Birthweight: < 1000g","Birthweight: 1000 - < 1500g","Birthweight: <1500 - < 2000g","Birthweight: 2000 - < 2500g","Birthweight: >=2500g"),
       col=1:6, lty=1:6, cex=1,lwd=2, bty ='n')
#########-------------------------------------------------------------------Analysis of NBU morbidities:
###Examining those with discharge information:
# ##########---------Inclusion and exclusion:

adm_dx=c("adm_diag_1", "adm_diag_2","adm_diag_3","other_admission_diag_1","other_admission_diag_2",
         "other_admission_diag_3","other_admission_diag_4","other_admission_diag_5","admisn_diag_not_listed")
disch_dx=c('primary_disch_diagnosis','disch_diag_1','disch_diag_2','disch_diag_3','disch_diag_4','disch_diag_5',
                 'other_discharge_diag_1','other_discharge_diag_2','other_discharge_diag_3','other_discharge_diag_4','other_discharge_diag_5',
                 'other_disch_diag_old','other_discharge_diag_unlisted')
eval(parse(text=paste0('CIN_N$',c(adm_dx,disch_dx),'=factor(as.character(CIN_N$',c(adm_dx,disch_dx),"))", sep  = "\n")))
#eval(parse(text=paste0('CIN_N$',c(adm_dx,disch_dx),'=as.character(CIN_N$',c(adm_dx,disch_dx),")", sep  = "\n")))

#Those with missing diagnosis:
eval(parse(text=paste0('table(',paste0('is.na(CIN_N$',c(adm_dx,disch_dx),')', collapse = '&'),')')))

###Concatenating the diagnoses per patient:
CIN_N$all_diagnoses=NULL
CIN_N$all_diagnoses=  paste0('"',as.character(CIN_N$adm_diag_1),'","',as.character(CIN_N$adm_diag_2),'","',as.character(CIN_N$adm_diag_3),'","',as.character(CIN_N$other_admission_diag_1),'","',
                             as.character(CIN_N$other_admission_diag_2),'","',as.character(CIN_N$other_admission_diag_3),'","',as.character(CIN_N$other_admission_diag_4),'","',
                             as.character(CIN_N$other_admission_diag_5),'","',as.character(CIN_N$admisn_diag_not_listed),'","',as.character(CIN_N$primary_disch_diagnosis),'","',
                             as.character(CIN_N$disch_diag_1),'","',as.character(CIN_N$disch_diag_2),'","',as.character(CIN_N$disch_diag_3),'","',(CIN_N$disch_diag_4),'","',as.character(CIN_N$disch_diag_5),'","',
                             as.character(CIN_N$other_discharge_diag_1),'","',as.character(CIN_N$other_discharge_diag_2),'","',as.character(CIN_N$other_discharge_diag_3),'","',
                             as.character(CIN_N$other_discharge_diag_4),'","',as.character(CIN_N$other_discharge_diag_5),'","',as.character(CIN_N$other_disch_diag_old),'","',
                             as.character(CIN_N$other_discharge_diag_unlisted),'"')

##########################dominating diagnoses:
#lbw_premature='lbw'|'weight'|'preterm'|'prem'|'immatur'|'small'
#asphyxia: 'asphyxia' 
#Sepsis: 'sepsis'
#distress: 'distress'
#jaundice: 'jaundice'
#twin delivery: 'twin delivery'
#meconium: 'meconium'
#healthy: accompany, well, stable, accom
#large baby or macrosomia: 'large'|'macrosomia'|'heavy'|'big'
##dehydration: dehyd

##other illnesses:
all_illness=tolower(na.omit(c(levels(CIN_N$adm_diag_1),levels(CIN_N$adm_diag_2),levels(CIN_N$adm_diag_3),levels(CIN_N$other_admission_diag_1),
       levels(CIN_N$other_admission_diag_2),levels(CIN_N$other_admission_diag_3),levels(CIN_N$other_admission_diag_4),
       levels(CIN_N$other_admission_diag_5),levels(CIN_N$admisn_diag_not_listed),levels(CIN_N$primary_disch_diagnosis),
       levels(CIN_N$disch_diag_1),levels(CIN_N$disch_diag_2),levels(CIN_N$disch_diag_3),levels(CIN_N$disch_diag_4),levels(CIN_N$disch_diag_5),
       levels(CIN_N$other_discharge_diag_1),levels(CIN_N$other_discharge_diag_2),levels(CIN_N$other_discharge_diag_3),
       levels(CIN_N$other_discharge_diag_4),levels(CIN_N$other_discharge_diag_5),levels(CIN_N$other_disch_diag_old),
       levels(CIN_N$other_discharge_diag_unlisted))))

dominating_illness=grep('lbw|weight|preterm|prem|immatur|small|asphyxia|sepsis|distress|jaundice|twin delivery|meconium|accompany|well|norm|suck|stable|accom|large|macrosomia|heavy|big|macro|dehyd|volume|shock
|hiv|thermia|poxic|caput|meningitis|observation|convul|isoim|necro|injury|hypog|feed|fever|talipes|down|pneum|cleft|malf|deform|spina|hydroce|anaemia|anem|tach
                        |dhydration|dehyrdation|dehdration|volume|injuries|stable|well|accom|neglect|nose|nasal|hernia|chori|gas|intest',all_illness,v=T)

other_illness=setdiff(all_illness,c(dominating_illness,"-1",''))

####Generating illness variables:
source('diagnosis_recoding.R')
source('pareto.R')

###-----------------------Analysis of admission and mortality variation by age: consolidation analyses from all datasets:
# CIN_P$age_cat=NULL
# CIN_P$age_cat[CIN_P$age_mths_rcd<1]=1
# CIN_P$age_cat[CIN_P$age_mths_rcd>=1 & CIN_P$age_mths_rcd<=4]=2
# CIN_P$age_cat[CIN_P$age_mths_rcd>4 & CIN_P$age_mths_rcd<=11]=3
# CIN_P$age_cat[CIN_P$age_mths_rcd> 11]=4
# CIN_P$age_cat=factor(CIN_P$age_cat, levels = 1:4, labels = c('<1 month','1 - 4 months', '5 - 11 months','>=12 months'))

CIN_P$days_age=CIN_P$age_mths_rcd*30
CIN_P$age_cat=NULL
CIN_P$age_cat[CIN_P$days_age<=6]=1
CIN_P$age_cat[CIN_P$days_age> 6 & CIN_P$days_age<= 28]=2
CIN_P$age_cat[CIN_P$days_age> 28 & CIN_P$days_age<= 180]=3
CIN_P$age_cat[CIN_P$days_age> 180 & CIN_P$days_age<= 360]=4
CIN_P$age_cat[CIN_P$days_age>360]=5
CIN_P$age_cat=factor(CIN_P$age_cat, levels = 1:5, labels = c('0 - 6 days','7 - 28 days','1 - 6 months' ,'7 - 12 months','1 - 13 years'))

nCIN_P=subset(CIN_P, days_age<=30*13*12)
###
##Admissions:
nCIN_P$hosp_id=factor(nCIN_P$hosp_id)
mort_paed=rbind(table(nCIN_P$hosp_id[nCIN_P$outcome=='Died'],nCIN_P$age_cat[nCIN_P$outcome=='Died']),
                table(nCIN_P$age_cat[nCIN_P$outcome=='Died']))
hosp_n_total=rbind(table(nCIN_P$hosp_id,nCIN_P$age_cat),table(nCIN_P$age_cat))


###Age analysis CIN N:

CIN_N$revised_age_days=NULL
CIN_N$revised_age_days[CIN_N$age_days<=6]=1
CIN_N$revised_age_days[is.na(CIN_N$revised_age_days)]=2
CIN_N$revised_age_days=factor(CIN_N$revised_age_days, levels = 1:2, labels = c('0 - 6 days','7 - 28 days'))

mort_neonatal=rbind(table(CIN_N$hosp_id[CIN_N$outcome=='Dead'],CIN_N$revised_age_days[CIN_N$outcome=='Dead']),
                table(CIN_N$revised_age_days[CIN_N$outcome=='Dead']))
hospneonatal_n_total=rbind(table(CIN_N$hosp_id,CIN_N$revised_age_days),table(CIN_N$revised_age_days))


###Analysis of Kilifi data:
kilifi$age=NULL
kilifi$age=as.numeric(as.character(kilifi$age_days))

kilifi$age_cat=NULL
kilifi$age_cat[kilifi$age<=6]=1
kilifi$age_cat[is.na(kilifi$age_cat)]=2
kilifi$age_cat=factor(kilifi$age_cat, levels = 1:2, labels = c('0 - 6 days','7 - 28 days'))

table(kilifi$age_cat)
table(as.character(kilifi$vital_status))
table(kilifi$age_cat,as.character(kilifi$vital_status))


#####################Funnel plots by age:
###Calling the funnel function:
age_mortality=read.csv('Mortality_data_for_anaysis.csv')

d1=subset(age_mortality,select=c('Hospital','n1','d1'))
d2=subset(age_mortality,select=c('Hospital','n2','d2'))
d3=subset(age_mortality,select=c('Hospital','n3','d3'))
d4=subset(age_mortality,select=c('Hospital','n4','d4'))
d5=subset(age_mortality,select=c('Hospital','n5','d5'))
Tot=subset(age_mortality,select=c('Hospital','dN','dT'))

colnames(d1)=c("Hospital",'n','d')
colnames(d2)=c("Hospital",'n','d')
colnames(d3)=c("Hospital",'n','d')
colnames(d4)=c("Hospital",'n','d')
colnames(d5)=c("Hospital",'n','d')
colnames(Tot)=c("Hospital",'n','d')

# plot_grid(
#   funnel_plot_function(datum=Tot, foot='All admissions'),
#   funnel_plot_function(datum=d1, foot='Under 1 month (Newborn units) mortality per hospital'),
#   funnel_plot_function(datum=d2, foot='Under 1 month (Paediatric wards) mortality per hospital'),
#   funnel_plot_function(datum=d3, foot='1 - 4 months mortality per hospital'),
#   funnel_plot_function(datum=d4, foot='5 - 11 months mortality per hospital'),
#   funnel_plot_function(datum=d5, foot='>=12 months mortality per hospital')
#   ,labels = "AUTO")

plot_grid(
  funnel_plot_function(datum=Tot, foot='All admissions'),
  funnel_plot_function(datum=d1, foot='0 - 6 days mortality per hospital'),
  funnel_plot_function(datum=d2, foot='7 - 28 days mortality per hospital'),
  funnel_plot_function(datum=d3, foot='1 - 6 months mortality per hospital'),
  funnel_plot_function(datum=d4, foot='7 - 12 months mortality per hospital'),
  funnel_plot_function(datum=d5, foot='1 - 13 years mortality per hospital')
  ,labels = "AUTO")

####Further cleaning of all diagnoses:
#eval(parse(text=paste0('diagres',1:nrow(CIN_N),'=unique(c(',CIN_N$all_diagnoses,'))')))
#eval(parse(text=paste0('unique(c(',CIN_N$all_diagnoses,'))')))
# 
# i=NULL
# new_all_diagnosis=c()
# 
# for (i in 1:nrow(CIN_N))
# {
#   datum = CIN_N[i,]
#   diag= unique(eval(parse(text=paste0('c(',datum$all_diagnoses,')'))))#[unique(eval(parse(text=paste0('c(',datum$all_diagnoses,')'))))!="NA"]
#   new_all_diagnosis=c(new_all_diagnosis,diag)
# }


###################Other outputs:
mort_by_birth=rbind(table(CIN_N$hosp_id[CIN_N$outcome=='Dead'],CIN_N$bwt_cat[CIN_N$outcome=='Dead']),
                    table(CIN_N$hosp_id,CIN_N$bwt_cat))


####################------------Standalone analysis of the Kilifi data
nrow(kilifi)
sum(table(as.character(kilifi$sex))[c('f','F')])
sum(prop.table(table(as.character(kilifi$sex)))[c('f','F')])

##age for descriptive analysis
kilifi$age_descriptive=NULL
kilifi$age_descriptive[kilifi$age==0]=0
kilifi$age_descriptive[kilifi$age==1]=1
kilifi$age_descriptive[kilifi$age>1 & kilifi$age<=7]=2
kilifi$age_descriptive[kilifi$age>7]=3

table(kilifi$age_descriptive)
prop.table(table(kilifi$age_descriptive))
##gestation - no data

###mode of delivery:
kilifi$mode_of_delivery[kilifi$mode_of_delivery==''|kilifi$mode_of_delivery=='ventouse']=NA
table(factor(kilifi$mode_of_delivery))
sum(table(factor(kilifi$mode_of_delivery)))
prop.table(table(factor(kilifi$mode_of_delivery)))

##Birthweight:
kilifi$weight=as.numeric(as.character(kilifi$weight))
######bwt categories:
kilifi$bwt_cat=NULL
kilifi$bwt_cat[kilifi$weight<1]=1
kilifi$bwt_cat[kilifi$weight>=1 & kilifi$weight<1.5]=2
kilifi$bwt_cat[kilifi$weight>=1.5 & kilifi$weight<2]=3
kilifi$bwt_cat[kilifi$weight>=2 & kilifi$weight<2.5]=4
kilifi$bwt_cat[kilifi$weight>=2.5 & kilifi$weight<=4]=5
kilifi$bwt_cat[kilifi$weight>4]=6

kilifi$bwt_cat=factor(kilifi$bwt_cat, levels = 1:6, labels = c('< 1 Kg','1 - < 1.5 Kg','1.5 - < 2 Kg','2 - < 2.5 Kg','2.5 - 4 Kg','> 4 Kg'))

table(kilifi$bwt_cat)
prop.table(table(kilifi$bwt_cat))
sum(table(kilifi$bwt_cat))

##born outside the facility:
kilifi$place_delivery[kilifi$place_delivery=='']=NA
kilifi$outborn=NULL
kilifi$outborn[kilifi$place_delivery=='hospital'|kilifi$place_delivery=='clinic'|kilifi$place_delivery=='maternity']=1
kilifi$outborn[kilifi$place_delivery=='home/dwelling'|kilifi$place_delivery=='other']=2
kilifi$outborn=factor(kilifi$outborn, levels = 1:2, labels = c('hospital','outborn'))

table(kilifi$outborn)
prop.table(table(kilifi$outborn))
sum(table(kilifi$outborn))

##Neonates with hypothermia at admission  
kilifi$temp_axilla=as.numeric(as.character(kilifi$temp_axilla))

#Hypothermia:36.5
kilifi$hypothermia=NULL
kilifi$hypothermia[kilifi$temp_axilla<36.5]=1
kilifi$hypothermia[kilifi$temp_axilla>=36.5]=0
kilifi$hypothermia=factor(kilifi$hypothermia,levels = 0:1, labels = c('No','Yes'))

table(kilifi$hypothermia)
prop.table(table(kilifi$hypothermia))
sum(table(kilifi$hypothermia))

##hiv exposure:No available data

####-----Kilifi diagnoses:

###diag variables:
kilifi_diag=unique(names(table(kilifi$diag_disch1)),names(table(kilifi$diag_disch1)))[-1]
diag_vars=gsub(' |[[:punct:]]','_',kilifi_diag)
eval(parse(text=paste0('kilifi$',diag_vars,'=NULL', sep='\n')))
eval(parse(text=paste0('kilifi$',diag_vars,'[', paste0(paste0('kilifi$diag_disch1==',"'",kilifi_diag,"'"),'|',
                                       paste0('kilifi$diag_disch2==',"'",kilifi_diag,"'"))   ,']=1',sep='\n')))
eval(parse(text=paste0('kilifi$',diag_vars,'[is.na(kilifi$',diag_vars,')]=0', sep='\n')))

###Tabulation function:
kilifi$new_bwt=NULL
kilifi$new_bwt[kilifi$bwt_cat=='< 1 Kg'|kilifi$bwt_cat=='1 - < 1.5 Kg'|kilifi$bwt_cat=='1.5 - < 2 Kg']=1
kilifi$new_bwt[kilifi$bwt_cat=='2 - < 2.5 Kg']=2
kilifi$new_bwt[kilifi$bwt_cat=='2.5 - 4 Kg'|kilifi$bwt_cat=='> 4 Kg']=3

###########
total_episodes=function(x = 'neonatal_sepsis')
{
  n_out = table(kilifi[,x])['1']
  n_out[is.na(n_out)]=0
  return (n_out)
}
total_kilifi_diag=sapply(diag_vars, total_episodes)

bwt1_episodes=function(x = 'neonatal_sepsis')
{
  bwt1=subset(kilifi, new_bwt==1)
  n_out = table(bwt1[,x])['1']
  n_out[is.na(n_out)]=0
  return (n_out)
}
bwt1_kilifi_diag=sapply(diag_vars, bwt1_episodes)

bwt2_episodes=function(x = 'neonatal_sepsis')
{
  bwt2=subset(kilifi, new_bwt==2)
  n_out = table(bwt2[,x])['1']
  n_out[is.na(n_out)]=0
  return (n_out)
}
bwt2_kilifi_diag=sapply(diag_vars, bwt2_episodes)

bwt3_episodes=function(x = 'neonatal_sepsis')
{
  bwt3=subset(kilifi, new_bwt==3)
  n_out = table(bwt3[,x])['1']
  n_out[is.na(n_out)]=0
  return (n_out)
}
bwt3_kilifi_diag=sapply(diag_vars, bwt3_episodes)

all_kilifi_diag=cbind(total_kilifi_diag,bwt1_kilifi_diag,bwt2_kilifi_diag,bwt3_kilifi_diag)











