mod_under1_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='< 1 Kg' & CIN_N$los<=2], CIN_N$hosp_id[CIN_N$bwt_cat=='< 1 Kg' & CIN_N$los<=2])['Dead',]
mod_onetoless1.5_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='1 - < 1.5 Kg'& CIN_N$los<=2], CIN_N$hosp_id[CIN_N$bwt_cat=='1 - < 1.5 Kg'& CIN_N$los<=2])['Dead',]
mod_onepoint5toless2_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='1.5 - < 2 Kg'], CIN_N$hosp_id[CIN_N$bwt_cat=='1.5 - < 2 Kg'])['Dead',]
mod_twotoless2.5_mortality=table(CIN_N$outcome[CIN_N$bwt_cat=='2 - < 2.5 Kg'& CIN_N$los<=2], CIN_N$hosp_id[CIN_N$bwt_cat=='2 - < 2.5 Kg'& CIN_N$los<=2])['Dead',]
mod_twopoint5andabove_mortality=table(CIN_N$outcome[(CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=="> 4 Kg")& CIN_N$los<=2], CIN_N$hosp_id[(CIN_N$bwt_cat=='2.5 - 4 Kg'|CIN_N$bwt_cat=="> 4 Kg")& CIN_N$los<=2])['Dead',]
###Totals
mod_under1_total=table(CIN_N$bwt_cat[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])['< 1 Kg',]
mod_onetoless1.5_total=table(CIN_N$bwt_cat[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])['1 - < 1.5 Kg',]
mod_onepoint5toless2_total=table(CIN_N$bwt_cat[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])['1.5 - < 2 Kg',]
mod_twotoless2.5_total=table(CIN_N$bwt_cat[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])['2 - < 2.5 Kg',]
mod_twopoint5andabove_total=colSums(table(CIN_N$bwt_cat[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])[c('2.5 - 4 Kg','> 4 Kg'),])

mod_under1_data=data.frame(hosp=paste0('H',1:16),d=mod_under1_total,n=mod_under1_mortality)
mod_onetoless1.5_data=data.frame(hosp=paste0('H',1:16),d=mod_onetoless1.5_total,n=mod_onetoless1.5_mortality)
mod_onepoint5toless2_data=data.frame(hosp=paste0('H',1:16),d=mod_onepoint5toless2_total,n=mod_onepoint5toless2_mortality)
mod_twotoless2.5_data=data.frame(hosp=paste0('H',1:16),d=mod_twotoless2.5_total,n=mod_twotoless2.5_mortality)
mod_twopoint5andabove_data=data.frame(hosp=paste0('H',1:16),d=mod_twopoint5andabove_total,n=mod_twopoint5andabove_mortality)

###overall mortality per hospital
mod_overall_mortality=table(CIN_N$outcome[CIN_N$los<=2], CIN_N$hosp_id[CIN_N$los<=2])['Dead',]
mod_total_data=c(table(CIN_N$hosp_id[CIN_N$los<=2]))
names(mod_total_data)=NULL
mod_all_data=data.frame(hosp=paste0('H',1:16),d=mod_total_data,n=mod_overall_mortality)

###Data to export:
all_mortality_data=cbind(Hospital=paste0('H',1:16),with(mod_all_data,n/d),with(mod_under1_data,n/d),with(mod_onetoless1.5_data,n/d),
                         with(mod_onepoint5toless2_data,n/d),with(mod_twotoless2.5_data,n/d),with(mod_twopoint5andabove_data,n/d))

###Calling the funnel function:
plot_grid(
  funnel_plot_function(mod_all_data),
  funnel_plot_function(datum=mod_under1_data, foot='< 1000g birthweight admissions for each hospital'),
  funnel_plot_function(datum=mod_onetoless1.5_data, foot='1000 - < 1500g birthweight admissions for each hospital'),
  funnel_plot_function(datum=mod_onepoint5toless2_data, foot='1500 - < 2000g birthweight admissions for each hospital'),
  funnel_plot_function(datum=mod_twotoless2.5_data, foot='2000 - < 2500g birthweight admissions for each hospital'),
  funnel_plot_function(datum=mod_twopoint5andabove_data, foot='>=2500g birthweight admissions for each hospital')
  ,labels = "AUTO")