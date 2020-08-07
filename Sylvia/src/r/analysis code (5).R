rm(list = ls())
setwd('C:/data/extras/Sylvia MSc')
data = read.csv('Database (8)-sylviaCSV.csv')

data$hosp[data$hosp==' KNH'|data$hosp=='knh'|data$hosp=='KHN']='KNH'
data$hosp[data$hosp=='PRIVATE ']='PRIVATE'

####psq deriving
variables=c('snore_more','snore_always','snore_badly','heavy_loud_breathing','rouble_breathing','stop_beathing_night','breath_thru_mouth','dry_mouth',
            'wet_bed','waking_unrefreshed','sleeping_prob','sleepy_day_b','hard_wake','wake_headaches','stopped_growing','overweight',
            'listen','diff_organizing','easily_distructed','fidgets_hands','active','intrudes')


variables_b=c('snore_more_b','snore_always_b','snore_badly_b','snore_heavy_b','trouble_breathing_b',
              'V71','V72','V73',
            'V74','unfreshed_b','prob_sleeping','sleepy_day','hard_wake_b','headache_b','not_growing_b','overweight_b',
            'listens_b','diff_tasks','distracted_b','fidgets_b','active_b','interrupts_b')
####
eval(parse(text=paste0('data$',variables,'[data$',variables,'==0]=NA', sep='\n')))
eval(parse(text=paste0('data$',variables,'[data$',variables,'==2]=0', sep='\n')))

###Generating denominator:
eval(parse(text=paste0('data$denom_',variables,'=NULL', sep='\n')))
eval(parse(text=paste0('data$denom_',variables,'[!is.na(data$',variables,')]=1',sep = '\n')))
eval(parse(text=paste0('data$denom_',variables,'[is.na(data$denom_',variables,')]=0',sep = '\n')))

data$final.denom=NULL
data$final.denom= eval(parse(text=paste0('data$denom_',variables, collapse = '+')))

####Generating the numerator
eval(parse(text=paste0('data$',variables,'[is.na(data$',variables,')]=0', sep='\n')))
data$final.num=NULL
data$final.num= eval(parse(text=paste0('data$',variables, collapse = '+')))

data$psq_score=data$final.num/data$final.denom

data$sdb=NULL
data$sdb[data$psq_score<0.33]=0
data$sdb[data$psq_score>=0.33]=1
data$sdb=factor(data$sdb, levels = 0:1, labels = c('No sdb','sdb'))

##############################
####
eval(parse(text=paste0('data$',variables_b,'[data$',variables_b,'==0]=NA', sep='\n')))
eval(parse(text=paste0('data$',variables_b,'[data$',variables_b,'==2]=0', sep='\n')))

###Generating denominator:
eval(parse(text=paste0('data$denom_',variables_b,'=NULL', sep='\n')))
eval(parse(text=paste0('data$denom_',variables_b,'[!is.na(data$',variables_b,')]=1',sep = '\n')))
eval(parse(text=paste0('data$denom_',variables_b,'[is.na(data$denom_',variables_b,')]=0',sep = '\n')))

data$bfinal.denom=NULL
data$bfinal.denom= eval(parse(text=paste0('data$denom_',variables_b, collapse = '+')))

####Generating the numerator
eval(parse(text=paste0('data$',variables_b,'[is.na(data$',variables_b,')]=0', sep='\n')))
data$bfinal.num=NULL
data$bfinal.num= eval(parse(text=paste0('data$',variables_b, collapse = '+')))

data$bpsq_score=data$bfinal.num/data$bfinal.denom

data$bsdb=NULL
data$bsdb[data$bpsq_score<0.33]=0
data$bsdb[data$bpsq_score>=0.33]=1
data$bsdb=factor(data$bsdb, levels = 0:1, labels = c('No sdb','sdb'))

###
data=subset(data,sdb=='sdb')
##Variable cleaning:
data$sex=factor(data$sex, levels = 0:1, labels = c('Female','Male'))
data$bth_wght=as.numeric(as.character(data$bth_wght))

data$bwt_cat=NULL
data$bwt_cat[data$bth_wght<2.5]=1
data$bwt_cat[data$bth_wght>=2.5 & data$bth_wght<=4]=2
data$bwt_cat[data$bth_wght>4]=3
data$bwt_cat=factor(data$bwt_cat, levels = 1:3, labels = c('LBWT','Normal','LGA'))

data$age_mnths[is.na(data$age_mnths)]=0
data$new_age=NULL
data$new_age=as.numeric(as.character(data$age_yrs))+(as.numeric(as.character(data$age_mnths))/12)

data$age_cat=NULL
data$age_cat[data$new_age<=6]=1
data$age_cat[data$new_age>6 & data$new_age <=12]=2
data$age_cat[data$new_age>12]=3
data$age_cat=factor(data$age_cat, levels = 1:3, labels = c('2 - 6 years','>6 - 12','> 12'))

###Caregiver characteristics:
data$caregiver_agecat=NULL
data$caregiver_agecat[data$V9 <35]=1
data$caregiver_agecat[data$V9 >=35]=2
data$caregiver_agecat=factor(data$caregiver_agecat, levels = 1:2, labels = c('<35','>=35'))

data$employment=factor(data$employment, levels = 1:3, labels = c('Employed','Self - employed','Unemployed'))

data$income=NULL
data$income[is.na(data$wealth_quantile)]=1
data$income[data$wealth_quantile<=5000]=2
data$income[data$wealth_quantile>5000 & data$wealth_quantile<=10000]=3
data$income[data$wealth_quantile>10000 & data$wealth_quantile<=15000]=4
data$income[data$wealth_quantile>15000 & data$wealth_quantile<=20000]=5
data$income[data$wealth_quantile>20000]=6
data$income=factor(data$income, levels = 1:6, labels = c('Not employed','<= Ksh 5000',
                                                         '>Ksh 5000 - 10000','>Ksh. 10000 - <=15000',
                                                         '>Ksh. 15000 - <=20000','>Ksh. 20000'))

data$insuarance[data$insuarance==4]=2
data$insuarance=factor(data$insuarance, levels = 1:3, 
                       labels = c('NHIF','Employer','None'))

###Comordities:
data$new_asthma=NULL

data$new_asthma[data$child_asthma==1| data$wheezing==1|data$medication_a==1|
                  data$medication_b==1|data$medication_d==1|data$medication==1|
                  data$medication_e==1]=1
data$new_asthma[is.na(data$new_asthma)]=0
data$new_asthma=factor(data$new_asthma, levels = 0:1, labels = c('No asthma','Asthma'))


data$new_rhinitis=NULL
data$new_rhinitis[data$allergic_rhinitis==1| data$a_symptoms==1|data$b_symptoms==1|
                    data$c_symptoms==1| (data$V26==1|data$V26==2)|
                    data$findings_b==1|data$c_findings==1]=1
data$new_rhinitis[is.na(data$new_rhinitis)]=0
data$new_rhinitis=factor(data$new_rhinitis, levels = 0:1, labels = c('No rhinitis','Rhinitis'))

data$antiacids=factor(data$antiacids, 0:1, labels = c('No antacids','Uses antacids'))

data$exposed_cigarret=factor(data$exposed_cigarret, levels = 0:1, labels = c('Not exposed to smoke','Exposed to smoke'))


###computing cumulative score:
data$cum_scoreA=(data$shake_breath+data$apnoea_during_sleep)/2
data$cum_scoreB=(data$cum_scoreA + data$struggle_breathing)/2
data$cum_scoreC=(data$cum_scoreB + data$conserns)/2
data$cum_scoreD=(data$cum_scoreC + data$snore_loud)/2
data$cum_scoreE=(data$cum_scoreD + data$snore_sleep)/2


data$osa=NULL
data$osa[data$cum_scoreE<2.72]=0
data$osa[data$cum_scoreE>=2.72]=1
data$osa=factor(data$osa, levels = 0:1, labels = c('No osa','osa'))

#demog.variables=c('hosp','sex','bwt_cat','age_cat','caregiver_agecat','employment','insuarance','new_asthma','new_rhinitis','antiacids','exposed_cigarret','bmi_category')
demog.variables=c('hosp','sex','bwt_cat','age_cat','caregiver_agecat','employment','insuarance','new_asthma','new_rhinitis','antiacids','exposed_cigarret')



######:Socio - demographic frequencies

demog.function = function (var = 'hosp')
{
  n = table (data[,var])
  perc_= round(prop.table(n)*100,1)
  lab = c(var, rep('',length(names(n))-1)) 
  all_res=cbind(lab,n,perc_)
  return(all_res)
}

demog.results = do.call('rbind',sapply(demog.variables,demog.function))


######Residual prevalence:


residsdb.prevalence = function (var = 'hosp')
{
  
  prev = round(prop.table(table(data[, var],data$bsdb),1)[,'sdb'],2)
  denom_n = rowSums(table(data[, var],data$bsdb))
  
  lower_p=prev - 1.96*sqrt(prev*(1-prev)/denom_n)
  upper_p=prev + 1.96*sqrt(prev*(1-prev)/denom_n)
  
  lower_p[lower_p<0]=0
  upper_p[upper_p>1]=1
  lower_p=format(round(lower_p,2), nsmall = 2)
  upper_p=format(round(upper_p,2), nsmall = 2)
  prev=format(round(prev,2), nsmall = 2)
  lab = c(var, rep('',length(names(denom_n))-1)) 
  
  results = cbind(lab,names(denom_n),paste0(prev,' [',lower_p,' - ',upper_p,']'))
  return (results)
  
}

prev.results = do.call('rbind',sapply(demog.variables,residsdb.prevalence))

##################Univariate tests

#Fisher's exact test

fishertest.fn = function (var = 'hosp')
{
  p_value = fisher.test(data$bsdb, data[,var], simulate.p.value = T)
  
  if (length(names(table(data[,var])))==2)
  {
  odds = p_value$estimate
  interval = p_value$conf.int
  pvalued = round(p_value$p.value,4)
  result = cbind(var,paste0(format(round(odds,2), nsmall = 2), ' [',format(round(interval[1],2),nsmall = 2), ' - ',format(round(interval[2],2),nsmall=2),']'),pvalued)
  return(result)
  }
  
  if (length(names(table(data[,var])))>2)
  {
    pvalued = round(p_value$p.value,4)
    result = c(var,'',pvalued)
    return(result)
  }
  
}

fisher.results = do.call('rbind',lapply(demog.variables,fishertest.fn))


###logistf regression:

library(logistf)

data$bsdb=relevel(data$bsdb, ref = 'No sdb')

########################################################
unadjusted.analysis = function (var ='hosp')
{
  model = glm(bsdb ~ data[,var], data=data, family = binomial('logit'))
  print(var)
  summary(model)
}

sapply(demog.variables,unadjusted.analysis)

###Multivariable logistic regression adjusting for:

adjusted.model = glm(bsdb ~ hosp+sex+bwt_cat+age_cat+caregiver_agecat+employment+insuarance, data=data, family = binomial('logit'))

##reduced mode:excluding hosp and insurance
reduced.model = glm(bsdb ~ sex+bwt_cat+age_cat+caregiver_agecat+employment, data=data, family = binomial('logit'))






############################################################################################################################################
# tabulation_fn = function (var = 'hosp')
# {
#   ##Demog characteristics
#   data[,var]=factor(data[,var])
#   freq_n = table(data[, var])
#   perc_d= format(round(prop.table(table(data[, var]))*100,1), nsmall=1)
#   results = cbind(freq_n,perc_d)
#   ##SDB
#   freq_n1 = table(data[, var],data$sdb)
#   perc_d1= format(round(prop.table(table(data[, var],data$sdb),1)*100,1), nsmall=1)
#   
#   results1=cbind(freq_n1,perc_d1)
#   
#   ##OSA
#   freq_n2 = table(data[, var],data$osa)
#   perc_d2= format(round(prop.table(table(data[, var],data$osa),1)*100,1), nsmall=1)
#   results2=cbind(freq_n2,perc_d2)
#   
#   all_results =cbind(results,results1,results2)
#   return(all_results)
# }
# 
# refq_findings = do.call('rbind',sapply(variables,tabulation_fn))
# 
# 
# library(arm)
# 
# logistf_model1=function(var ='hosp')
# {
#   model1=bayesglm(sdb ~ data[,var], data=data, family = binomial('logit'))
#   model1_coef=format(round(exp(coef(summary(model1))[,1]),1),nsmall=1)
#   model1_std=format(round(exp(coef(summary(model1))[,2]),1),nsmall=1)
#   model1_pvalue=format(round(coef(summary(model1))[,4],4),nsmall=4)
#   all_model1=cbind(names(model1$coef),model1_coef,model1_std,model1_pvalue)[-1,]
#   return(all_model1)
# }
# 
# sdb_findings = do.call('rbind',sapply(variables,logistf_model1))
# 
# 
# logistf_model2=function(var ='hosp')
# {
#   model1=bayesglm(osa ~ data[,var], data=data, family = binomial('logit'))
#   model1_coef=format(round(exp(coef(summary(model1))[,1]),1),nsmall=1)
#   model1_std=format(round(exp(coef(summary(model1))[,2]),1),nsmall=1)
#   model1_pvalue=format(round(coef(summary(model1))[,4],4),nsmall=4)
#   all_model1=cbind(names(model1$coef),model1_coef,model1_std,model1_pvalue)[-1,]
#   return(all_model1)
# }
# osa_findings = do.call('rbind',sapply(variables,logistf_model2))
# 
# 
# ######Chi square results:
# i=NULL
# for (i in variables)
# {
#   gen_chi=fisher.test(data[,i], data$sdb,simulate.p.value=TRUE)
#   capture.output(print(gen_chi),file = paste0(i,'_with_sdb_chi_square_tests.txt'))
# }
# 
# i=NULL
# for (i in variables)
# {
#   gen_chi=fisher.test(data[,i], data$osa,simulate.p.value=TRUE)
#   capture.output(print(gen_chi),file = paste0(i,'_with_osa_chi_square_tests.txt'))
# }













