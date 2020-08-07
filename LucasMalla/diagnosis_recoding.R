
text_illnesses=c(adm_dx,disch_dx)

##Prematurity:
immature_vars=tolower(grep('preterm|prem|immatur', unique(all_illness), v=T))
CIN_N$immature=NULL
eval(parse(text=paste0("CIN_N$immature[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',immature_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',immature_vars, '"', collapse = '|'),"]=1")))
CIN_N$immature[is.na(CIN_N$immature)]=0
CIN_N$immature=factor(CIN_N$immature, levels = 0:1, labels = c("No","Yes"))

###Low birthweight:
bwt_vars=tolower(grep('lbw|weight|small', unique(all_illness), v=T))
CIN_N$lbwt=NULL
eval(parse(text=paste0("CIN_N$lbwt[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',bwt_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',bwt_vars, '"', collapse = '|'),"]=1")))
CIN_N$lbwt[is.na(CIN_N$lbwt)]=0
CIN_N$lbwt=factor(CIN_N$lbwt, levels = 0:1, labels = c("No","Yes"))

###asphyxia
asphyxia_vars=tolower(grep('asphyxia', unique(all_illness), v=T))
CIN_N$asphyxia=NULL
eval(parse(text=paste0("CIN_N$asphyxia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',asphyxia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',asphyxia_vars, '"', collapse = '|'),"]=1")))
CIN_N$asphyxia[is.na(CIN_N$asphyxia)]=0
CIN_N$asphyxia=factor(CIN_N$asphyxia, levels = 0:1, labels = c("No","Yes"))

sepsis_vars=tolower(grep('sepsis', unique(all_illness), v=T))
CIN_N$sepsis=NULL
eval(parse(text=paste0("CIN_N$sepsis[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',sepsis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',sepsis_vars, '"', collapse = '|'),"]=1")))
CIN_N$sepsis[is.na(CIN_N$sepsis)]=0
CIN_N$sepsis=factor(CIN_N$sepsis, levels = 0:1, labels = c("No","Yes"))

###distress
distress_vars=tolower(grep('distress|RDS|respiratory', unique(all_illness), v=T))
CIN_N$distress=NULL
eval(parse(text=paste0("CIN_N$distress[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',distress_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',distress_vars, '"', collapse = '|'),"]=1")))
CIN_N$distress[is.na(CIN_N$distress)]=0
CIN_N$distress=factor(CIN_N$distress, levels = 0:1, labels = c("No","Yes"))

###jaundice
jaundice_vars=tolower(grep('jaundice', unique(all_illness), v=T))
CIN_N$jaundice=NULL
eval(parse(text=paste0("CIN_N$jaundice[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',jaundice_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',jaundice_vars, '"', collapse = '|'),"]=1")))
CIN_N$jaundice[is.na(CIN_N$jaundice)]=0
CIN_N$jaundice=factor(CIN_N$jaundice, levels = 0:1, labels = c("No","Yes"))

###Twin
twin_vars=tolower(grep('twin', unique(all_illness), v=T))
CIN_N$var_twin=NULL
eval(parse(text=paste0("CIN_N$var_twin[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',twin_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',twin_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_twin[is.na(CIN_N$var_twin)]=0
CIN_N$var_twin=factor(CIN_N$var_twin, levels = 0:1, labels = c("No","Yes"))

###meconium
meconium_vars=tolower(grep('meconium', unique(all_illness), v=T))
CIN_N$meconium=NULL
eval(parse(text=paste0("CIN_N$meconium[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',meconium_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',meconium_vars, '"', collapse = '|'),"]=1")))
CIN_N$meconium[is.na(CIN_N$meconium)]=0
CIN_N$meconium=factor(CIN_N$meconium, levels = 0:1, labels = c("No","Yes"))

####
###well
well_vars=tolower(grep('accompany|well|stable|accom|norm', unique(all_illness), v=T))
CIN_N$well=NULL
eval(parse(text=paste0("CIN_N$well[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',well_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',well_vars, '"', collapse = '|'),"]=1")))
CIN_N$well[is.na(CIN_N$well)]=0
CIN_N$well=factor(CIN_N$well, levels = 0:1, labels = c("No","Yes"))

###macrosomia
macrosomia_vars=tolower(grep('large|macrosomia|heavy|big|macro', unique(all_illness), v=T))
CIN_N$macrosomia=NULL
eval(parse(text=paste0("CIN_N$macrosomia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',macrosomia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',macrosomia_vars, '"', collapse = '|'),"]=1")))
CIN_N$macrosomia[is.na(CIN_N$macrosomia)]=0
CIN_N$macrosomia=factor(CIN_N$macrosomia, levels = 0:1, labels = c("No","Yes"))

###dehydration/volume/shock
dehydration_vars=tolower(grep('dehyd|shock|dhydration|dehyrdation|dehdration', unique(all_illness), v=T))
CIN_N$dehydration=NULL
eval(parse(text=paste0("CIN_N$dehydration[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',dehydration_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',dehydration_vars, '"', collapse = '|'),"]=1")))
CIN_N$dehydration[is.na(CIN_N$dehydration)]=0
CIN_N$dehydration=factor(CIN_N$dehydration, levels = 0:1, labels = c("No","Yes"))

###transient tachyponea
transient_tachyponea_vars=tolower(grep('tach', unique(all_illness), v=T))
CIN_N$tachyponea=NULL
eval(parse(text=paste0("CIN_N$tachyponea[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',transient_tachyponea_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',transient_tachyponea_vars, '"', collapse = '|'),"]=1")))
CIN_N$tachyponea[is.na(CIN_N$tachyponea)]=0
CIN_N$tachyponea=factor(CIN_N$tachyponea, levels = 0:1, labels = c("No","Yes"))

###hiv exposed
hiv_vars=tolower(grep('hiv', unique(all_illness), v=T))
CIN_N$hiv=NULL
eval(parse(text=paste0("CIN_N$hiv[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',hiv_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',hiv_vars, '"', collapse = '|'),"]=1")))
CIN_N$hiv[is.na(CIN_N$hiv)]=0
CIN_N$hiv=factor(CIN_N$hiv, levels = 0:1, labels = c("No","Yes"))

###Hypothermia and hyperthermia
hypothermia_vars=tolower(grep('thermia', unique(all_illness), v=T))
CIN_N$var_hypothermia=NULL
eval(parse(text=paste0("CIN_N$var_hypothermia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',hypothermia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',hypothermia_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_hypothermia[is.na(CIN_N$var_hypothermia)]=0
CIN_N$var_hypothermia=factor(CIN_N$var_hypothermia, levels = 0:1, labels = c("No","Yes"))

###Meningitis
meningitis_vars=tolower(grep('meningitis', unique(all_illness), v=T))
CIN_N$meningitis=NULL
eval(parse(text=paste0("CIN_N$meningitis[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',meningitis_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',meningitis_vars, '"', collapse = '|'),"]=1")))
CIN_N$meningitis[is.na(CIN_N$meningitis)]=0
CIN_N$meningitis=factor(CIN_N$meningitis, levels = 0:1, labels = c("No","Yes"))

###isoimmunisation
isoimmunisation_vars=tolower(grep('isoim', unique(all_illness), v=T))
CIN_N$isoimmunisation=NULL
eval(parse(text=paste0("CIN_N$isoimmunisation[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',isoimmunisation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',isoimmunisation_vars, '"', collapse = '|'),"]=1")))
CIN_N$isoimmunisation[is.na(CIN_N$isoimmunisation)]=0
CIN_N$isoimmunisation=factor(CIN_N$isoimmunisation, levels = 0:1, labels = c("No","Yes"))

###Hypoxic ischemic encephalopathy
poxic_vars=tolower(grep('poxic', unique(all_illness), v=T))
CIN_N$poxic=NULL
eval(parse(text=paste0("CIN_N$poxic[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',poxic_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',poxic_vars, '"', collapse = '|'),"]=1")))
CIN_N$poxic[is.na(CIN_N$poxic)]=0
CIN_N$poxic=factor(CIN_N$poxic, levels = 0:1, labels = c("No","Yes"))

##
###Caput Succedaneum
caput_vars=tolower(grep('caput', unique(all_illness), v=T))
CIN_N$caput=NULL
eval(parse(text=paste0("CIN_N$caput[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',caput_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',caput_vars, '"', collapse = '|'),"]=1")))
CIN_N$caput[is.na(CIN_N$caput)]=0
CIN_N$caput=factor(CIN_N$caput, levels = 0:1, labels = c("No","Yes"))

###observation
observation_vars=tolower(grep('observation', unique(all_illness), v=T))
CIN_N$observation=NULL
eval(parse(text=paste0("CIN_N$observation[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',observation_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',observation_vars, '"', collapse = '|'),"]=1")))
CIN_N$observation[is.na(CIN_N$observation)]=0
CIN_N$observation=factor(CIN_N$observation, levels = 0:1, labels = c("No","Yes"))

###var_convulsion
var_convulsion_vars=tolower(grep('convul', unique(all_illness), v=T))
CIN_N$var_convulsion=NULL
eval(parse(text=paste0("CIN_N$var_convulsion[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',var_convulsion_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',var_convulsion_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_convulsion[is.na(CIN_N$var_convulsion)]=0
CIN_N$var_convulsion=factor(CIN_N$var_convulsion, levels = 0:1, labels = c("No","Yes"))

###necrotizing enterocolitis
necrotising_vars=tolower(grep('necro', unique(all_illness), v=T))
CIN_N$var_necrotising=NULL
eval(parse(text=paste0("CIN_N$var_necrotising[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',necrotising_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',necrotising_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_necrotising[is.na(CIN_N$var_necrotising)]=0
CIN_N$var_necrotising=factor(CIN_N$var_necrotising, levels = 0:1, labels = c("No","Yes"))

###Birth related injuries
injury_vars=tolower(grep('injury|injuries', unique(all_illness), v=T))
CIN_N$var_injury=NULL
eval(parse(text=paste0("CIN_N$var_injury[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',injury_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',injury_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_injury[is.na(CIN_N$var_injury)]=0
CIN_N$var_injury=factor(CIN_N$var_injury, levels = 0:1, labels = c("No","Yes"))

###Hypoglycaemia
hypoglycaemia_vars=tolower(grep('hypog', unique(all_illness), v=T))
CIN_N$var_hypoglycaemia=NULL
eval(parse(text=paste0("CIN_N$var_hypoglycaemia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',hypoglycaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',hypoglycaemia_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_hypoglycaemia[is.na(CIN_N$var_hypoglycaemia)]=0
CIN_N$var_hypoglycaemia=factor(CIN_N$var_hypoglycaemia, levels = 0:1, labels = c("No","Yes"))

###Feeding difficulty
feed_vars=tolower(grep('feed|suck|poor', unique(all_illness), v=T))
CIN_N$var_feed=NULL
eval(parse(text=paste0("CIN_N$var_feed[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',feed_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',feed_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_feed[is.na(CIN_N$var_feed)]=0
CIN_N$var_feed=factor(CIN_N$var_feed, levels = 0:1, labels = c("No","Yes"))

###Fever
fever_vars=tolower(grep('fever', unique(all_illness), v=T))
CIN_N$var_fever=NULL
eval(parse(text=paste0("CIN_N$var_fever[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',fever_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',fever_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_fever[is.na(CIN_N$var_fever)]=0
CIN_N$var_fever=factor(CIN_N$var_fever, levels = 0:1, labels = c("No","Yes"))


###talipes
talipes_vars=tolower(grep('talipes', unique(all_illness), v=T))
CIN_N$var_talipes=NULL
eval(parse(text=paste0("CIN_N$var_talipes[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',talipes_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',talipes_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_talipes[is.na(CIN_N$var_talipes)]=0
CIN_N$var_talipes=factor(CIN_N$var_talipes, levels = 0:1, labels = c("No","Yes"))

###Down syndrome
down_vars=tolower(grep('down', unique(all_illness), v=T))
CIN_N$var_down=NULL
eval(parse(text=paste0("CIN_N$var_down[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',down_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',down_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_down[is.na(CIN_N$var_down)]=0
CIN_N$var_down=factor(CIN_N$var_down, levels = 0:1, labels = c("No","Yes"))

###
###Pneumonia 
pneumonia_vars=tolower(grep('pneum', unique(all_illness), v=T))
CIN_N$var_pneumonia=NULL
eval(parse(text=paste0("CIN_N$var_pneumonia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',pneumonia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',pneumonia_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_pneumonia[is.na(CIN_N$var_pneumonia)]=0
CIN_N$var_pneumonia=factor(CIN_N$var_pneumonia, levels = 0:1, labels = c("No","Yes"))

###cleft palate
cleft_vars=tolower(grep('cleft', unique(all_illness), v=T))
CIN_N$var_cleft=NULL
eval(parse(text=paste0("CIN_N$var_cleft[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',cleft_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',cleft_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_cleft[is.na(CIN_N$var_cleft)]=0
CIN_N$var_cleft=factor(CIN_N$var_cleft, levels = 0:1, labels = c("No","Yes"))

###Malformations
congenital_vars=tolower(grep('congenital|deform|malf', unique(all_illness), v=T))
CIN_N$var_congenital=NULL
eval(parse(text=paste0("CIN_N$var_congenital[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',congenital_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',congenital_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_congenital[is.na(CIN_N$var_congenital)]=0
CIN_N$var_congenital=factor(CIN_N$var_congenital, levels = 0:1, labels = c("No","Yes"))

###Spina Bifida
bifida_vars=tolower(grep('spina', unique(all_illness), v=T))
CIN_N$var_bifida=NULL
eval(parse(text=paste0("CIN_N$var_bifida[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',bifida_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',bifida_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_bifida[is.na(CIN_N$var_bifida)]=0
CIN_N$var_bifida=factor(CIN_N$var_bifida, levels = 0:1, labels = c("No","Yes"))

###Hydrocephalus
hydrocephal_vars=tolower(grep('hydroce', unique(all_illness), v=T))
CIN_N$var_hydrocephal=NULL
eval(parse(text=paste0("CIN_N$var_hydrocephal[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',hydrocephal_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',hydrocephal_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_hydrocephal[is.na(CIN_N$var_hydrocephal)]=0
CIN_N$var_hydrocephal=factor(CIN_N$var_hydrocephal, levels = 0:1, labels = c("No","Yes"))

###anaemiaus
anaemia_vars=tolower(grep('anaem|anem', unique(all_illness), v=T))
CIN_N$var_anaemia=NULL
eval(parse(text=paste0("CIN_N$var_anaemia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')=="',anaemia_vars, '"', collapse = '|'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')=="',anaemia_vars, '"', collapse = '|'),"]=1")))
CIN_N$var_anaemia[is.na(CIN_N$var_anaemia)]=0
CIN_N$var_anaemia=factor(CIN_N$var_anaemia, levels = 0:1, labels = c("No","Yes"))

##Volume depletion:
volume_var=tolower(grep('volume', unique(all_illness), v=T))
CIN_N$volume=NULL
eval(parse(text=paste0("CIN_N$volume[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(volume_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(volume_var)'),"]=1")))
CIN_N$volume[is.na(CIN_N$volume)]=0
CIN_N$volume=factor(CIN_N$volume, levels = 0:1, labels = c("No","Yes"))

##Nose/nasal problems:

nasal_var=tolower(grep('nose|nasal', unique(all_illness), v=T))
CIN_N$nose=NULL
eval(parse(text=paste0("CIN_N$nose[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(nasal_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(nasal_var)'),"]=1")))
CIN_N$nose[is.na(CIN_N$nose)]=0
CIN_N$nose=factor(CIN_N$nose, levels = 0:1, labels = c("No","Yes"))

hernia_var=tolower(grep('hernia', unique(all_illness), v=T))
CIN_N$hernia=NULL
eval(parse(text=paste0("CIN_N$hernia[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(hernia_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(hernia_var)'),"]=1")))
CIN_N$hernia[is.na(CIN_N$hernia)]=0
CIN_N$hernia=factor(CIN_N$hernia, levels = 0:1, labels = c("No","Yes"))

gastro_var=tolower(grep('gas|intest', unique(all_illness), v=T))
CIN_N$gastro=NULL
eval(parse(text=paste0("CIN_N$gastro[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(gastro_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(gastro_var)'),"]=1")))
CIN_N$gastro[is.na(CIN_N$gastro)]=0
CIN_N$gastro=factor(CIN_N$gastro, levels = 0:1, labels = c("No","Yes"))

choriomniotis_var=tolower(grep('chori', unique(all_illness), v=T))
CIN_N$choriomniotis=NULL
eval(parse(text=paste0("CIN_N$choriomniotis[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(choriomniotis_var)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(choriomniotis_var)'),"]=1")))
CIN_N$choriomniotis[is.na(CIN_N$choriomniotis)]=0
CIN_N$choriomniotis=factor(CIN_N$choriomniotis, levels = 0:1, labels = c("No","Yes"))




###other illnesses
CIN_N$other_illneses=NULL
eval(parse(text=paste0("CIN_N$other_illneses[",
                       paste0('tolower(CIN_N$',text_illnesses[1],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[2],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[3],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[4],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[5],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[6],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[7],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[8],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[9],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[10],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[11],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[12],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[13],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[14],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[15],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[16],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[17],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[18],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[19],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[20],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[21],')%in%c(other_illness)'),"|",
                       paste0('tolower(CIN_N$',text_illnesses[22],')%in%c(other_illness)'),"]=1")))

CIN_N$other_illneses[is.na(CIN_N$other_illneses)]=0
CIN_N$other_illneses=factor(CIN_N$other_illneses, levels = 0:1, labels = c("No","Yes"))

CIN_N$other=NULL
CIN_N$other[CIN_N$immature=='No'&CIN_N$lbwt=="No"&CIN_N$asphyxia=="No"&CIN_N$sepsis=="No"&CIN_N$distress=="No"&CIN_N$jaundice=="No"&CIN_N$var_twin=="No"&
              CIN_N$meconium=="No"&CIN_N$macrosomia=="No"&CIN_N$dehydration=="No"&CIN_N$tachyponea=="No"&CIN_N$hiv=="No"&CIN_N$var_hypothermia=="No"&
              CIN_N$meningitis=="No"&CIN_N$isoimmunisation=="No"&CIN_N$poxic=="No"&CIN_N$caput=="No"&CIN_N$observation=="No"&CIN_N$var_convulsion=="No"&
              CIN_N$var_necrotising=="No"&CIN_N$var_injury=="No"&CIN_N$var_hypoglycaemia=="No"&CIN_N$var_feed=="No"&CIN_N$var_fever=="No"&CIN_N$var_talipes=="No"&
              CIN_N$var_down=="No"&CIN_N$var_pneumonia=="No"&CIN_N$var_cleft=="No"&CIN_N$var_congenital=="No"&CIN_N$var_bifida=="No"&CIN_N$var_hydrocephal=="No"&
              CIN_N$var_anaemia=="No" &CIN_N$volume=='No' & CIN_N$nose =='No' & CIN_N$hernia == 'No' & CIN_N$gastro=='No' & CIN_N$choriomniotis=='No'&CIN_N$other_illneses=="Yes"]=1
CIN_N$other[is.na(CIN_N$other)]=0
CIN_N$other=factor(CIN_N$other, levels = 0:1, labels = c("No","Yes"))

####all illnesses excluding well
agg_illnesses =c('immature','lbwt','asphyxia','sepsis','distress','jaundice','var_twin','meconium','macrosomia','dehydration','tachyponea',
                 'hiv','var_hypothermia','meningitis','isoimmunisation','poxic','caput','observation','var_convulsion','var_necrotising',
                 'var_injury','var_hypoglycaemia','var_feed','var_fever','var_talipes','var_down','var_pneumonia','var_cleft',
                 'var_congenital','var_bifida','var_hydrocephal','var_anaemia','volume','nose','hernia','gastro','choriomniotis','other')


###Tose without any diagnosis
with(CIN_N, table(immature=='Yes'&lbwt=="No"&asphyxia=="No"&sepsis=="No"&distress=="No"&jaundice=="No"&var_twin=="No"&
                       meconium=="No"&macrosomia=="No"&dehydration=="No"&tachyponea=="No"&hiv=="No"&var_hypothermia=="No"&
                       meningitis=="No"&isoimmunisation=="No"&poxic=="No"&caput=="No"&observation=="No"&var_convulsion=="No"&
                       var_necrotising=="No"&var_injury=="No"&var_hypoglycaemia=="No"&var_feed=="No"&var_fever=="No"&var_talipes=="No"&
                       var_down=="No"&var_pneumonia=="No"&var_cleft=="No"&var_congenital=="No"&var_bifida=="No"&var_hydrocephal=="No"&
                       var_anaemia=="No" &volume=='No' & nose =='No' & hernia == 'No' & gastro=='No' & choriomniotis=='No'&other=="No"))

#Among those without diagnosis, how many are completely well babies:
# eval(parse(text=paste0('table(CIN_N$well=="Yes" &',paste0('CIN_N$',c('lbwt','asphyxia','sepsis','distress','jaundice','var_twin','meconium','macrosomia','dehydration','tachyponea',
#                         'hiv','var_hypothermia','meningitis','isoimmunisation','poxic','caput','observation','var_convulsion','var_necrotising',
#                         'var_injury','var_hypoglycaemia','var_feed','var_fever','var_talipes','var_down','var_pneumonia','var_cleft',
#                         'var_congenital','var_bifida','var_hydrocephal','var_anaemia','other_illneses'),'=="No"', collapse='&'),')')))
###Obtaining data subset excludimg all with missing diagnosis:
new_CIN_N=subset(CIN_N, immature=='Yes' |lbwt=="Yes"|asphyxia=="Yes"|sepsis=="Yes"|distress=="Yes"|jaundice=="Yes"|var_twin=="Yes"|
                                       meconium=="Yes"|macrosomia=="Yes"|dehydration=="Yes"|tachyponea=="Yes"|hiv=="Yes"|var_hypothermia=="Yes"|
                                       meningitis=="Yes"|isoimmunisation=="Yes"|poxic=="Yes"|caput=="Yes"|observation=="Yes"|var_convulsion=="Yes"|
                                       var_necrotising=="Yes"|var_injury=="Yes"|var_hypoglycaemia=="Yes"|var_feed=="Yes"|var_fever=="Yes"|var_talipes=="Yes"|
                                       var_down=="Yes"|var_pneumonia=="Yes"|var_cleft=="Yes"|var_congenital=="Yes"|var_bifida=="Yes"|var_hydrocephal=="Yes"|
                                       var_anaemia=="Yes"|other_illneses=="Yes" | nose=="Yes" | hernia =="Yes" | gastro =="Yes" | choriomniotis=="Yes")

# bwt_data1=as.data.frame(subset(new_CIN_N, bwt_cat=='< 1 Kg'))
# bwt_data2=as.data.frame(subset(new_CIN_N, bwt_cat=='1 - < 1.5 Kg'))
# bwt_data3=as.data.frame(subset(new_CIN_N, bwt_cat=='1.5 - < 2 Kg'))
# bwt_data4=as.data.frame(subset(new_CIN_N, bwt_cat=='2 - < 2.5 Kg'))
# bwt_data5=as.data.frame(subset(new_CIN_N, bwt_cat=='2.5 - 4 Kg'|bwt_cat=='> 4 Kg'))
##revised:
bwt_data1=as.data.frame(subset(new_CIN_N, bwt_cat=='< 1 Kg'|bwt_cat=='1 - < 1.5 Kg'|bwt_cat=='1.5 - < 2 Kg'))
bwt_data2=as.data.frame(subset(new_CIN_N, bwt_cat=='2 - < 2.5 Kg'))
bwt_data3=as.data.frame(subset(new_CIN_N, bwt_cat=='2.5 - 4 Kg'|bwt_cat=='> 4 Kg'))

diag_tab_function = function (bwt_data=bwt_data1)
{
  eval(parse(text=paste0('result',1:length(agg_illnesses),'=table(bwt_data$',agg_illnesses,')["Yes"]', sep ='\n')))
  numbers=cbind(agg_illnesses,eval(parse(text=paste0('c(',paste0('result',1:length(agg_illnesses), collapse = ','),')'))))
  return(numbers)
}
####Calling the function
all_diagnoses=cbind(diag_tab_function(new_CIN_N),diag_tab_function(bwt_data1),diag_tab_function(bwt_data2),diag_tab_function(bwt_data3))

##Those diagnosed with LBW/prematurity:

unique_illnesses=function (datum = new_CIN_N)
{

lbwt_n=with(datum, table(lbwt=="Yes"& immature=='No' &asphyxia=="No"&sepsis=="No"&distress=="No"&jaundice=="No"&var_twin=="No"&
                        meconium=="No"&macrosomia=="No"&dehydration=="No"&tachyponea=="No"&hiv=="No"&var_hypothermia=="No"&
                        meningitis=="No"&isoimmunisation=="No"&poxic=="No"&caput=="No"&observation=="No"&var_convulsion=="No"&
                        var_necrotising=="No"&var_injury=="No"&var_hypoglycaemia=="No"&var_feed=="No"&var_fever=="No"&var_talipes=="No"&
                        var_down=="No"&var_pneumonia=="No"&var_cleft=="No"&var_congenital=="No"&var_bifida=="No"&var_hydrocephal=="No"&
                        var_anaemia=="No"&other =="No" & nose=="No" & hernia =="No" & gastro =="No" & choriomniotis=="No"))['TRUE']
immature_n=with(datum, table(lbwt=="No"& immature=='Yes' &asphyxia=="No"&sepsis=="No"&distress=="No"&jaundice=="No"&var_twin=="No"&
                           meconium=="No"&macrosomia=="No"&dehydration=="No"&tachyponea=="No"&hiv=="No"&var_hypothermia=="No"&
                           meningitis=="No"&isoimmunisation=="No"&poxic=="No"&caput=="No"&observation=="No"&var_convulsion=="No"&
                           var_necrotising=="No"&var_injury=="No"&var_hypoglycaemia=="No"&var_feed=="No"&var_fever=="No"&var_talipes=="No"&
                           var_down=="No"&var_pneumonia=="No"&var_cleft=="No"&var_congenital=="No"&var_bifida=="No"&var_hydrocephal=="No"&
                           var_anaemia=="No"&other =="No" & nose=="No" & hernia =="No" & gastro =="No" & choriomniotis=="No"))['TRUE']
hiv_n=with(datum, table(lbwt=="No"& immature=='No' &asphyxia=="No"&sepsis=="No"&distress=="No"&jaundice=="No"&var_twin=="No"&
                               meconium=="No"&macrosomia=="No"&dehydration=="No"&tachyponea=="No"&hiv=="Yes"&var_hypothermia=="No"&
                               meningitis=="No"&isoimmunisation=="No"&poxic=="No"&caput=="No"&observation=="No"&var_convulsion=="No"&
                               var_necrotising=="No"&var_injury=="No"&var_hypoglycaemia=="No"&var_feed=="No"&var_fever=="No"&var_talipes=="No"&
                               var_down=="No"&var_pneumonia=="No"&var_cleft=="No"&var_congenital=="No"&var_bifida=="No"&var_hydrocephal=="No"&
                               var_anaemia=="No"&other =="No" & nose=="No" & hernia =="No" & gastro =="No" & choriomniotis=="No"))['TRUE']
twin_n=with(datum, table(lbwt=="No"& immature=='No' &asphyxia=="No"&sepsis=="No"&distress=="No"&jaundice=="No"&var_twin=="Yes"&
                          meconium=="No"&macrosomia=="No"&dehydration=="No"&tachyponea=="No"&hiv=="No"&var_hypothermia=="No"&
                          meningitis=="No"&isoimmunisation=="No"&poxic=="No"&caput=="No"&observation=="No"&var_convulsion=="No"&
                          var_necrotising=="No"&var_injury=="No"&var_hypoglycaemia=="No"&var_feed=="No"&var_fever=="No"&var_talipes=="No"&
                          var_down=="No"&var_pneumonia=="No"&var_cleft=="No"&var_congenital=="No"&var_bifida=="No"&var_hydrocephal=="No"&
                          var_anaemia=="No"&other =="No" & nose=="No" & hernia =="No" & gastro =="No" & choriomniotis=="No"))['TRUE']
results_n = cbind(c('LBWT','Premature','Twin','HIV'),c(lbwt_n,immature_n,twin_n,hiv_n))
return (results_n)

}










