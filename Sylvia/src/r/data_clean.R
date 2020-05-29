# ipk = function(pkg){
#   new.pkg = list.pkg[!(list.pkg %in% installed.packages()[, "Package"])]
#   if("practicalgg" %in% list.pkg){
#     remotes::install_github("wilkelab/practicalgg")
#   }
#   if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
#   sapply(pkg, require, character.only = T)
# }
# list.pkg = c('taskscheduleR', 'tidyverse', 'dbplyr', 'DBI', 'RMySQL') 
# #ggforce for geom_arc_bar()
# #cowplot for theme_map()
# ipk(list.pkg)

browseURL("https://stackoverflow.com/questions/32767164/use-gsub-remove-all-string-before-first-white-space-in-r")

summ = function(data){
  
  
  out_all = data %>%
    summarise(`Age` = paste(round(mean(data$age, na.rm = T),2), round(sd(data$age, na.rm = T),2), sep = "\u00B1")) %>%
    cbind(
      `2-5 yrs` = paste0(sum(data$age_cat == '2-5'), '(',round(sum(data$age_cat == '2-5')/nrow(data)*100),")"),
      `6-10 yrs` = paste0(sum(data$age_cat == '6-10'), '(',round(sum(data$age_cat == '6-10')/nrow(data)*100),")")
    ) %>%
    cbind(
      data %>%
        summarise(`Gender, n(%)` = NA_character_)
      
    ) %>%
    cbind(
      `\tMale` = paste0(sum(data$sex==1, na.rm = T),'(',round(sum(data$sex==1, na.rm = T)/ nrow(data) *100),')')
    ) %>%
    cbind(`\tFemale`=
            paste0(sum(data$sex==0, na.rm = T),'(',round(sum(data$sex==0, na.rm = T)/ nrow(data) *100),')') 
    ) %>% cbind(
      `Asthma, n(%)` = paste0(sum(data$asthma_diag == 1,na.rm = T), '(', round(sum(data$asthma_diag == 1,na.rm = T)/nrow(data) *100), ')')
    ) %>%
    cbind(
      `Allergic rhinitis, n(%)` = paste0(sum(data$allergic_rhinitis_diag == 1,na.rm = T), '(', round(sum(data$allergic_rhinitis_diag == 1,na.rm = T)/nrow(data) *100), ')')
    ) %>%
    cbind(
      `BMI(kg/m^2)` = paste(round(mean(data$bmi, na.rm = T),2), round(sd(data$bmi, na.rm = T),2), sep = "\u00B1")
    ) %>%
    cbind(
      `Birth weight category, n(%)` = NA_character_,
      `\t<2.5 kgs` = paste0(sum(data$`birth weight` == "< 2.5 kgs"), '(', round(sum(data$`birth weight` == "< 2.5 kgs")/nrow(data)*100), ')'),
      `\t>=2.5 kgs` = paste0(sum(data$`birth weight` == ">= 2.5 kgs"), '(', round(sum(data$`birth weight` == ">= 2.5 kgs")/nrow(data)*100), ')')
    ) %>%
    cbind(
      `Hospital category, n(%)` = NA_character_,
      `\tPrivate` = paste0(sum(data$hospital =='private'), '(', round(sum(data$hospital == 'private')/nrow(data)*100), ')'),
      `\tPublic` = paste0(sum(data$hospital == 'public'), '(', round(sum(data$hospital == 'public')/nrow(data)*100), ')')
    ) %>%
    cbind(
      `BMI category, n(%)` = NA_character_,
      `\tUnderweight` = paste0(sum(data$bmi_cat == 'Underweightt'), '(', round(sum(data$bmi_cat == 'Underweightt')/nrow(data)*100), ')'),
      `\tNormal` = paste0(sum(data$bmi_cat == 'Normal'), '(', round(sum(data$bmi_cat == 'Normal')/nrow(data)*100), ')'),
      `\tOverweight` = paste0(sum(data$bmi_cat =='Overweight'), '(', round(sum(data$bmi_cat == 'Overweight')/nrow(data)*100), ')'),
      `\tObese` = paste0(sum(data$bmi_cat == 'Obese'), '(', round(sum(data$bmi_cat == 'Obese')/nrow(data)*100), ')')
    ) %>%
    cbind(
      `Height` = paste(round(mean(data$height, na.rm = T),2), round(sd(data$height, na.rm = T),2), sep = "\u00B1")
    ) %>%
    cbind(
      `SDB,n(%)` = paste0(sum(data$psq_binary == 1), '(', round(sum(data$psq_binary == 1)/nrow(data)*100),")")
    ) %>%
    cbind(
      `OSA,n(%)` = paste0(sum(data$osa_binary == 1), '(', round(sum(data$osa_binary == 1)/nrow(data)*100),")")
    )
  
  
  out_all = out_all %>%
    gather(key = "Characteristics", value = "All Participants\n")
  # return(out_all)
  assign(paste('out_', deparse(substitute(data)), sep = ""), out_all, envir = .GlobalEnv)
  
}




yes_no = meta_data %>%
  filter(value_label == 'Yes_No') %>%
  dplyr::select(name) %>%pull()

data = DataXXX

# with SDB at baseline

data_sdb = filter(data, psq_binary == 1)

summ(data_sdb)
names(out_data_sdb) = c("Characteristics", "PSQ > 0.33")

# no SDB at baseline

data_no_sdb = filter(data, psq_binary == 0)
summ(data_no_sdb)
names(out_data_no_sdb) = c("Characteristics", "PSQ < 0.33")

# with OSA at baseline

data_osa = filter(data, psq_binary ==1 & osa_binary== 1)
summ(data_osa)
names(out_data_osa) = c("Characteristics", "POSAT >= 2.72")

# no OSA at baseline

data_no_osa = filter(data, psq_binary == 1 & osa_binary== 0)
summ(data_no_osa)
names(out_data_no_osa) = c("Characteristics", "POSAT < 2.72")

# data[, yes_no]

# with SDB at endline

data_sdb_end = filter(data, psq_b_binary == 1)
summ(data_sdb_end)
names(out_data_sdb_end) = c("Characteristics", "PSQ > 0.33")

# no SDB at endline

data_no_sdb_end = filter(data, psq_b_binary == 0)
summ(data_no_sdb_end)
names(out_data_no_sdb_end) = c("Characteristics", "PSQ < 0.33")

# with OSA at endline

data_osa_end = filter(data, psq_b_binary ==1 & osa_b_binary== 1)
summ(data_osa_end)
names(out_data_osa_end) =c("Characteristics", value = "POSAT >= 2.72")

# no OSA at endline

data_no_osa_end = filter(data, psq_b_binary == 1 & osa_b_binary== 0)
summ(data_no_osa_end)
names(out_data_no_osa_end)= c("Characteristics", value = "POSAT < 2.72")


data_yes = data %>% mutate_at(vars(all_of(yes_no)), ~replace(., is.na(.), 0))

for(c in yes_no){
data_yes[, c] = sapply(data_yes[,c], function(x){
  x = ifelse(x == 0, 'No', 'Yes')
})
}

data_yes[, yes_no]

yes_no_dont = meta_data %>%
  filter(value_label == 'Y_N_DK') %>%
  dplyr::select(name) %>% pull

data_yes = data_yes %>% mutate_at(vars(all_of(yes_no_dont)), ~replace(., is.na(.), 0))
data_yes[, yes_no_dont]

for(c in yes_no_dont){
  data_yes[, c] = sapply(data_yes[,c], function(x){
    x = ifelse(x == 1, 'Yes', x)
    x = ifelse(x == 2, 'No', x)
    x = ifelse(x == 0, 'Dont know', x)
  })
}

# stocks <- data.frame(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# 
# stocksm <- stocks %>% gather(stock, price, -time)
# stocks %>% spread(stock, price)
# stocksm %>% spread(time, price)

apnoea = meta_data %>%
  filter(., value_label == 'APNOEA') %>%
  dplyr::select(name) %>% pull()
table(data_yes[,apnoea])

data_yes = data_yes %>% mutate_at(vars(all_of(apnoea)), ~replace(., is.na(.), 0))

for(c in apnoea){
  if(c == 'snore_loud'| c == 'loud_snore_b'){
    data_yes[, c] = sapply(data_yes[,c], function(x){
      x = ifelse(x == 0, 'mild quiet', x)
      x = ifelse(x == 1, 'medium loud', x)
      x = ifelse(x == 2, 'loud', x)
      x = ifelse(x == 3, 'very loud', x)
      x = ifelse(x == 4, 'extremely loud', x)
    
    })
  }
  else{
  data_yes[, c] = sapply(data_yes[,c], function(x){
    x = ifelse(x == 0, 'never', x)
    x = ifelse(x == 1, 'rarely', x)
    x = ifelse(x == 2, 'occassionaly', x)
    x = ifelse(x == 3, 'frequently', x)
    x = ifelse(x == 4, 'almost always', x)
  
  })
  }
}
  
data_yes_out = data_yes[,c(yes_no, yes_no_dont,apnoea)]
data_yes_out_b = data_yes[,c(yes_no, yes_no_dont,apnoea)] 
data_yes_out_b$psq_binary = data$psq_binary
data_yes_out_b$psq_b_binary = data$psq_b_binary

cat_out <- lapply(data_yes_out, table)
cat_out_b = lapply(filter(data_yes_out_b, psq_binary == 1), table)
cat_out_e = lapply(filter(data_yes_out_b, psq_b_binary == 1), table)

neat.table <- function(cat_out, name){
  xx <- data.frame(cat_out)
  names(xx) <- c("Value", "Count")
  xx$Percent <- with(xx, round(Count/sum(Count)*100))
  data.frame(Variable = name, xx)
}

data = as.data.frame(data)
xxxx = list()

for(x in names(data_yes_out)) {
  p_val_yes = tryCatch(stats::fisher.test(table(data[,"psq_binary"], data[,x]))[["p.value"]], error = function(e) NA) # two angle brackets
  xxxx[[x]] = list(p_val = p_val_yes, var = x, period = "before") # list of lists
}

yyyy = list()

for(x in names(data_yes_out)) {
  p_val_yes = tryCatch(stats::fisher.test(table(data[,"psq_b_binary"], data[,x]))[["p.value"]], error = function(e) NA) # two angle brackets
  yyyy[[x]] = list(p_val = p_val_yes, var = x, period = "after")
}
xx = xxxx %>% dplyr::bind_rows()
yy = yyyy %>% dplyr::bind_rows()

zz = rbind(xx, yy) %>%
  mutate(., p_val = round(p_val,4))

mmm = do.call(rbind, lapply(seq_along(cat_out), function(i)neat.table(cat_out[i], names(cat_out[i]))))
mmm_b = do.call(rbind, lapply(seq_along(cat_out_b), function(i)neat.table(cat_out_b[i], names(cat_out_b[i]))))
mmm_b = mmm_b %>% dplyr::filter(.,!grepl('psq', Variable))

mmm_e = do.call(rbind, lapply(seq_along(cat_out_e), function(i)neat.table(cat_out_e[i], names(cat_out_e[i]))))
mmm_e = mmm_e%>% dplyr::filter(.,!grepl('psq', Variable))

n = nrow(data_yes_out)
mmm_b$ci = paste0(round(mmm_b$Count/n*100,2), "(",round(mmm_b$Count/n + c(-qnorm(0.975))*sqrt((1/n)*mmm_b$Count/n*(1-mmm_b$Count/n)),4)*100, "-", round(mmm_b$Count/n + c(qnorm(0.975))*sqrt((1/n)*mmm_b$Count/n*(1-mmm_b$Count/n)),4)*100,")")
mmm_e$ci = paste0(round(mmm_e$Count/n*100,2), "(",round(mmm_e$Count/n + c(-qnorm(0.975))*sqrt((1/n)*mmm_e$Count/n*(1-mmm_e$Count/n)),4)*100, "-", round(mmm_e$Count/n + c(qnorm(0.975))*sqrt((1/n)*mmm_e$Count/n*(1-mmm_e$Count/n)),4)*100,")")

end = grep("_b$", names(data_yes_out), v = T)
all = names(data_yes_out)
start = subset(all, !all %in% end)

mmm_before = mmm_b 

mmm_after = mmm_e

cols = as.data.frame(unique(mmm$Variable)) %>% `colnames<-`(c("Variable"))
cols = cols %>%
  mutate(., Value = NA_integer_, Count = NA_integer_, Percent = NA_integer_, ci = NA_character_) # for use later

mmm_before = mmm_before %>%
  mutate(., Variable = paste0(Variable, "\t", Value),
         Count = paste0(Count, '(', Percent, ")"))

mmm_after = mmm_after %>%
  mutate(., Variable = paste0(Variable, "\t", Value),
         Count = paste0(Count, '(', Percent, ")"))

label = meta_data %>%
  dplyr::select(c(name, question_caption)) %>%
  filter(., name %in% cols$Variable)

# label_before = label %>%
#   dplyr::filter(., name %in% start)
# label_after = label %>%
#   dplyr::filter(., name %in% end)
cols_b = subset(cols, cols$Variable %in% mmm_b$Variable)
cols_after = subset(cols, cols$Variable %in% mmm_e$Variable)

cat_col_out_b = rbind(mmm_before, cols_b) %>%
  arrange(Variable) # before

cat_col_out_after = rbind(mmm_after, cols_after) %>%
  arrange(Variable) # after


cat_col_out_b = cat_col_out_b %>%
  mutate(., Variable = sub(".+?\t", "", Variable))
cat_col_out_b = cat_col_out_b %>%
  mutate(., Variable = ifelse(Variable %in% c('Yes', 'No', 'Dont know', 'mild quiet', 'medium loud', 'loud', 'very loud', 'extremely loud', 'never', 'rarely', 'occassionaly', 'frequently', 'almost always'), paste0("\t",Variable), Variable))

cat_col_out_b = cat_col_out_b %>%
  mutate(., Variable = ifelse(Variable %in% label$name, label$question_caption, Variable)) 
cat_col_out_b = cat_col_out_b %>%
  dplyr::select(c(Variable, Count,ci)) %>% `colnames<-`(c("Characteristics", "Count\nSDB Baseline", "Mean(CI)\n SDB Baseline")) 

zz_b = zz %>% filter(., period == "before") %>% dplyr::select(., - period) %>% dplyr::rename(Characteristics = var) %>%
  mutate(., Characteristics = ifelse(Characteristics %in% label$name, label$question_caption, Characteristics))
cat_col_out_b = left_join(cat_col_out_b, zz_b) %>%
  mutate_all(., ~replace(., is.na(.), ""))


# after

cat_col_out_after = cat_col_out_after %>%
  mutate(., Variable = sub(".+?\t", "", Variable))
cat_col_out_after = cat_col_out_after %>%
  mutate(., Variable = ifelse(Variable %in% c('Yes', 'No', 'Dont know', 'mild quiet', 'medium loud', 'loud', 'very loud', 'extremely loud', 'never', 'rarely', 'occassionaly', 'frequently', 'almost always'), paste0("\t",Variable), Variable))

cat_col_out_after = cat_col_out_after %>%
  mutate(., Variable = ifelse(Variable %in% label$name, label$question_caption, Variable)) 
cat_col_out_after = cat_col_out_after %>%
  dplyr::select(c(Variable, Count,ci)) %>% `colnames<-`(c("Characteristics", "Count\nSDB endline", "Mean(CI)\n SDB endline")) 

zz_e = zz %>% filter(., period == "after") %>% dplyr::select(., - period) %>% dplyr::rename(Characteristics = var) %>%
  mutate(., Characteristics = ifelse(Characteristics %in% label$name, label$question_caption, Characteristics))
cat_col_out_after = left_join(cat_col_out_after, zz_e) %>%
  mutate_all(., ~replace(., is.na(.), ""))


summ(data)

out_data_b = out_data %>% `colnames<-`(c("Characteristics", "Before"))
out_data_after= out_data %>% `colnames<-`(c("Characteristics", "After"))

# cat_col_out_b = rbind(out_data_b, cat_col_out_b)
# cat_col_out_after= rbind(out_data_after, cat_col_out_after)

cat_col_out_b =  cat_col_out_b %>% mutate_all(., ~replace(., is.na(.), ""))
cat_col_out_after=  cat_col_out_after %>% mutate_all(., ~replace(., is.na(.), ""))

# cat_col_out = cat_col_out_b %>% cbind(., cat_col_out_after)
  
  

tab_1 = cat_col_out_b %>%
  mutate_if(is.character, ~replace(.,is.na(.), ''))
  
tab_2 = cat_col_out_after %>%
  mutate_if(is.character, ~replace(.,is.na(.), ''))

test_stat_bmi_hosp = function(data, data_1){
  
  sex = left_join(
    data_sdb %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `psq` = length(sub_id)),
    data_no_sdb %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `no psq` = length(sub_id))  
    
  ) 
  
  sexx = gather(sex, key, value, -sex)
  chi_sex =  stats::chisq.test( xtabs(value ~ key + sex, data = sexx))
  f_sex = stats::fisher.test(xtabs(value ~ key + sex, data = sexx))
  # f_sex$conf.int[1] # lower
  # f_sex$conf.int[2] #upper
  
  age = cbind(
    data %>%
      summarise(psq = mean(age, na.rm= T)),
    data_1 %>%
      summarise(`no psq` = mean(age, na.rm = T))
  )
  
  age_w = wilcox.test(data$age, data_1$age, conf.int = TRUE)
  age_t = t.test(data$age, data_1$age)
  
  #asthma
  
  asthma = left_join(
    data %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - asthma_diag) #%>%
  
  chi_asthma = chisq.test(xtabs(value ~ key + asthma_diag, data = asthma))
  f_asthma = stats::fisher.test(xtabs(value ~ key + asthma_diag, data = asthma))
  
  #allergic rhinitis
  
  rhinitis = left_join(
    data %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - allergic_rhinitis_diag) #%>%
  
  chi_rhinitis = chisq.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  f_rhinitis = stats::fisher.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  
  #BMI
  
  bmi_t = t.test(data$bmi, data_1$bmi)
  bmi_w= wilcox.test(data$bmi, data_1$bmi, conf.int = TRUE)
  
  # BMI category
  
  # bmi_cat = left_join(
  #   data %>%
  #     mutate(bmi_cat = as.factor(bmi_cat)) %>%
  #     ddply(.(bmi_cat), summarise,
  #           `psq` = length(sub_id)),
  #   data_1 %>%
  #     mutate(bmi_cat = as.factor(bmi_cat)) %>%
  #     ddply(.(bmi_cat), summarise,
  #           `no psq` = length(sub_id))  
  #   
  # ) %>%
  #   gather(.,key, value, - bmi_cat) #%>%
  # 
  # chi_bmi_cat = chisq.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  # f_bmi_cat = stats::fisher.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  
  # Birth weight category
  
  birth_weight = left_join(
    data %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - `birth weight`) %>%
    mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  
  chi_birth_weight = chisq.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  f_birth_weight = stats::fisher.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  
  # Hospital category
  
  # hospital = left_join(
  #   data %>%
  #     mutate(hospital = as.factor(hospital)) %>%
  #     ddply(.(hospital), summarise,
  #           `psq` = length(sub_id)),
  #   data_1 %>%
  #     mutate(hospital = as.factor(hospital)) %>%
  #     ddply(.(hospital), summarise,
  #           `no psq` = length(sub_id))  
  #   
  # ) %>%
  #   gather(.,key, value, - hospital) %>%
  #   mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  # 
  # chi_hospital = chisq.test(xtabs(value ~ key + hospital, data = hospital))
  # f_hospital = stats::fisher.test(xtabs(value ~ key + hospital, data = hospital))
  # 
  
  # height
  
  height_t = t.test(data$height, data_1$height)
  height_w = wilcox.test(data$height, data_1$height, conf.int = T)
  
  #osa
  
  osa = left_join(
    data %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - osa_binary) #%>%
  
  chi_osa = chisq.test(xtabs(value ~ key + osa_binary, data = osa))
  f_osa = stats::fisher.test(xtabs(value ~ key + osa_binary, data = osa))
  
  
  p_val_baseline = cbind(
    `Gender, n(%)` = round(f_sex$p.value,3),
    `Age` = round(age_w$p.value,3),
    `Asthma, n(%)` = round(f_asthma$p.value,3),
    `Allergic rhinitis, n(%)` = round(chi_rhinitis$p.value,3),
    # `BMI category, n(%)` = round(f_bmi_cat$p.value,3),
    `Birth weight category, n(%)` = round(f_birth_weight$p.value,3),
    # `Hospital category, n(%)` = round(f_hospital$p.value,3),
    `OSA,n(%)` = round(f_osa$p.value,3)
  )
  
  p_val_baseline = as_tibble(p_val_baseline) %>%
    mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "p-value")
  
  conf_baseline = cbind(
    `Gender, n(%)` = paste(round(as.numeric(f_sex$conf.int[1]),4),round(as.numeric(f_sex$conf.int[2]),4), sep = ","),
    `Age` = paste(round(as.numeric(age_w$conf.int[1]),4),round(as.numeric(age_w$conf.int[2]),4), sep = ","),
    `Asthma, n(%)` = paste(round(as.numeric(f_asthma$conf.int[1]),4),round(as.numeric(f_asthma$conf.int[2]),4), sep = ","), #f_asthma
    `Allergic rhinitis, n(%)` = paste(round(as.numeric(f_rhinitis$conf.int[1]),4),round(as.numeric(f_rhinitis$conf.int[2]),4), sep = ","), #f_rhinitis
    # `BMI category, n(%)` = paste(round(as.numeric(f_bmi_cat$conf.int[1]),4),round(as.numeric(f_bmi_cat$conf.int[2]),4), sep = ","), #f_bmi_cat
    `Birth weight category, n(%)` = paste(round(as.numeric(f_birth_weight$conf.int[1]),4),round(as.numeric(f_birth_weight$conf.int[2]),4), sep = ","), #f_birth_weight
    # `Hospital category, n(%)` = paste(round(as.numeric(f_hospital$conf.int[1]),4),round(as.numeric(f_hospital$conf.int[2])), sep = ","), #f_hospital
    `OSA,n(%)` = paste(round(as.numeric(f_osa$conf.int[1]),4),round(as.numeric(f_osa$conf.int[2]),4), sep = ",")
  )
  
  conf_baseline = as_tibble(conf_baseline) %>%
    # mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "CI")
  assign(paste("conf_", deparse(substitute(data)), sep = ""), conf_baseline, envir = .GlobalEnv)
  assign(paste("p_val_", deparse(substitute(data)), sep = ""), p_val_baseline, envir = .GlobalEnv)
}



test_stat = function(data, data_1){
  
  sex = left_join(
    data_sdb %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `psq` = length(sub_id)),
    data_no_sdb %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `no psq` = length(sub_id))  
    
  ) 
  
  sexx = gather(sex, key, value, -sex)
  chi_sex =  stats::chisq.test( xtabs(value ~ key + sex, data = sexx))
  f_sex = stats::fisher.test(xtabs(value ~ key + sex, data = sexx))
  # f_sex$conf.int[1] # lower
  # f_sex$conf.int[2] #upper
  
  age = cbind(
    data %>%
      summarise(psq = mean(age, na.rm= T)),
    data_1 %>%
      summarise(`no psq` = mean(age, na.rm = T))
  )
  
  age_w = wilcox.test(data$age, data_1$age, conf.int = TRUE)
  age_t = t.test(data$age, data_1$age)
  
  #asthma
  
  asthma = left_join(
    data %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - asthma_diag) #%>%
  
  chi_asthma = chisq.test(xtabs(value ~ key + asthma_diag, data = asthma))
  f_asthma = stats::fisher.test(xtabs(value ~ key + asthma_diag, data = asthma))
  
  #allergic rhinitis
  
  rhinitis = left_join(
    data %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - allergic_rhinitis_diag) #%>%
  
  chi_rhinitis = chisq.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  f_rhinitis = stats::fisher.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  
  #BMI
  
  bmi_t = t.test(data$bmi, data_1$bmi)
  bmi_w= wilcox.test(data$bmi, data_1$bmi, conf.int = TRUE)
  
  # BMI category
  
  bmi_cat = left_join(
    data %>%
      mutate(bmi_cat = as.factor(bmi_cat)) %>%
      ddply(.(bmi_cat), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(bmi_cat = as.factor(bmi_cat)) %>%
      ddply(.(bmi_cat), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - bmi_cat) #%>%
  
  chi_bmi_cat = chisq.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  f_bmi_cat = stats::fisher.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  
  # Birth weight category
  
  birth_weight = left_join(
    data %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - `birth weight`) %>%
    mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  
  chi_birth_weight = chisq.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  f_birth_weight = stats::fisher.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  
  # Hospital category
  
  hospital = left_join(
    data %>%
      mutate(hospital = as.factor(hospital)) %>%
      ddply(.(hospital), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(hospital = as.factor(hospital)) %>%
      ddply(.(hospital), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - hospital) %>%
    mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  
  chi_hospital = chisq.test(xtabs(value ~ key + hospital, data = hospital))
  f_hospital = stats::fisher.test(xtabs(value ~ key + hospital, data = hospital))
  
  
  # height
  
  height_t = t.test(data$height, data_1$height)
  height_w = wilcox.test(data$height, data_1$height, conf.int = T)
  
  #osa
  
  osa = left_join(
    data %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - osa_binary) #%>%
  
  chi_osa = chisq.test(xtabs(value ~ key + osa_binary, data = osa))
  f_osa = stats::fisher.test(xtabs(value ~ key + osa_binary, data = osa))
  
  
  p_val_baseline = cbind(
    `Gender, n(%)` = round(f_sex$p.value,3),
    `Age` = round(age_w$p.value,3),
    `Asthma, n(%)` = round(f_asthma$p.value,3),
    `Allergic rhinitis, n(%)` = round(chi_rhinitis$p.value,3),
    `BMI category, n(%)` = round(f_bmi_cat$p.value,3),
    `Birth weight category, n(%)` = round(f_birth_weight$p.value,3),
    `Hospital category, n(%)` = round(f_hospital$p.value,3),
    `OSA,n(%)` = round(f_osa$p.value,3)
  )
  
  p_val_baseline = as_tibble(p_val_baseline) %>%
    mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "p-value")
  
  conf_baseline = cbind(
    `Gender, n(%)` = paste(round(as.numeric(f_sex$conf.int[1]),4),round(as.numeric(f_sex$conf.int[2]),4), sep = ","),
    `Age` = paste(round(as.numeric(age_w$conf.int[1]),4),round(as.numeric(age_w$conf.int[2]),4), sep = ","),
    `Asthma, n(%)` = paste(round(as.numeric(f_asthma$conf.int[1]),4),round(as.numeric(f_asthma$conf.int[2]),4), sep = ","), #f_asthma
    `Allergic rhinitis, n(%)` = paste(round(as.numeric(f_rhinitis$conf.int[1]),4),round(as.numeric(f_rhinitis$conf.int[2]),4), sep = ","), #f_rhinitis
    `BMI category, n(%)` = paste(round(as.numeric(f_bmi_cat$conf.int[1]),4),round(as.numeric(f_bmi_cat$conf.int[2]),4), sep = ","), #f_bmi_cat
    `Birth weight category, n(%)` = paste(round(as.numeric(f_birth_weight$conf.int[1]),4),round(as.numeric(f_birth_weight$conf.int[2]),4), sep = ","), #f_birth_weight
    `Hospital category, n(%)` = paste(round(as.numeric(f_hospital$conf.int[1]),4),round(as.numeric(f_hospital$conf.int[2])), sep = ","), #f_hospital
    `OSA,n(%)` = paste(round(as.numeric(f_osa$conf.int[1]),4),round(as.numeric(f_osa$conf.int[2]),4), sep = ",")
  )
  
  conf_baseline = as_tibble(conf_baseline) %>%
    # mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "CI")
  assign(paste("conf_", deparse(substitute(data)), sep = ""), conf_baseline, envir = .GlobalEnv)
  assign(paste("p_val_", deparse(substitute(data)), sep = ""), p_val_baseline, envir = .GlobalEnv)
}

prop_baseline = function(data){
  
  # data = data_sdb
  
  out_prop = cbind(
    `Overall` = paste0(round(sum(data$psq_binary == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1, na.rm = T)/nrow(data))),4)*100,")")
  )%>%
    cbind(
      Age = NA_character_,
      `\t2-5 yrs` = paste0(round(sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data))),4)*100,")"),
      `\t6-10 yrs` =paste0(round(sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(
      data %>%
        summarise(`Gender` = NA_character_)
      
    ) %>%
    cbind(
      `\tMale` = paste0(round(sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$sex==1, na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(`\tFemale`=
            paste0(round(sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$sex==0, na.rm = T)/nrow(data))),4)*100,")") 
    )%>% cbind(
      `Asthma` = NA_character_,
      `\tAsthma Yes` =  paste0(round(sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tAsthma No` = paste0(round(sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(
      `Allergic rhinitis` = NA_character_,
      `\tYes` = paste0(round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data))),4)*100,")"),
      `\tNo` = paste0(round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data))),4)*100,")")
      
      
    ) %>%
    cbind(
      `Birth weight category` = NA_character_,
      `\t<2.5 kgs` = paste0(round(sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data))),4)*100,")"),
      
      `\t>=2.5 kgs` = paste0(round(sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `Hospital category` = NA_character_,
      `\tPrivate` =  paste0(round(sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tPublic` = paste0(round(sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `BMI category`= NA_character_,
      `\tUnderweight` = paste0(round(sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tNormal` = paste0(round(sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tOverweight` = paste0(round(sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tObese` = paste0(round(sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `OSA` = NA_character_,
      
      `\tOSA Yes` = paste0(round(sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$osa_binary == 1, na.rm = T)/nrow(data))),4)*100,")"),
      `\tOSA No` = paste0(round(sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$osa_binary == 0, na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(
      `wet bed`= NA_character_,
      `\twet Yes` = paste0(round(sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 1, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\twet No` = paste0(round(sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 2, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\twet dont know` = paste0(round(sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data)*(1-sum(data$psq_binary == 1 & data$wet_bed== 0, na.rm = T)/nrow(data))),4)*100,")")
    )
  
  
  out_prop = out_prop %>%
    gather(key = "Characteristics", value = "SDB\nBaseline")
  # return(out_all)
  assign('out_prop_b',  out_prop, envir = .GlobalEnv)
  
}

prop_baseline(data)

prop_endline = function(data){
  
  # data = data_sdb
  
  out_prop = cbind(
    `Overall` = paste0(round(sum(data$psq_b_binary == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1, na.rm = T)/nrow(data))),4)*100,")")
  )%>%
    cbind(
      Age = NA_character_,
      `\t2-5 yrs` = paste0(round(sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$age_cat == '2-5', na.rm = T)/nrow(data))),4)*100,")"),
      `\t6-10 yrs` =paste0(round(sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$age_cat == '6-10', na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(
      data %>%
        summarise(`Gender` = NA_character_)
      
    ) %>%
    cbind(
      `\tMale` = paste0(round(sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$sex==1, na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(`\tFemale`=
            paste0(round(sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$sex==0, na.rm = T)/nrow(data))),4)*100,")") 
    )%>% cbind(
      `Asthma` = NA_character_,
      `\tAsthma Yes` =  paste0(round(sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$asthma_diag == 1, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tAsthma No` = paste0(round(sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$asthma_diag == 0, na.rm = T)/nrow(data))),4)*100,")")
      
    ) %>%
    cbind(
      `Allergic rhinitis` = NA_character_,
      `\tYes` = paste0(round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 1, na.rm = T)/nrow(data))),4)*100,")"),
      `\tNo` = paste0(round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$allergic_rhinitis_diag == 0, na.rm = T)/nrow(data))),4)*100,")")
      
      
    ) %>%
    cbind(
      `Birth weight category` = NA_character_,
      `\t<2.5 kgs` = paste0(round(sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$`birth weight` == "< 2.5 kgs", na.rm = T)/nrow(data))),4)*100,")"),
      
      `\t>=2.5 kgs` = paste0(round(sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$`birth weight` == ">= 2.5 kgs", na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `Hospital category` = NA_character_,
      `\tPrivate` =  paste0(round(sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$hospital =='private', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tPublic` = paste0(round(sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$hospital == 'public', na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `BMI category`= NA_character_,
      `\tUnderweight` = paste0(round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Underweightt', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tNormal` = paste0(round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Normal', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tOverweight` = paste0(round(sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat =='Overweight', na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tObese` = paste0(round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$bmi_cat == 'Obese', na.rm = T)/nrow(data))),4)*100,")")
    ) %>%
    cbind(
      `OSA` = NA_character_,
      
      `\tOSA Yes` = paste0(round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 1, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\tOSA No` = paste0(round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$data$osa_b_binary == 0, na.rm = T)/nrow(data))),4)*100,")")
      
    )%>%
    cbind(
      `wet bed`= NA_character_,
      `\twet Yes` = paste0(round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 1, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\twet No` = paste0(round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 2, na.rm = T)/nrow(data))),4)*100,")"),
      
      `\twet dont know` = paste0(round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data)*100,2), "(",round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data))),4)*100, "-", round(sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data) + c(qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data)*(1-sum(data$psq_b_binary == 1 & data$wet_bed_b == 0, na.rm = T)/nrow(data))),4)*100,")")
    )
  
  
  out_prop = out_prop %>%
    gather(key = "Characteristics", value = "SDB\nendline")
  # return(out_all)
  assign('out_prop_e',  out_prop, envir = .GlobalEnv)
  
}
prop_endline(data)
prop_out = left_join(out_prop_b, out_prop_e)

# p-val

test_sdb = function(data, data_1){
  sex = left_join(
    data %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(sex = factor(sex,  labels = c("female","male"))) %>%
      ddply(.(sex), summarise,
            `no psq` = length(sub_id))  
    
  ) 
  
  sexx = gather(sex, key, value, -sex)
  chi_sex =  stats::chisq.test( xtabs(value ~ key + sex, data = sexx))
  f_sex = stats::fisher.test(xtabs(value ~ key + sex, data = sexx))
  # f_sex$conf.int[1] # lower
  # f_sex$conf.int[2] #upper
  
  age = cbind(
    data %>%
      summarise(psq = mean(age, na.rm= T)),
    data_1 %>%
      summarise(`no psq` = mean(age, na.rm = T))
  )
  
  age_w = wilcox.test(data$age, data_1$age, conf.int = TRUE)
  age_t = t.test(data$age, data_1$age)
  
  #asthma
  
  asthma = left_join(
    data %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(asthma_diag = factor(asthma_diag,  labels = c("no","yes"))) %>%
      ddply(.(asthma_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - asthma_diag) #%>%
  
  chi_asthma = chisq.test(xtabs(value ~ key + asthma_diag, data = asthma))
  f_asthma = stats::fisher.test(xtabs(value ~ key + asthma_diag, data = asthma))
  
  #allergic rhinitis
  
  rhinitis = left_join(
    data %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(allergic_rhinitis_diag = factor(allergic_rhinitis_diag,  labels = c("no","yes"))) %>%
      ddply(.(allergic_rhinitis_diag), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - allergic_rhinitis_diag) #%>%
  
  chi_rhinitis = chisq.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  f_rhinitis = stats::fisher.test(xtabs(value ~ key + allergic_rhinitis_diag, data = rhinitis))
  
  #BMI
  
  # bmi_t = t.test(data$bmi, data_1$bmi)
  # bmi_w= wilcox.test(data$bmi, data_1$bmi, conf.int = TRUE)
  
  # BMI category
  
  bmi_cat = left_join(
    data %>%
      mutate(bmi_cat = as.factor(bmi_cat)) %>%
      ddply(.(bmi_cat), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(bmi_cat = as.factor(bmi_cat)) %>%
      ddply(.(bmi_cat), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - bmi_cat) #%>%
  
  chi_bmi_cat = chisq.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  f_bmi_cat = stats::fisher.test(xtabs(value ~ key + bmi_cat, data = bmi_cat))
  
  # Birth weight category
  
  birth_weight = left_join(
    data %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(`birth weight` = as.factor(`birth weight`)) %>%
      ddply(.(`birth weight`), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - `birth weight`) %>%
    mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  
  chi_birth_weight = chisq.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  f_birth_weight = stats::fisher.test(xtabs(value ~ key + `birth weight`, data = birth_weight))
  
  # Hospital category
  
  hospital = left_join(
    data %>%
      mutate(hospital = as.factor(hospital)) %>%
      ddply(.(hospital), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(hospital = as.factor(hospital)) %>%
      ddply(.(hospital), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - hospital) %>%
    mutate_if(is.numeric, ~replace(.,is.na(.), 0))
  
  chi_hospital = chisq.test(xtabs(value ~ key + hospital, data = hospital))
  f_hospital = stats::fisher.test(xtabs(value ~ key + hospital, data = hospital))
  
  
  # height
  
  height_t = t.test(data$height, data_1$height)
  height_w = wilcox.test(data$height, data_1$height, conf.int = T)
  
  #osa
  
  osa = left_join(
    data %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(osa_binary = factor(osa_binary,  labels = c("no","yes"))) %>%
      ddply(.(osa_binary), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - osa_binary) #%>%
  
  chi_osa = chisq.test(xtabs(value ~ key + osa_binary, data = osa))
  f_osa = stats::fisher.test(xtabs(value ~ key + osa_binary, data = osa))
  
  #wet bed
  wet_cat = left_join(
    data %>%
      mutate(wet_bed = as.factor(wet_bed)) %>%
      ddply(.(wet_bed), summarise,
            `psq` = length(sub_id)),
    data_1 %>%
      mutate(wet_bed = as.factor(wet_bed)) %>%
      ddply(.(wet_bed), summarise,
            `no psq` = length(sub_id))  
    
  ) %>%
    gather(.,key, value, - wet_bed) #%>%
  
  chi_wet_cat = chisq.test(xtabs(value ~ key + wet_bed, data = wet_cat))
  f_wet_cat = stats::fisher.test(xtabs(value ~ key + wet_bed, data = wet_cat))
  
  p_val_sdb = cbind(
    `Gender` = round(f_sex$p.value,3),
    `Age` = round(age_w$p.value,3),
    `Asthma` = round(f_asthma$p.value,3),
    `Allergic rhinitis` = round(chi_rhinitis$p.value,3),
    `BMI category` = round(f_bmi_cat$p.value,3),
    `Birth weight category` = round(f_birth_weight$p.value,3),
    `Hospital category` = round(f_hospital$p.value,3),
    `OSA` = round(f_osa$p.value,3),
    `wet bed` = round(f_wet_cat$p.value,3)
  )
  
  p_val_sdb = as_tibble(p_val_sdb) %>%
    mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "p-value")
  
  conf_sdb = cbind(
    `Gender` = paste(round(as.numeric(f_sex$conf.int[1]),4),round(as.numeric(f_sex$conf.int[2]),4), sep = ","),
    `Age` = paste(round(as.numeric(age_w$conf.int[1]),4),round(as.numeric(age_w$conf.int[2]),4), sep = ","),
    `Asthma` = paste(round(as.numeric(f_asthma$conf.int[1]),4),round(as.numeric(f_asthma$conf.int[2]),4), sep = ","), #f_asthma
    `Allergic rhinitis` = paste(round(as.numeric(f_rhinitis$conf.int[1]),4),round(as.numeric(f_rhinitis$conf.int[2]),4), sep = ","), #f_rhinitis
    `BMI category` = paste(round(as.numeric(f_bmi_cat$conf.int[1]),4),round(as.numeric(f_bmi_cat$conf.int[2]),4), sep = ","), #f_bmi_cat
    `Birth weight category` = paste(round(as.numeric(f_birth_weight$conf.int[1]),4),round(as.numeric(f_birth_weight$conf.int[2]),4), sep = ","), #f_birth_weight
    `Hospital category` = paste(round(as.numeric(f_hospital$conf.int[1]),4),round(as.numeric(f_hospital$conf.int[2])), sep = ","), #f_hospital
    `OSA` = paste(round(as.numeric(f_osa$conf.int[1]),4),round(as.numeric(f_osa$conf.int[2]),4), sep = ","),
    `wet bed` = paste(round(as.numeric(f_wet_cat$conf.int[1]),4),round(as.numeric(f_wet_cat$conf.int[2]),4), sep = ",")
  )
  
  conf_sdb = as_tibble(conf_sdb) %>%
    # mutate_all(., as.numeric) %>%
    gather(., key = "Characteristics", value = "CI")
  assign("conf_sdb", conf_sdb, envir = .GlobalEnv)
  assign("p_val_sdb",  p_val_sdb, envir = .GlobalEnv)
}
test_sdb(data_sdb, data_sdb_end)

prop_out = prop_out %>%
  left_join(., p_val_sdb)  %>%
  mutate_all(.,~replace(., is.na(.), ""))



