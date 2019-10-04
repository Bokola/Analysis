cat('========================\n')
cat('Data recording for reporting ....\n')

#daily register
daily_db_htn_register = daily_db_htn_register %>%
  mutate(., htn_2 = ifelse(htn_2 == 'a', 'new', 'known')) %>%
  mutate(., diagnosis_3 = ifelse(grepl('a,e', diagnosis_3),'Type 1 Diabetes Mellitus,Hypertension', 
                                 ifelse(grepl('b,e', diagnosis_3), 'Type 2 Diabetes Melitus,Hypertension', 
                                        ifelse(grepl('b,f', diagnosis_3), 'Type 2 Diabetes Melitus,Pre-Eclampsia','Gestational Diabetes Mellitus, Hypertension'
                                               #ifelse(grepl('c,e', diagnosis_3), 'Gestational Diabetes Mellitus, Hypertension', 
                                               #ifelse(grepl('e', diagnosis_3),'Hypertension', 'Pre-Eclampsia')
                                        )))) %>%
  mutate(., complications_4 = ifelse(nchar(complications_4)>1, gsub('([a-z])', '\\1,', complications_4), complications_4)) %>%
  mutate(., complications_4 = sub('\\ba\\b', 'Retinopathy', complications_4)) %>%
  mutate(., complications_4 = sub('\\bb\\b', 'Peripheral Neuropathy', complications_4)) %>% # \\b\\b indicates word boundary
  mutate(., complications_4 = sub('\\bc\\b', 'Nephropathy', complications_4)) %>%
  mutate(., complications_4 = sub('\\bd\\b', 'Stroke', complications_4)) %>%
  mutate(., complications_4 = sub('\\be\\b', 'Ischemic heart disease', complications_4)) %>%
  mutate(., complications_4 = sub('\\bf\\b', 'Heart failure', complications_4)) %>%
  mutate(., complications_4 = sub('\\bg\\b', 'Peripheral vascular disease', complications_4)) %>%
  mutate(., complications_4 = sub('\\bh\\b', 'Heart Failure', complications_4)) %>%
  mutate(., complications_4 = sub('\\bi\\b', 'Erectile dysfunction', complications_4)) %>%
  mutate(., complications_4 = sub('\\bj\\b', 'Gastropathy', complications_4)) %>%
  mutate(., complications_4 = sub('\\bk\\b', 'Cataracts', complications_4)) %>%
  mutate(., complications_4 = sub('\\bl\\b', 'Dental complication', complications_4)) %>%
  mutate(., complications_4 = sub('\\bm\\b', 'Dyslipidemia', complications_4)) %>%
  mutate(., complications_4 = sub('\\bn\\b', 'Obesity', complications_4)) %>%
  mutate(., complications_4 = sub('\\bo\\b', 'HIV', complications_4)) %>%
  mutate(., complications_4 = sub('\\bp\\b', 'TB', complications_4)) %>%
  mutate(., complications_4 = gsub(',$', '', complications_4)) # delete last comma 
# complications in long format
complications_data = daily_db_htn_register %>%
  select(date, sub_county, complications_4) %>%
  mutate(., retinopathy_complication = ifelse(grepl('Retinopathy', complications_4), 1, 0)) %>%
  mutate(., `peripheral neuropathy_complication` = ifelse(grepl('Peripheral Neuropathy', complications_4), 1, 0)) %>%
  mutate(., nephropathy_complication = ifelse(grepl('Nephropathy', complications_4), 1, 0)) %>%
  mutate(., stroke_complication = ifelse(grepl('Stroke', complications_4), 1, 0)) %>%
  mutate(., ischemic_complication = ifelse(grepl('Ischemic heart disease', complications_4), 1, 0)) %>%
  mutate(., heart_failure_complication = ifelse(grepl('Heart failure', complications_4), 1, 0)) %>%
  mutate(., erectile_dysfunction_complication = ifelse(grepl('Erectile dysfunction', complications_4), 1, 0)) %>%
  mutate(., gastropathy_complication = ifelse(grepl('Gastropathy', complications_4), 1, 0)) %>%
  mutate(., cataracts_complication = ifelse(grepl('Cataracts', complications_4), 1, 0)) %>%
  mutate(., dental_complication = ifelse(grepl('Dental complication', complications_4), 1, 0)) %>%
  mutate(., dyslipidemia_complication = ifelse(grepl('Dyslipidemia', complications_4), 1, 0)) %>%
  mutate(., obesity_complication = ifelse(grepl('Obesity', complications_4), 1, 0)) %>%
  mutate(., hiv_complication = ifelse(grepl('HIV', complications_4), 1, 0)) %>%
  mutate(., tb_complication = ifelse(grepl('TB', complications_4), 1, 0)) %>%
  pivot_longer(cols = ends_with('_complication'), names_to = 'complications', values_to = 'numbers_complication') %>%
  filter(numbers_complication >0)

complications_data = complications_data %>%
  mutate(., complications = sapply(strsplit(complications, split = '_', fixed = T), function(x) (x[1])))
  
# diagnosis in long format  

diagnosis_data = daily_db_htn_register %>%
  select(date, sub_county, diagnosis_3) %>%
  mutate(., `Type 1 Diabetes Mellitus_diagnosis` = ifelse(grepl('Type 1 Diabetes Mellitus', diagnosis_3), 1, 0)) %>%
  mutate(., Hypertension_diagnosis = ifelse(grepl('Hypertension', diagnosis_3), 1, 0)) %>%
  mutate(., `Type 2 Diabetes Melitus_diagnosis` = ifelse(grepl('Type 2 Diabetes Melitus', diagnosis_3), 1, 0)) %>%
  mutate(., `Pre-Eclampsia_diagnosis` = ifelse(grepl('Pre-Eclampsia', diagnosis_3), 1, 0)) %>%
  mutate(., `Gestational Diabetes Mellitus_diagnosis` = ifelse(grepl('Gestational Diabetes Mellitus', diagnosis_3), 1, 0)) %>%
  pivot_longer(cols = ends_with('_diagnosis'), names_to = 'diagnosis', values_to = 'numbers_diagnosis') %>%
  filter(numbers_diagnosis >0)

diagnosis_data = diagnosis_data %>%
  mutate(., diagnosis = sapply(strsplit(diagnosis, split = '_', fixed = T), function(x) (x[1])))
  

daily_db_htn_register = daily_db_htn_register %>%
  mutate(., treatment_5 = ifelse(nchar(treatment_5) > 1, gsub('([a-z])', '\\1,',treatment_5), treatment_5)) %>%
  mutate(., treatment_5 = sub('\\ba\\b', 'Diet & physical activity', treatment_5)) %>%
  mutate(., treatment_5 = sub('\\bb\\b', 'OGLAs', treatment_5)) %>% # \\b\\b indicates word boundary
  mutate(., treatment_5 = sub('\\bc\\b', 'Insulin', treatment_5)) %>%
  mutate(., treatment_5 = sub('\\bd\\b', 'Anti hypertensive', treatment_5)) %>%
  mutate(., treatment_5 = sub('\\be\\b', 'Herbal', treatment_5)) %>%
  mutate(., treatment_5 = sub('\\bf\\b', 'Other', treatment_5)) %>%
  mutate(., treatment_5 = gsub(',$', '', treatment_5))

daily_db_htn_register = daily_db_htn_register %>%
  mutate(., footcare_outcome_7 = ifelse(footcare_outcome_7 == 'a', 'Ulcer healed', 
                                        ifelse(footcare_outcome_7 == 'd', 'Loss to follow up', 'Empty')))

daily_db_htn_register = daily_db_htn_register %>% 
  mutate(., nhif_y_n_int = ifelse(nhif_y_n == 'Yes', 1, 0) )


# summaries and plots
#1. daily diabates and hypertension counts

daily_summ = daily_db_htn_register %>%
  select(sub_county,ht_cm, wt_kg, bmi_1, blood_pressure_diastolic,blood_pressure_systolic,blood_sugar_fbs, blood_sugar_rbs, nhif_y_n_int, age) %>%
  group_by(sub_county) %>%
  summarise_all(funs(min = min(., na.rm = T), 
                     q25 = quantile(., 0.25, na.rm = T), 
                     #median = median(., na.rm = ), 
                     q75 = quantile(., 0.75, na.rm = T ), 
                     max = max(.,na.rm = T),
                     mean = mean(.,na.rm = T, trim = .2), 
                     sd = sd(.,na.rm = T),
                     count = n())) %>%
  mutate_if(is.numeric, ~round(.,2))
daily_summ = daily_summ[, sort(colnames(daily_summ))] # sort colnames

median = daily_db_htn_register %>%
  select(sub_county,ht_cm, wt_kg, bmi_1, blood_pressure_diastolic,blood_pressure_systolic,blood_sugar_fbs, blood_sugar_rbs, nhif_y_n_int, age) %>%
  group_by(sub_county) %>%
  summarise_all(funs(
                     median = median(., na.rm = ))) %>%
  mutate_if(is.numeric, ~round(.,2))
median = median[, sort(colnames(median))] # sort colnames





library(gridExtra)
# plot_bars <- function(df, catcols){
#   options(repr.plot.width = 6, repr.plot.height = 5)
#   temp0 <- df[df$sub_countyt == 0, ]
#   temp1 <- df[df$bad_credit ==1, ]
#   for (col in cat_cols){
#     p1 <- ggplot(temp0, aes_string(col)) +
#       geom_bar() +
#       #ggtitle(paste('Bar plot of \n', col, '\n for good credit')) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     p2 <- ggplot(temp1, aes_string(col)) +
#       geom_bar() +
#       #ggtitle(paste('Bar plot of \n', col, '\n for bad credi')) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     grid.arrange(p1, p2, nrow = 1)
# 
#   }
# }
# plot_bars(credit, cat_cols)

# diagnosis = daily_db_htn_register %>%
#   select(sub_county, diagnosis_3) %>%
#   group_by(sub_county) %>%
#   summarise(count = count(.)) 

#1 Diagnoses

diagnosis = daily_db_htn_register %>%
  select(sub_county, diagnosis_3) %>%
  ddply(., .(sub_county,diagnosis_3), nrow) %>%
  dplyr::rename(count = V1)





write.csv(diagnosis, file = file.path(data_dir, "daily_diagnosis_3.csv"), row.names = F)
library(highcharter)

# highchart() %>% 
#   hc_chart(type = "column") %>%
#   hc_xAxis(categories = diagnosis$sub_county) %>%
#   hc_add_series(name="Gestational Diabetes Mellitus, Hypertension",data = diagnosis$diagnosis_3) %>%
#   hc_add_series(name="Type 1 Diabetes Mellitus,Hypertension",data = diagnosis$diagnosis_3) %>%
#   hc_add_series(name="Type 2 Diabetes Melitus,Hypertension",data = diagnosis$diagnosis_3) %>%
#   hc_add_series(name="Type 2 Diabetes Melitus,Pre-Eclampsia" ,data = diagnosis$diagnosis_3) %>%
#   hc_add_theme(hc_theme_ft())

library(gridExtra)
#options(repr.plot.width = 5, repr.plot.height = 4) # set plot area dims

#plots =============================================================================

diagnosis_plot_data = diagnosis_data %>%
  ddply(., .(sub_county, diagnosis), summarise,
        n =  sum(numbers_diagnosis))

diag_plot = ggplot(subset(diagnosis_plot_data, sub_county !=''), aes(sub_county, n, label = n, fill = diagnosis)) + 
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9),
            hjust = 0.8) +
  scale_y_continuous(breaks = seq(0, 900, by = 50)) +

# diag_plot = ggplot(diagnosis, aes(x = diagnosis_3, y = count, fill = sub_county)) +
#   geom_bar(stat = "identity") + coord_flip() + 
#   #ggtitle(label = "Bar graph of diagnoses ") +
  xlab("Sub county") +
  ylab("Frequency") +
  scale_fill_discrete(name = "Diagnosis") +
  theme_bw() +
  theme(
    plot.background =  element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    #,axis.text.x = element_text(size = 10)
    ,axis.text.x = element_text(angle = 90, hjust = 1)
  )
# complications - stacked bar

complications_plot_data = complications_data %>%
  #select(sub_county, complications_4) %>%
  ddply(., .(sub_county,complications), summarise,
        n = sum(numbers_complication)) %>%
  subset(., sub_county !='') %>%
  arrange(desc(sub_county)) %>%
  group_by(complications) %>%
  mutate(pos = cumsum(n) - n/2)

complications_plot = ggplot(complications_plot_data) +
geom_bar(aes(x=complications, y=n, fill=sub_county), stat='identity') +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 250, by = 15)) +
  geom_text(aes(x = complications, y = pos
                ,label = prettyNum(n,big.mark = ","))
            , vjust = 0,  size = 2) +
  xlab("complications") +
  ylab("Frequency") +
  scale_fill_discrete(name = "Sub county") +
  theme_bw() +
  theme(
    plot.background =  element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text.x=element_blank()
  )

#table of complications

complications_table = complications_plot_data %>%
  select(., -pos) %>%
  #tidyr::spread(., key = complications, value = n) %>%
  tidyr::pivot_wider(names_from = 'complications', values_from = 'n') %>%
  janitor::adorn_totals(., na.rm = T) %>%
  mutate_if(., is.numeric, ~replace(., is.na(.), 0))
  

  

# treatments
treatments = daily_db_htn_register %>%
  select(sub_county, treatment_5) %>%
  ddply(., .(sub_county,treatment_5), nrow) %>%
  dplyr::rename(count = V1)%>%
  subset(., count > 20 & treatment_5 != "")

#table of treatments 
treatments_table = treatments %>%
  pivot_wider(., names_from = 'treatment_5', values_from = 'count') %>%
  janitor::adorn_totals(., na.rm = T) %>%
  mutate_if(., is.numeric, ~replace(., is.na(.), 0))


treatment_plot = ggplot(treatments, aes(x = treatment_5, y = count, fill = sub_county)) +
  geom_bar(stat = "identity") + coord_flip() + 
  # #ggtitle(label = "Bar graph top treatments ") +
  xlab(" combination treatments") +
  ylab("Count of treatments with  >=20 patients") +
  scale_fill_discrete(name = "Sub County") +
  theme_bw() +
  theme(
    plot.background =  element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  )

#trend lines for monthly data

monthly_db_htn = monthly_db_htn %>%
  mutate(., date = ifelse(! month %in% c('January', 'February', 'March', 'April', 'May', 'June', 'July'), paste('01', month, "2018", sep = ' '), paste('01', month, "2019", sep = ' '))) %>%
  mutate(., date = as.Date(date, '%d %b %Y'))

#using plotly lib
library(plotly)
ay <- list(
  #tickfont = list(color = "red"),
  overlaying = "y",
  side = "right"
)


monthly_trends_plot = plot_ly() %>%
  add_lines(x = monthly_db_htn$date, y = monthly_db_htn$total_dm_pts_in_care, name = 'total dm patients in care', type = 'scatter', mode = 'lines') %>%
  add_lines(x = monthly_db_htn$date, y = monthly_db_htn$total_htn_pts_in_care, name = 'total htn patients in care', type = 'scatter', mode = 'lines') %>%
  add_lines(x = monthly_db_htn$date, y = monthly_db_htn$dm_revisit, name = 'dm revisits', type = 'scatter', mode = 'lines') %>%
  add_lines(x = monthly_db_htn$date, y = monthly_db_htn$dm_first_visit, name = 'dm first visits', type = 'scatter', mode = 'lines') 


# p1 = ggplot(monthly_db_htn,aes(x = date, y = total_dm_pts_in_care)) +
#   # geom_jitter(alpha = 0.5) +
#   # geom_smooth() + 
#   geom_line(color = 'grey') +
#   geom_smooth() +
#   facet_wrap(~subcounty) +
#   #ggtitle(label = "Line plot of total dm patients ") +
#   xlab("Date") +
#   ylab("Total dm patients") +
#   #scale_fill_discrete(name = "Sub County") +
#   theme_bw() +
#   theme(
#     plot.background =  element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#   )
# 
# p2 = ggplot(monthly_db_htn,aes(x = date, y = total_htn_pts_in_care)) +
#   # geom_jitter(alpha = 0.5) +
#   # geom_smooth() + 
#   geom_line(color = 'grey') +
#   geom_smooth() +
#   facet_wrap(~subcounty) +
#   #ggtitle(label = "Line plot of total htn patients ") +
#   xlab("Date") +
#   ylab("Total htn patients") +
#   #scale_fill_discrete(name = "Sub County") +
#   theme_bw() +
#   theme(
#     plot.background =  element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#   )
# 
# p3 = ggplot(monthly_db_htn,aes(x = date, y = dm_revisit)) +
#   # geom_jitter(alpha = 0.5) +
#   # geom_smooth() + 
#   geom_line(color = 'grey') +
#   geom_smooth() +
#   facet_wrap(~subcounty) +
#   #ggtitle(label = "Line plot dm revisits ") +
#   xlab("Date") +
#   ylab("Total dm revisits") +
#   #scale_fill_discrete(name = "Sub County") +
#   theme_bw() +
#   theme(
#     plot.background =  element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#   )
# 
# p4 = ggplot(monthly_db_htn,aes(x = date, y = dm_first_visit)) +
#   # geom_jitter(alpha = 0.5) +
#   # geom_smooth() + 
#   geom_line(color = 'grey') +
#   geom_smooth() +
#   facet_wrap(~subcounty) +
#   #ggtitle(label = "Line plot dm first visit ") +
#   xlab("Date") +
#   ylab("Total dm first visits") +
#   #scale_fill_discrete(name = "Sub County") +
#   theme_bw() +
#   theme(
#     plot.background =  element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#   )

write.csv(median, file = file.path(data_dir, "daily median.csv"), row.names = F)
write.csv(monthly_db_htn, file = file.path(data_dir, "monthly_db_htn_formatted.csv"), row.names = F)
write.csv(daily_db_htn_register, file = file.path(data_dir, "daily_db_htn_register_for.csv"), row.names = F)

cat("Data recoding complete ...!\n\n")
