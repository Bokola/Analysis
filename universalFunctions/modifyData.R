# Correct missing values
relevelPumwani = function(x){
  x = x
  x = mutate(x,  
             gender = ifelse(gender==3, -1, gender),delivery = ifelse(is.na(delivery) & !is.na(matmodel), matmodel, delivery), 
             referred_to_hospital = ifelse(referred_to_hospital==3, -1, referred_to_hospital),
             born_where = ifelse(born_where==3, -1, born_where),
             fever = ifelse(fever==3, -1, fever),
             difficulty_breathing = ifelse(difficulty_breathing==3, -1, difficulty_breathing),
             diarrhoea = ifelse(diarrhoea==3, -1, diarrhoea),
             severe_vomiting = ifelse(severe_vomiting==3, -1, severe_vomiting),
             difficulty_feeding = ifelse(difficulty_feeding==3, -1, difficulty_feeding),
             convulsions = ifelse(convulsions==3, -1, convulsions),
             partial_focal_fits = ifelse(partial_focal_fits==3, -1, partial_focal_fits),
             apnoea = ifelse(apnoea==3, -1, apnoea),
             high_pitched_cry = ifelse(high_pitched_cry==3, -1, high_pitched_cry),
             maternal_hiv = ifelse(maternal_hiv==3, -1, maternal_hiv),
             maternal_arv_s = ifelse(maternal_arv_s==3, -1, maternal_arv_s),
             baby_given_arv_s = ifelse(baby_given_arv_s==3, -1, baby_given_arv_s),
             maternal_vdrl = ifelse(maternal_vdrl==3, -1, maternal_vdrl),
             cap_refill = ifelse(cap_refill==9, -1, cap_refill),
             pallor_anaemia = ifelse(pallor_anaemia==9, -1, pallor_anaemia),
             skin = ifelse(skin==5, -1, skin),
             jaundice = ifelse(jaundice==5, -1, jaundice),
             gest_size = ifelse(gest_size==4, -1, gest_size),
             glucose_test_results_units = ifelse(glucose_test_results_units==3, -1, glucose_test_results_units),
             lp_results = ifelse(lp_results==6, -1, lp_results),
             pen_route = ifelse(pen_route==3, -1, pen_route),
             pen_dose_unit = ifelse(pen_dose_unit==4, -1, pen_dose_unit),
             pen_freq = ifelse(pen_freq==6, -1, pen_freq),
             genta_route = ifelse(genta_route==5, -1, genta_route),
             amp_route = ifelse(amp_route==2, -1, amp_route),
             ceftr_route = ifelse(ceftr_route==3, -1, ceftr_route),
             ceftr_freq = ifelse(ceftr_freq==6, -1, ceftr_freq),
             amikacin_route = ifelse(amikacin_route==3, -1, amikacin_route),
             time_to_start_feeds_after = ifelse(time_to_start_feeds_after==4, -1, time_to_start_feeds_after),
             feeding_route_prescribed = ifelse(feeding_route_prescribed==3, -1, feeding_route_prescribed),
             otcme = ifelse(otcme==5, -1, otcme),
             condition_on_discharge = ifelse(condition_on_discharge==4, -1, condition_on_discharge)            
  )
  return(x)
}

renameNdropPumwani = function(x){
  # Rename variable to be similar to standard neonatal tool
  x = plyr::rename(x, c(survey_id='id', randomized='random', patients_ipno='ipno',
                    date_of_discharge_death ="date_discharge",
                    admission_date='date_adm', agedoc='age_recorded', age='age_days',gender='child_sex', 
                    birth_weight='birth_wt', abnormalities___7='abnormalities____1', pus_from_the_eyes='eye_pus',
                    other_admission_diag_not_listed='other_adm_diag_not_listed',
                    fldte='date_fluid_presc', 
                    fluid_feed_monitoring_char='fluid_feed_monitoring_chart',
                    feed_fluid_monitoring_char='feed_fluid_monitorng_chart',
                    intravenous_fluids_prescri='intravenous_fluids_presc',
                    total_volume_of_other_flui='total_vol_of_other_fluid', 
                    other_feed='other_feeds',
                    number_of_times_temp_monit='no_of_times_temp_monitored',
                    number_of_times_respirator='no_of_times_resp_monitored',
                    number_of_times_pulse_rate='no_of_times_puls_monitored',
                    frequency_of_administratio='freq_of_administration',
                    date_the_feeds_are_initiat='date_the_feeds_initiated',
                    date_of_phototherapy ='start_date_phototherapy',
                    vital_signs_monitored_f48___1='vital_signs_monitored___1',
                    vital_signs_monitored_f48___2='vital_signs_monitored___2',
                    vital_signs_monitored_f48___3='vital_signs_monitored___3',
                    vital_signs_monitored_f48___4='vital_signs_monitored___4',
                    fld_cht='fluid_monitoring_chart',dth_sum='disch_death_summ', otcme='outcome', 
                    condition_on_discharge='dsc_condition', refereed_where='referred_where',follow_up_care='follow_up',
                    ddgnsis='dsc_dx1_primary', any_other_disch_diag = 'other_discharge_diag',
                    other_disch_diag_1='other_discharge_diag_1',other_disch_diag_2='other_discharge_diag_2',
                    other_disch_diag_3='other_discharge_diag_3',other_disch_diag_4='other_discharge_diag_4',
                    other_disch_diag_5='other_discharge_diag_5',other_disch_diag_not_listed='any_other_disch_diag',
                    disch_diag_not_listed=	'other_disch_diag_old',other_discharge_diagnosis=	'other_discharge_diag_unlisted')) %>%
    
    # Remove variables not required
    subset(., select = -c(biodata_complete, babys_history_complete, maternal_history_complete,
                          examination_complete, investigations_complete, babys_admission_diagnoses_complete,
                          babys_daily_case_notes_complete, drug_treatment_complete, supportive_care_complete,
                          follow_up_monitoring_complete, discharge_information_complete,
                          recid, residence_location_sub_loc, residence_district,
                          re_admission_to_this_hospi, bcg_given, opv_birth_given, ind_cs, tindcs,diarrhoea_bloody,
                          depid, matmodel,length_of_illness_days,abdominal_distension,
                          diarrhoea_duration, ismdrugs, maternal_drugs___1, maternal_drugs___2, maternal_drugs___3,
                          maternal_drugs___4, maternal_drugs___5, maternal_drugs___6, maternal_drugs___7, maternal_drugs___8,
                          maternal_drugs___9, maternal_drugs___10, maternal_drugs___11, maternal_drugs___12, maternal_drugs___13,
                          maternal_drugs___14, maternal_drugs___15, maternal_drugs___16, maternal_drugs___17, idrugs,
                          marital_status, mother_s_inpatient_number, anc_visuts_documented, number_of_documented_anc_v,
                          maternal_abo_blood_group, maternal_rhesus_blood_grou, last_normal_menstrual_peri,
                          maternal_cdns___1, maternal_cdns___2, maternal_cdns___3, maternal_cdns___4, maternal_cdns___5, 
                          maternal_cdns___6, maternal_cdns___7, maternal_cdns___8, maternal_cdns___9, othrmatcdn,
                          antibiotics, tetanus_toxoid, duration_of_labour_hrs, induction_augmented, btl_done,
                          baby_femoral_pulse, murmur, skin_cold,stiff_neck,
                          inflmumbilicus,thrush, lymph_nodes_1cm, visible_severe_wasting, floppy_inability_to_suck, 
                          skin_pustules, bp_done, diast_bp, syst_bp, femoral_pulse, severe_indrawing, skin_pinch_sec, 
                          type_of_hb_test, units_for_hb_results, type_of_glucose_test_reque, bilirubin_test_results,
                          chemistry, other_investigations, other_investigations_done, 
                          adm_diag___1, adm_diag___2, adm_diag___3, adm_diag___4, adm_diag___5, adm_diag___6, 
                          adm_diag___7, adm_diag___8, adm_diag___9, adm_diag___10, adm_diag___11, other_diag1, 
                          other_diagnoses, specify_other_diag, dtfrstrevw, 
                          when_was_the_first_documen, sevendyrev, number_of_documented_revie,units, ceftr_units,
                          amikacin_units, teo, date_1_teo_prescribed, teofreq, duration, date_1_teo_stopped,
                          any_other_drug, other_drugs_not_listed, other_treatment_2, other_drugs_not_listed_abo,
                          flow_rate, route_of_admission, date_oxygen_prescribed,cpap_prescriber,
                          cpap_ad_events___1, cpap_ad_events___2, cpap_ad_events___3, cpap_ad_events___4, cpap_ad_events___5, 
                          cpap_ad_events___6, cpap_ad_events___7, cpap_ad_events___8, cpap_ad_events___9, cpap_ad_events____1,
                          cpap_ad_events_other, cpap_outcome, volume_of_blood, duration_of_transfusion, multiple_transfusions,
                          name_of_prescriber_is_legi, signature_of_prescriber_is, incubator_keep_warm, date_incubator_start,
                          date_incubator_stop, inc_temp_hum_chart, wght, wt_frq, fld_mnt, ddate, othr_cndtn_dischg, 
                          oxygen_saturation_monitore,number_of_times_oxygen_sat,
                          no_clear_primary_discharge_diagnosis___1, 
                          no_clear_primary_discharge_diagnosis___2, no_clear_primary_discharge_diagnosis___3, 
                          no_clear_primary_discharge_diagnosis___4, no_clear_primary_discharge_diagnosis___5, 
                          no_clear_primary_discharge_diagnosis___6, no_clear_primary_discharge_diagnosis___7, 
                          no_clear_primary_discharge_diagnosis___8, no_clear_primary_discharge_diagnosis___9, 
                          other_discharge_diagn2_069, disharge_treatment_prescri, discharge_treatment_1, 
                          discharge_treatment_2, discharge_treatment_3, discharge_treatment4, discharge_treatment5, 
                          other_disch_treat_1, other_disch_treatment, othr_drg, othr_disch1))
  return(x)
}



# Wrangle data types
changeTypes = function(x){
# dates
dateVars <- as.character(grep('date', names(x), v = T))
for(i in dateVars){
  x[[i]] = as.Date(x[[i]], "%Y-%m-%d")
}
#weight monitored
weightVars <- as.character(grep('^weigth_', names(x), v = T))
for(i in weightVars){
  x[[i]] = as.numeric(x[[i]])
}

x = mutate(x, id = as.integer(id), hosp_id = as.character(hosp_id), is_minimum = as.integer(is_minimum),
           referred_from_which_facili = as.character(referred_from_which_facili),
           infant_drugs = as.character(infant_drugs),bilirubin_d=as.character(bilirubin_d),
           other_admission_diag_4=as.character(other_admission_diag_4),other_admission_diag_5=as.character(other_admission_diag_5),
           fever_duration = as.numeric(fever_duration),cefta_dur=as.numeric(cefta_dur),
           time_of_admission_document=as.character(time_of_admission_document),infant_drugs=as.character(infant_drugs),
           pen_dur = as.numeric(pen_dur), amp_dose=as.numeric(amp_dose),
           ceftr_dur=as.numeric(ceftr_dur), duration_amikacin = as.integer(duration_amikacin), cefta_dose=as.numeric(cefta_dose),
           total_volume_of_iv_fluids =as.numeric(total_volume_of_iv_fluids),duration_of_iv_fluid_presc=as.numeric(duration_of_iv_fluid_presc),
           duration_prescribed=as.numeric(duration_prescribed),specify_other_fluid_2_pres=as.character(specify_other_fluid_2_pres),
           feed_volume=as.numeric(feed_volume),other_discharge_diag_3=as.character(other_discharge_diag_3),
           other_discharge_diag_4=as.character(other_discharge_diag_4),other_discharge_diag_5=as.character(other_discharge_diag_5),
           other_disch_diag_old=as.character(other_disch_diag_old)
           ) 

return(x)
}


rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}   
             


