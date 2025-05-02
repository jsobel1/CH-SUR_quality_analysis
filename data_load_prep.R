#########################
#       Data load       #
#########################
#loads hospdat_new_vars => numeric data
datafilename <- paste0("CH_SUR_",
                       disease_nm,
                       "_",
                       gsub("-","",as.character(exportdate)),
                       "_CLEANED_ON_",
                       gsub("-","",as.character(cleandate)),
                       "_nofactor.RData")
load(datafilename)

#loads hospdat_recoded => Factor data
datafilename <- paste0("CH_SUR_",
                       disease_nm,
                       "_",
                       gsub("-","",as.character(exportdate)),
                       "_CLEANED_ON_",
                       gsub("-","",as.character(cleandate)),
                       ".RData")
load(datafilename)

#########################
#    Functions of use   #
#########################
### Deduplicates but keep last ###
dedup_last <- . %>%
  group_by(record_id) %>%
  filter(redcap_repeat_instance == max(redcap_repeat_instance)) %>%
  ungroup()

### Deduplicates but keep first ###
dedup_first <- . %>%
  group_by(record_id) %>%
  filter(redcap_repeat_instance == min(redcap_repeat_instance)) %>%
  ungroup()

### keep data from one centre ###
filter_dags <- . %>%
  filter(dags_label %in% centre)

#########################
#        New vars       #
#########################
#### COVID-19 waves and Influenza seasons ####
inf_season <- c("2018/19 season", "2019/20 season", "2020/21 season", "2021/22 season","2022/23 season") 
wave <- c("Phase 1 (02/2020-07/06/2020)", "Phase 2 (08/06/2020-27/09/2020)", "Phase 2b (28/09/2020-14/02/2021)", 
          "Phase 3 (15/02/2021-20/06/2021)", "Phase 4 (21/06/2021-10/10/2021)", "Phase 5 (11/10/2021-19/12/2021)",
          "Phase 6 (20/12/2021-31/03/2022)", "Normal Situation (from 01/04/2022)")

wave_season_recode <- . %>%
  mutate(COVID_wave = case_when(
    ((corr_hospital_entry_date >= as.Date("2020-02-24")) & (corr_hospital_entry_date <= as.Date("2020-06-07"))) ~ wave[1],
    ((corr_hospital_entry_date >= as.Date("2020-06-08")) & (corr_hospital_entry_date <= as.Date("2020-09-27"))) ~ wave[2],
    ((corr_hospital_entry_date >= as.Date("2020-09-28")) & (corr_hospital_entry_date <= as.Date("2021-02-14"))) ~ wave[3],
    ((corr_hospital_entry_date >= as.Date("2021-02-15")) & (corr_hospital_entry_date <= as.Date("2021-06-20"))) ~ wave[4],
    ((corr_hospital_entry_date >= as.Date("2021-06-21")) & (corr_hospital_entry_date <= as.Date("2021-10-10"))) ~ wave[5],
    ((corr_hospital_entry_date >= as.Date("2021-10-11")) & (corr_hospital_entry_date <= as.Date("2021-12-19"))) ~ wave[6],
    ((corr_hospital_entry_date >= as.Date("2021-12-20")) & (corr_hospital_entry_date <= as.Date("2022-03-31"))) ~ wave[7],
    ((corr_hospital_entry_date >= as.Date("2022-04-01")) & (corr_hospital_entry_date <= as.Date(today()))) ~ wave[8],
    TRUE ~ NA_character_
  )) %>%
  mutate(COVID_wave = factor(COVID_wave, ordered = TRUE, levels = wave)) %>%
  mutate(Flu_season = case_when(
    ((corr_hospital_entry_date >= as.Date("2018-11-01")) & (corr_hospital_entry_date <= as.Date("2019-04-30"))) ~ inf_season[1],
    ((corr_hospital_entry_date >= as.Date("2019-11-01")) & (corr_hospital_entry_date <= as.Date("2020-04-30"))) ~ inf_season[2],
    ((corr_hospital_entry_date >= as.Date("2020-11-01")) & (corr_hospital_entry_date <= as.Date("2021-04-30"))) ~ inf_season[3],
    ((corr_hospital_entry_date >= as.Date("2021-11-01")) & (corr_hospital_entry_date <= as.Date("2022-05-31"))) ~ inf_season[4],
    ((corr_hospital_entry_date >= as.Date("2022-11-01")) & (corr_hospital_entry_date <= as.Date("2023-05-31"))) ~ inf_season[5],
    TRUE ~ NA_character_
  )) %>%
  mutate(Flu_season = factor(Flu_season, ordered = TRUE, levels = inf_season)) %>%
  mutate(inclusion_date = if_else((redcap_repeat_instance %in% 1) & (inclusion_date < corr_hospital_entry_date), 
                                  corr_hospital_entry_date, 
                                  as.Date(inclusion_date)))

hospdat_recoded <-  wave_season_recode(hospdat_recoded)
hospdat_new_vars <-  wave_season_recode(hospdat_new_vars)

### Age and age group ###
# age_label <- c('<1 month', '1 month-<1 year','1-5','6-12','13-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99', '>100')
# age_recoded <- . %>% 
#   subset(!is.na(birth_year)) %>%
#   #mutate(full_birth = as.Date(paste(birth_year, replace_na(toddler_birthmonth, 1), 1, sep = "-"), format = "%Y-%m-%d")) %>%
#   mutate(age_y = -time_length(interval(inclusion_date, ymd(paste(birth_year,"-01-01",sep="",collapse=""))), "years")) %>%
#   mutate(age_group_detail = cut(age_y, c(0, 1.0/12, 1, 6, 13, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500), 
#                                 age_label, include.lowest=TRUE)) %>%
#   mutate(age_group_detail = factor(age_group_detail, ordered = TRUE, levels = age_label)) %>%
#   mutate(age_group_for_covid_vacc = case_when(
#     is.na(age_years) ~ NA_character_,
#     age_years < 0 ~ NA_character_,
#     age_years >= 0 & age_years < 5 ~ "0-4",
#     age_years >= 5 & age_years < 12 ~ "5-11",
#     age_years >= 12 ~ "12+"
#   ))
# 
# hospdat_recoded <-  age_recoded(hospdat_recoded)
# hospdat_new_vars <-  age_recoded(hospdat_new_vars)

### UKBB has changed name between COVID & COVID+FLU databases ###
update_dags <- . %>%
  mutate(redcap_data_access_group = case_when(redcap_data_access_group %in% "kispi_bs_basel" ~ "ukbb_basel",
                                              TRUE ~ redcap_data_access_group))

hospdat_recoded <-  update_dags(hospdat_recoded)
hospdat_new_vars <-  update_dags(hospdat_new_vars)

### Labels of hospitals for plots ###
code_dags <- . %>%
  mutate(dags_label = case_when(
    redcap_data_access_group %in% "hug_geneva" ~ "HUG",
    redcap_data_access_group %in% "usb_basel" ~ "USB",
    redcap_data_access_group %in% "kssg_stgallen__con" ~ "KSSG",
    redcap_data_access_group %in% "stgag_ksm_muenster" ~ "STGAG",
    redcap_data_access_group %in% "eoc_lugano" ~ "EOC",
    redcap_data_access_group %in% "hopital_vs_sion" ~ "HVS",  
    redcap_data_access_group %in% "chuv_lausanne" ~ "CHUV",
    redcap_data_access_group %in% "inselspital_bern" ~ "INSEL",
    redcap_data_access_group %in% "luks_luzern" ~ "LUKS",
    redcap_data_access_group %in% "usz_zurich" ~ "USZ",
    redcap_data_access_group %in% "kispi_zh_zurich" ~ "KISPI ZH",
    redcap_data_access_group %in% "ksgr_graubuenden" ~ "KSGR",
    redcap_data_access_group %in% "oks_st_gallen" ~ "OKS",
    redcap_data_access_group %in% "hfr_fribourg" ~ "PED HFR",
    redcap_data_access_group %in% "ksa_aarau" ~ "PED KSA",
    redcap_data_access_group %in% "ksw_winterthur" ~ "PED KSW",
    redcap_data_access_group %in% "ksnw_niedwalden" ~ "KSNW",  
    redcap_data_access_group %in% "spitaeler_sh_schaf" ~ "SSH",
    redcap_data_access_group %in% "hirslanden_ag_zh_z" ~ "Hirsl. ZH",
    redcap_data_access_group %in% "ukbb_basel" ~ "UKBB",
    redcap_data_access_group %in% "hirslanden_klinik" ~ "Hirsl. LU",
    redcap_data_access_group %in% "hopital_jura_ju" ~ "HJU")
  )

hospdat_recoded <-  code_dags(hospdat_recoded)
hospdat_new_vars <-  code_dags(hospdat_new_vars)

### Extensive status ###
extend_outcome <- . %>%
  mutate(status_exp = case_when(
    status %in% "Died" & (status_death_cause_covid_flu %in% c("Yes","Unknown") | is.na(status_death_cause_covid_flu)) ~ "Died from virus",
    status %in% "Died" & status_death_cause_covid_flu %in% "No" ~ "Died from other cause",
    TRUE ~ status
  ))

hospdat_recoded <- extend_outcome(hospdat_recoded)
hospdat_new_vars <- extend_outcome(hospdat_new_vars)

### Immunosupression ###
immunosup <- . %>%
  mutate(immuno_sup_nohiv = case_when(
    com_hemato_immuno %in% c("Yes", 1) ~ 1,
    #com_rheumato_immuno %in% c("Yes", 1) ~ 1,
    com_immuno_treat %in% c("Yes", 1) ~ 1,
    com_transplant %in% c("Yes", 1) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(immuno_sup = case_when(
    com_hemato_immuno %in% c("Yes", 1) ~ 1,
    #com_rheumato_immuno %in% c("Yes", 1) ~ 1,
    com_immuno_treat %in% c("Yes", 1) ~ 1,
    com_transplant %in% c("Yes", 1) ~ 1,
    com_hivpos %in% c("Yes", 1) ~ 1,
    TRUE ~ 0
  ))


hospdat_recoded <- immunosup(hospdat_recoded)
hospdat_new_vars <- immunosup(hospdat_new_vars)

### vaccination status following HUG definition ###
### 
vacc_stat_hug <- c("Fully immunised (w. additional boosters)",
                   "Fully immunised",
                   "Base immunised",
                   "Partially immunised",
                   "Not immunised",
                   "Unknown status")

# hug_vacc_status <- .%>%
#   mutate(tmp = case_when(
#     vaccincovid_dose %notin% "Unknown" ~ as.double(vaccincovid_dose),
#     TRUE ~ NA_real_)) %>%
#   mutate(covid_vacc_status_at_admission_hug = case_when(
#     ## @NOTE: I changed this, since there is no more cases with Fully immunized  AND 
#     ## nb of boosters = 4 or more, it is replaced by Fully immunized (with additional boosters)
#     ## Maria 26/10/2022
#     # covid_immunity_status_inclusive %in% "Fully immunized" & ((tmp %in% 3 & immuno_sup %in% 0) |
#     #                                                             (tmp %in% 4 & immuno_sup %in% 1)) ~ vacc_stat_hug[2],
#     # covid_immunity_status_inclusive %in% "Fully immunized" & ((tmp > 3 & immuno_sup %in% 0) |
#     #                                                             (tmp > 4 & immuno_sup %in% 1)) ~ vacc_stat_hug[1],
#     covid_immunity_status_inclusive %in% "Fully immunized" ~ vacc_stat_hug[2],
#     covid_immunity_status_inclusive %in% "Fully immunized (with additional boosters)" ~ vacc_stat_hug[1],
#     covid_immunity_status_inclusive %in% "Fully immunized" ~ vacc_stat_hug[2], 
#     covid_immunity_status_inclusive %in% "Base immunized" ~ vacc_stat_hug[3],
#     covid_immunity_status_inclusive %in% "Partially immunized" ~ vacc_stat_hug[4],
#     covid_immunity_status_inclusive %in% "Not immunized" ~ vacc_stat_hug[5],
#     covid_immunity_status_inclusive %in% "Unknown status" ~ vacc_stat_hug[6] 
#     )) %>%
#   mutate(covid_vacc_status_at_admission_hug = factor(covid_vacc_status_at_admission_hug, 
#                                                      ordered = TRUE, 
#                                                      levels = vacc_stat_hug))
# 
# hospdat_recoded <- hug_vacc_status(hospdat_recoded)
# hospdat_new_vars <- hug_vacc_status(hospdat_new_vars)



### Severity ###
severity_levels <- c("Intensive care unit admission",
                     "Intermediate care unit admission",
                     "Severe disease (Pneumoniae)",
                     "Non severe (Asymptomatic, mild or moderate disease)")

disease_severity <- . %>%
  mutate(severity_hug = case_when(
    any_icare_stay_hospi %in% TRUE ~ severity_levels[1],
    any_intermediate_stay_hospi %in% TRUE ~ severity_levels[2],
    compl_pneumonia %in% c("Yes", 1) & compl_pneumonia_inf %in% c("Yes", 1) ~ severity_levels[3],
    TRUE ~ severity_levels[4]
  )) %>%
  mutate(severity_hug = factor(severity_hug, 
                               ordered = TRUE, 
                               levels = severity_levels))

hospdat_recoded <- disease_severity(hospdat_recoded)
hospdat_new_vars <- disease_severity(hospdat_new_vars)

### Number of complications ###
complications <- c(#"compl_ent",
                   "compl_respiratory",
                   "compl_pims",
                   "compl_cardiovascular",
                   #"compl_digestive",
                   #"compl_liver",
                   #"compl_renal",
                   "compl_neuro_impair",
                   "compl_enceph",
                   #"compl_convulsion",
                   #"compl_psych",
                   #"compl_decond",
                   #"compl_decond", 
                   "compl_thrombosis") #, 
                   #"compl_bact",
                   #"compl_nonbact",
                    #"compl_other")

hospdat_new_vars$nb_complications <- rowSums(replace(hospdat_new_vars[,complications], hospdat_new_vars[,complications] == -1, 0), na.rm=TRUE) #trifouiller pour virer les -1 en 0
hospdat_recoded$nb_complications <- hospdat_new_vars$nb_complications #can do since same dataset, different coding

### Number of comorbidities ###
comorbidities <- c("com_respiratory",
                 "com_asthma",
                 "com_diabetes",
                 "com_hypertension",
                 "com_cardiovascular",
                 "com_neuro_impair",
                 "com_hemato_immuno",
                 "com_oncology",
                 #"com_rheumato_immuno",
                 "com_hivpos",
                 "com_immuno_treat")#,
                 #"com_others")

hospdat_new_vars$nb_comorbidities <- rowSums(replace(hospdat_new_vars[,comorbidities], hospdat_new_vars[,comorbidities] == -1, 0), na.rm=TRUE) #trifouiller pour virer les -1 en 0
hospdat_recoded$nb_comorbidities <- hospdat_new_vars$nb_comorbidities #can do since same dataset, different coding

### Number of symptoms ###
# symptoms <- c("severity___1",
#               "severity___2",
#               "severity___3",
#               "severity___4",
#               "severity___5",
#               "severity_children___1",
#               "severity_children___2",
#               "severity_children___3",
#               "severity_children___4",
#               "symptoms_extra___1",
#               "symptoms_extra___2",
#               "symptoms_extra___3",
#               "symptoms_extra___4",
#               "symptoms_extra___5",
#               "symptoms_extra___6",
#               "symptoms_extra___7",
#               "symptoms_extra___8",
#               "symptoms_extra___9",
#               "symptoms_extra___10")
# 
# hospdat_new_vars$nb_symptoms <- rowSums(replace(hospdat_new_vars[,symptoms], hospdat_new_vars[,symptoms] == -1, 0), na.rm=TRUE) #trifouiller pour virer les -1 en 0
# hospdat_recoded$nb_symptoms <- hospdat_new_vars$nb_symptoms #can do since same dataset, different coding

### Had a delta virus ###
#hospdat_recoded <- hospdat_recoded %>%
 # mutate(has_delta = case_when(grepl("AY", virus_type_cov, fixed = TRUE) ~ TRUE,
  #                             virus_type_cov %in% "B.1.617.2 - Delta" ~ TRUE,
   #                            corr_hospital_entry_date < as.Date("2021-07-01") ~ FALSE,
    #                           TRUE ~ FALSE))
#hospdat_new_vars$has_delta <- hospdat_recoded$has_delta #can do since same dataset, different coding

### Had a omicron virus ###
#hospdat_recoded <- hospdat_recoded %>%
   # mutate(has_omic_dropout = case_when(corr_hospital_entry_date < as.Date("2021-12-01") ~ FALSE, 
                                        #virus_type_cov %in% "B.1.1.529/BA.1 - Omicron" ~ TRUE,
                                        #virus_type_cov %in% "B.1.1.529/BA.2" ~ TRUE,
                                        #virus_type_cov %in% "BA.1.1" ~ TRUE,
                                        #grepl("BA.", virus_type_cov, fixed = TRUE) ~ TRUE,
                                         #TRUE ~ FALSE))
#hospdat_new_vars$has_omic_dropout <- hospdat_recoded$has_omic_dropout #can do since same dataset, different codings


### because with total
reasons_level <- c("Nosocomial SARS-CoV-2 infection",
                   "New hospitalisations with COVID-19",
                   "New hospitalisations because of COVID-19",
                   "Reason for hospitalisation undeterminable",
                   "Missing reason for hospitalisation")

reasons_level_std <- c("New hospitalisations because of COVID-19",
                       "New hospitalisations with COVID-19",
                       "Nosocomial SARS-CoV-2 infection",
                       "Reason for hospitalisation undeterminable",
                       "Missing reason for hospitalisation")

hospdat_recoded <- hospdat_recoded %>%
  mutate(because_with_total = case_when(!is.na(because_with_discharge) ~ because_with_discharge,
                                        TRUE ~ because_with_admission)) %>%
  mutate(reason_hug = case_when(case_classification_covid %in% c(2, "Nosocomial (> 5 days) from this hospital") ~ reasons_level[1],
                                because_with_total %in% "Because of COVID-19/Influenza" ~ reasons_level[3],
                                because_with_total %in% "With COVID-19/Influenza" ~ reasons_level[2],
                                because_with_total %in% c("No determination possible", "Not documented") ~ reasons_level[4],
                                TRUE ~ reasons_level[5])) %>%
  mutate(reason_hug = factor(reason_hug, 
                             ordered = TRUE, 
                             levels = reasons_level))

hospdat_new_vars$because_with_total <- hospdat_recoded$because_with_total #can do since same dataset, different coding
hospdat_new_vars$reason_hug <- hospdat_recoded$reason_hug #can do since same dataset, different coding



#########################
#         Filters       #
#########################
filter_data <- . %>%
  filter(!is.na(gender),
          gender != "Other",
          !is.na(age_years)) %>%
  filter(corr_hospital_entry_date >= start_date,
         corr_hospital_entry_date <= end_date) %>%
  mutate(corr_hospital_entry_week = factor(corr_hospital_entry_week, levels = week_levels, ordered = TRUE)) 

hospdat_recoded <-  filter_data(hospdat_recoded)
hospdat_new_vars <-  filter_data(hospdat_new_vars)

### Death week ### Done here because of the factor
death_week <- . %>%
  mutate(discharge_death_week = str_replace(tsibble::yearweek(discharge_death_date), " W","-")) %>%
  mutate(discharge_death_week = factor(discharge_death_week, levels = as.factor(sort(unique(.$corr_hospital_entry_week)))))

hospdat_new_vars <- death_week(hospdat_new_vars)
hospdat_recoded <- death_week(hospdat_recoded)