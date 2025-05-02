# Clean the environment
rm(list = ls())

setwd("C:/Users/sobel/Desktop/IDMM_projects/additional_requests/Delay analysis")

exportdate <- as.Date("2023-10-17")
cleandate <- as.Date("2023-10-17")

source("parameters_COVID.R")
source("data_load_prep.R")

library("cowplot")
library("lubridate")
library("scales")
library("ComplexHeatmap")
library("gtsummary")

library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)

# full Epi curve Covid-19 community acquired.

p1=dplyr::filter(hospdat_recoded,case_classification_covid %in% c("Community acquired","Nosocomial (> 5 days) from this hospital"))%>%
  mutate(case_classification_covid=case_when(case_classification_covid =="Community acquired" ~ "Community acquired",case_classification_covid == "Nosocomial (> 5 days) from this hospital"~ "Nosocomial"))%>%                                                                                                       
  dplyr::group_by(week = lubridate::floor_date(corr_hospital_entry_date, 'week'),case_classification_covid) %>% 
  dplyr::summarize(case_nb = n())%>% data.frame()%>% ggplot(aes(ymd(week),case_nb,fill=case_classification_covid))+#geom_hline(yintercept = c(250,500,750),linetype=2,color="grey")+
  geom_bar(stat="identity")+theme_classic()+scale_x_date(expand=c(0,0),breaks = "6 month", minor_breaks = "3 month", labels=date_format("%b-%Y"))+
  scale_y_continuous(expand=c(0,0),limits = c(0,1500),breaks =seq(0,1500,by=250),name ="case count")+
  ggtitle("Hospitalised COVID-19 cases over time")+scale_fill_manual(values = c("darkgreen","lightgreen"))+
  theme(legend.position="bottom",text = element_text(size = 15),legend.title=element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p1)
# add covid waves 
# 1st wave: 25/02/2020 to 30/04/2020

#split according to charge

p1+geom_vline(linetype=2,xintercept = c(ymd("2020-09-27"),
                                        ymd("2021-02-14"),
                                        ymd("2021-06-20"),
                                        ymd("2021-10-10"),
                                        ymd("2022-05-22"),
                                        ymd("2022-09-04"),
                                        ymd("2023-01-15"),
                                        ymd("2023-06-25")
                                        ))

# split according to FOPH phases

p1+geom_vline(linetype=2,xintercept = c(ymd("2020-06-07"),
                                        ymd("2020-09-27"),
                                        ymd("2021-02-14"),
                                        ymd("2021-06-20"),
                                        ymd("2021-10-10"),
                                        ymd("2021-12-19"),
                                        ymd("2022-03-31")
                                        
))


# split according to dominant variant

p1+geom_vline(linetype=2,xintercept = dmy(c("07.06.2020", "14.02.2021", "20.06.2021", "19.12.2021", 
                                        "28.02.2022", "05.06.2022", "20.11.2022", "07.06.2020", 
                                        "14.02.2021", "20.06.2021", "10.10.2021", "19.12.2021", "31.03.2022"))
                                        
)


covid_newphases_specific <- data.frame(
  phase = c("1 Spring wave 2020", "2 Fall/winter wave 2020", "3 Alpha wave", "4 Delta wave",
            "5 BA.1 (Omicron) wave", "6 BA.2 (Omicron) wave", "7 BA.5 (Omicron) wave", 
            "8 BQ.1 (Omicron) wave","9 XBB wave onwards"),
  start_date = dmy(c("24-02-2020", "08-06-2020", "15-02-2021", "21-06-2021",
                     "20-12-2021", "01-03-2022", "06-06-2022", "15-11-2022","12-02-2023")),
  end_date = dmy(c("07-06-2020", "14-02-2021", "20-06-2021", "19-12-2021",
                   "28-02-2022", "05-06-2022", "14-11-2022","11-02-2023", "17-12-2023"))
)

#as.Date(today())

#### COVID-19 waves and Influenza seasons ####
inf_season <- c("2018/19 season", "2019/20 season", "2020/21 season", "2021/22 season","2022/23 season") 
#wave <- c("Phase 1 (02/2020-07/06/2020)", "Phase 2 (08/06/2020-27/09/2020)", "Phase 2b (28/09/2020-14/02/2021)", 
#          "Phase 3 (15/02/2021-20/06/2021)", "Phase 4 (21/06/2021-10/10/2021)", "Phase 5 (11/10/2021-19/12/2021)",
 #         "Phase 6 (20/12/2021-31/03/2022)", "Normal Situation (from 01/04/2022)")



wave_season_recode <- . %>%
  mutate(COVID_wave = case_when(
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[1]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[1])) ~ covid_newphases_specific$phase[1],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[2]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[2])) ~ covid_newphases_specific$phase[2],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[3]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[3])) ~ covid_newphases_specific$phase[3],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[4]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[4])) ~ covid_newphases_specific$phase[4],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[5]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[5])) ~ covid_newphases_specific$phase[5],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[6]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[6])) ~ covid_newphases_specific$phase[6],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[7]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[7])) ~ covid_newphases_specific$phase[7],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[8]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[8])) ~ covid_newphases_specific$phase[8],
    ((corr_hospital_entry_date >= covid_newphases_specific$start_date[9]) & (corr_hospital_entry_date <= covid_newphases_specific$end_date[9])) ~ covid_newphases_specific$phase[9],
    TRUE ~ NA_character_
  )) %>%
  mutate(COVID_wave = factor(COVID_wave, ordered = TRUE, levels =covid_newphases_specific$phase)) %>%
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

p1=dplyr::filter(hospdat_recoded,case_classification_covid %in% c("Community acquired","Nosocomial (> 5 days) from this hospital"))%>%
  mutate(case_classification_covid=case_when(case_classification_covid =="Community acquired" ~ "Community acquired",case_classification_covid == "Nosocomial (> 5 days) from this hospital"~ "Nosocomial"))%>%                                                                                                       
  dplyr::group_by(week = lubridate::floor_date(corr_hospital_entry_date, 'week'),COVID_wave) %>% 
  dplyr::summarize(case_nb = n())%>% data.frame()%>% ggplot(aes(ymd(week),case_nb,fill=COVID_wave))+#geom_hline(yintercept = c(250,500,750),linetype=2,color="grey")+
  geom_bar(stat="identity")+theme_classic()+scale_x_date(expand=c(0,0),breaks = "6 month", minor_breaks = "3 month", labels=date_format("%b-%Y"))+
  scale_y_continuous(expand=c(0,0),limits = c(0,1500),breaks =seq(0,1500,by=250),name ="case count")+
  ggtitle("Hospitalised COVID-19 cases over time")+#scale_fill_manual(values = c("darkgreen","lightgreen"))+
  theme(legend.position="bottom",text = element_text(size = 15),legend.title=element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p1)
# Summer 2020 wave: 01/05/2020 to 30/09/2020

# 2nd wave: 01/10/2020 to 14/02/2021

# 3rd wave: 15/02/2021 to 20/06/2021

# 4th wave: 21/06/2021 to 10/10/2021

# 5th wave: 11/10/2021 to ???  

# Create factor variable variant_s ("Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.4/Omicron BA.5")
# data_chsur <- data_chsur %>% mutate(variant_s = case_when(covid_sdropout %in% "S drop-out present" ~ "Omicron BA.1",
#                                                           corr_hospital_entry_date <= "2021-11-30" ~ "Delta",
#                                                           covid_sdropout %in% "S drop-out absent" &
#                                                             corr_hospital_entry_date >= "2021-12-01" &
#                                                             corr_hospital_entry_date <= "2022-02-08" ~ "Delta",
#                                                           (covid_sdropout %in% "No search performed" |
#                                                              covid_sdropout %in% NA) &
#                                                             corr_hospital_entry_date >= "2022-01-20" &
#                                                             corr_hospital_entry_date <= "2022-02-08" ~ "Omicron BA.1", 
#                                                           covid_sdropout %in% "S drop-out absent" &
#                                                             corr_hospital_entry_date >= "2022-02-09" &
#                                                             corr_hospital_entry_date <= "2022-05-09" ~ "Omicron BA.2",
#                                                           corr_hospital_entry_date >= "2022-03-28" &
#                                                             corr_hospital_entry_date <= "2022-05-09" ~ "Omicron BA.2",
#                                                           corr_hospital_entry_date >= "2022-07-04" ~ "Omicron BA.4/Omicron BA.5",
# )) %>%
#   mutate(variant_s = factor(variant_s, levels = c("Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.4/Omicron BA.5")))

#hospdat_recoded$inclusion_date

#hospdat_recoded$hospital_entry_date

#hospdat_recoded$centre_id
#hospdat_recoded$case_classification_covid

length(unique(hospdat_recoded$record_id))

length(unique(hospdat_recoded$patient_id))

pdf("delay_evolution_year_2023_COVID_20231031.pdf",width=10,height=6)
print(hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2022-11-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
  ggplot(aes(ymd(inclusion_date),delay,colour=factor(dags_label)))+
  geom_point(color="lightgrey")+geom_smooth()+xlab("inclusion")+ylab("delay [day]")+
  facet_wrap(~dags_label,ncol=6)+theme_bw()+ theme(legend.position="none")+ggtitle("Delay in COVID-19 cases entry in CH-SUR"))
dev.off()


# pdf("delay_distribution_year_2023_COVID_20231031.pdf",width=16,height=10)
# print(hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
#   dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
#     dplyr::filter(ymd(hospital_entry_date)>ymd("2022-11-01"),ymd(hospital_entry_date)<ymd("2023-10-01"))  %>%
#   dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
#   ggplot(aes(factor(dags_label),delay,fill=factor(dags_label)))+coord_flip()+
#   geom_violin()+geom_boxplot(size=0.3)+theme_bw()+theme(legend.position="none")+
# ylab("delay [day]")+xlab("centre")+ggtitle("Delay in COVID-19 cases entry in CH-SUR in 2023"))

# dev.off()


pdf("delay_inclusion_per_phase_COVID_20231712.pdf",width=10,height=6)

hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,COVID_wave,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),!is.na(COVID_wave),case_classification_covid=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
  ggplot(aes(factor(COVID_wave),delay,fill=factor(COVID_wave)))+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="none")+
  #geom_violin()+
  geom_boxplot(width=0.5,outlier.shape ="")+
  ylim(0,100)+
ylab("delay [day]")+xlab("Covid-19 wave")+ggtitle("Delay in COVID-19 cases entry in CH-SUR")

dev.off()


hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,COVID_wave,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),!is.na(COVID_wave),case_classification_covid=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>% 
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),q1=quantile(delay,0.25,na.rm=T),q3=quantile(delay,0.75,na.rm=T))%>%
  dplyr::mutate(DIQ=q3-q1)



test_df=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,COVID_wave,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),!is.na(COVID_wave),case_classification_covid=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())


delay_summary=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2022-11-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::filter(delay>=0)%>%
  group_by(dags_label)%>% 
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T))


delay_summary_total=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2020-01-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::filter(delay>=0)%>%
  #group_by(dags_label)%>% 
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),q1=quantile(delay,0.25,na.rm=T),q3=quantile(delay,0.75,na.rm=T))%>%
  dplyr::mutate(DIQ=q3-q1)




write.csv(delay_summary, "delay_hosp_summary_COVID_23112023.csv")

# delay plot

hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label,COVID_wave)%>%
  dplyr::filter(!is.na(inclusion_date),,!is.na(COVID_wave),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2020-01-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::filter(delay>=0) %>%  ggplot(aes(factor(COVID_wave),delay,fill=factor(COVID_wave)))+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="none")+
  #geom_violin()+
  geom_boxplot(width=0.5,outlier.shape ="")+
  ylim(0,100)+
  ylab("delay [day]")+xlab("Covid-19 wave")+ggtitle("Delay in COVID-19 cases entry in CH-SUR")


#check negative delays

delay_check_neg=hospdat_recoded%>%dplyr::select(centre_id,dags_label,record_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2022-11-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%dplyr::filter(delay<0)

print(delay_check_neg)

write.csv(delay_check_neg, "delay_neg_COVID_check_20231031.csv")

colnames(hospdat_recoded)
 

write.csv(delay_summary, "delay_summary_2023_COVID_20231031.csv")

delay_summary_wave=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,COVID_wave,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>% dplyr::filter(delay>0)%>%
  group_by(COVID_wave)%>% 
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),
                   Q1=quantile(delay,0.25,na.rm=T),
                   Q3=quantile(delay,0.75,na.rm=T))%>%dplyr::mutate(DIQ=Q3-Q1)

write.csv(delay_summary_wave, "delay_summary_COVID_wave_20231031.csv")

# with gtsummary

hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label,COVID_wave)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(COVID_wave),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2020-01-01"),ymd(hospital_entry_date)<ymd("2023-11-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::filter(delay>=0) %>%
   select(COVID_wave,delay)%>% tbl_summary(by=COVID_wave,missing="no")%>%add_p()%>%as_gt() 
   
   
   %>% gt::gtsave("tab_delay_COVID_wave_20231031.tex")



#

hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,COVID_wave,case_classification_covid,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>% dplyr::filter(delay>0)%>%
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),
                   Q1=quantile(delay,0.25,na.rm=T),
                   Q3=quantile(delay,0.75,na.rm=T))%>%dplyr::mutate(DIQ=Q3-Q1)


# Data Completness check

vars_of_interest_covid <- c("death", "intermediate_stay", "icare_stay",
                            "vaccincovid","comorbidity_good_health",
                            "case_classification_covid","complications",
                            "because_with_admission")

# vars_of_interest_covid <- c("hospital_entry_date",
# 	"inclusion_date",
# 	#"case_declaration_complete",
# "vaccincovid",
# #"vaccination",
# #"admission_complete",
# "comorbidity_good_health",
# #"antiv_infection_ttt",
# #"antibodies_ttt",
# #"im_strategies_ttt",
# "intermediate_stay",
# #"intermediate_stay_entry_date",
# "icare_stay",
# #"icare_stay_entry_date",
# "complications",
# #"compl_antibiotic_ttt",
# #"clinical_complementary_information_complete",
# "death",
# #"death_date"
# "case_classification_covid",
# "because_with_admission"#,
# #"patient_follow_up_complete"
# )

                           
all_vars_covid <- c("hospital_entry_date", vars_of_interest_covid,"COVID_wave")

h_to_plot <- hospdat_recoded %>%
#  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_covid)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(COVID_wave) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_covid),
                   .fns = ~ 100-round(100 * sum(is.na(.x) / n()), 1),
                   .names = "completness_pct_{.col}")) %>%
  ungroup() %>%  
  complete(COVID_wave)

write.csv(h_to_plot, "completness_summary_season_covid_20240309.csv")

median(data.matrix(h_to_plot[,2:13]))

view(h_to_plot)

all_vars_covid <- c(vars_of_interest_covid,"COVID_wave")

table1 <- tbl_summary(hospdat_recoded %>%
                       
                        filter(hospital_entry_date >= as.Date(start_date)) %>%
                         select(all_of(all_vars_covid)),

  by = "COVID_wave", # Grouping variable (am: transmission)
  statistic = list(
    #all_continuous() ~ "{mean} ({sd})", # For continuous variables, show mean (SD)
    all_categorical() ~ "{n} ({p}%)"  # For categorical variables, show count (percent)
  ),
  missing = "ifany" # Exclude missing data
) %>%add_p()%>%as_gt() %>% gt::gtsave("tab_1_covid_03092024.tex")

#############################3

#all_vars_covid <- c("dags_label", "hospital_entry_date", vars_of_interest_covid,"COVID_wave")

h_to_plot <- hospdat_recoded %>%
#  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_covid)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(COVID_wave) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_covid),
                   .fns = ~ 100-round(100 * sum(is.na(.x) / n()), 1),
                   .names = "completness_pct_{.col}")) %>%
  ungroup() %>%  
  complete(COVID_wave)


hm_to_plot=as.matrix(h_to_plot[1:9,2:dim(h_to_plot)[2]],ncol=12)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:9,1]))

library(ComplexHeatmap)

pdf( "completness_summary_waves_covid_20231031.pdf",width=6,height=4) 

Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name="completness (%)",col = colorRamp2(c(0,50, 100), c("gold","white", "blue")))

dev.off()

# h_to_plot <- hospdat_recoded %>%
#   #  filter(!(centre_id %in% list_centers_exclude)) %>%
#   filter(hospital_entry_date >= as.Date(start_date)) %>%
#   mutate(month = lubridate::floor_date(corr_hospital_entry_date,"month"))%>%
#   select(all_of(all_vars_covid)) %>%
#   #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
#   group_by(month) %>%
#   dplyr::summarise(across(.cols = all_of(vars_of_interest_covid),
#                           .fns = ~ round(100 * sum(is.na(.x) / n()), 1),
#                           .names = "na_pct_{.col}")) %>%
#   ungroup() %>%  
#   complete(month)

# hm_to_plot=as.matrix(h_to_plot[1:46,2:7],ncol=6)
# rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:46,1]$month))



###########
vars_of_interest_covid <- c("delay")

all_vars_covid <- c(vars_of_interest_covid,"COVID_wave")

table2 <- tbl_summary(hospdat_recoded %>%
                        
                        filter(hospital_entry_date >= as.Date(start_date)) %>%
                        dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
                        select(all_of(all_vars_covid)),
                      
                      by = "COVID_wave",
                      missing = "ifany" # Exclude missing data
) %>%add_p()%>%as_gt() %>% gt::gtsave("tab_2_covid.tex")
  #gt::as_latex()

h_present <- hospdat_recoded %>%
  #  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_covid)) %>%
  
  mutate(across(everything(), ~ ifelse(is.na(.), "No", .)))%>%
  mutate(case_classification_covid= ifelse(case_classification_covid=="Community acquired", "Yes", "No"))%>%
  mutate(because_with_admission= ifelse(because_with_admission=="Because of COVID-19/Influenza", "Yes", "No"))%>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(COVID_wave) %>%
  dplyr::summarise(across(
    .cols = all_of(vars_of_interest_covid),
    .fns = list(
      sum = ~ sum(.x == "Yes", na.rm = TRUE), # Count of "Yes"
      pct = ~ round(100 * mean(.x == "Yes", na.rm = TRUE), 1) # Percentage of "Yes"
    ),
    .names = "{.col}_{.fn}" # Naming the resulting columns
  )) %>%
  ungroup() %>%
  complete(COVID_wave)

write.csv(h_present, "db_stats_summary_season_covid_20231031.csv")

hm_to_plot=as.matrix(h_to_plot[1:9,2:13],ncol=12)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:9,1]))
Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE)

vars_of_interest_covid <-colnames(hospdat_recoded)[grep("complete",colnames(hospdat_recoded))]

all_vars_covid <- c("dags_label", "hospital_entry_date", vars_of_interest_covid,"COVID_wave")


h_to_plot <- hospdat_recoded %>%
  #  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_covid)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(COVID_wave) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_covid),
                          .fns = ~ round((100 * sum(.x ==0) / n()), 1),
                          .names = "incomplete_{.col}")) %>%
  ungroup() %>%  
  complete(COVID_wave)

hm_to_plot=as.matrix(h_to_plot[1:9,2:6],ncol=5)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:9,1]))


pdf("completness_summary_waves_covid_20231031.pdf",width=6,height=5)
print(
Heatmap(t(hm_to_plot), cluster_rows = FALSE, cluster_columns = FALSE, name = "complete (%)", col = colorRamp2(c(0,50, 100), c("gold","white", "blue")))
)
dev.off()   

# completness instuments

vars_of_interest_covid <-colnames(hospdat_recoded)[grep("complete",colnames(hospdat_recoded))]

all_vars_covid <- c("dags_label", "hospital_entry_date", vars_of_interest_covid,"COVID_wave")


h_to_plot <- hospdat_recoded %>%
  #  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_covid)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(COVID_wave) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_covid),
                          .fns = ~100- round((100 * sum(.x ==0) / n()), 1),
                          .names = "completeness_{.col}")) %>%
  ungroup() %>%  
  complete(COVID_wave)

hm_to_plot=as.matrix(h_to_plot[1:9,2:7],ncol=6)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:9,1]))

pdf("completness_summary_instruments_covid_20231031.pdf",width=6,height=4)
Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name = "completeness [%]",column_title="Influenza season instruments", col = colorRamp2(c(90,95,100), c("gold", "white","blue")))

dev.off()

#df_delay=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid)%>%
 # dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
  #dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())

source("parameters_FLU.R")
source("data_load_prep.R")

season_colors_fill <- c("2018/19" = "#041669", 
                        "2019/20" = "#1431b5", 
                        "2020/21" = "#563ec9",
                        "2021/22" = "#e0d910",
                        "2022/23" = "#cc1042",
                        "2022/23" = "#CC10B3")



hospdat_recoded%>%dplyr::select(dags_label,centre_id,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
  ggplot(aes(ymd(inclusion_date),delay,colour=factor(dags_label)))+
  geom_point(color="lightgrey")+geom_smooth()+xlab("inclusion")+ylab("delay [day]")+
  facet_wrap(~dags_label,ncol=6)+theme_bw()+ theme(legend.position="none")+ggtitle("Delay in influenza cases entry in CH-SUR")

hospdat_recoded%>%dplyr::select(dags_label,centre_id,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
  ggplot(aes(factor(dags_label),delay,fill=factor(dags_label)))+coord_flip()+
  geom_violin()+geom_boxplot(size=0.3)+theme_bw()+theme(legend.position="none")+
  ylab("delay [day]")+xlab("centre")+ggtitle("Delay in influenza cases entry in CH-SUR")

pdf("delay_inclusion_per_season_FLU_20231130.pdf",width=5,height=4)

hospdat_recoded%>%dplyr::select(Flu_season,centre_id,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired",!is.na(Flu_season))%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
  ggplot(aes(factor(Flu_season),delay))+#coord_flip()+
  geom_boxplot(size=0.3,width=0.6,outlier.shape ="",aes(fill=Flu_season))+theme_bw()+scale_fill_manual(values=c("2018/19 season" = "#041669", 
                                                                                           "2019/20 season" = "#1431b5", 
                                                                                           "2020/21 season" = "#563ec9",
                                                                                           "2021/22 season" = "#e0d910",
                                                                                           "2022/23 season" = "#cc1042"
                                                                                          ))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="none")+
  #geom_violin()+
  ylab("delay [day]")+xlab("influenza season")+ggtitle("Delay in influenza cases entry in CH-SUR")+ylim(c(0,20))

dev.off()

delay_summary=hospdat_recoded%>%dplyr::select(centre_id,dags_label,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  group_by(dags_label)%>% 
  dplyr::summarise(nb_case=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T))

delay_summary_tot=hospdat_recoded%>%dplyr::select(centre_id,dags_label,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::summarise(nb_case=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),
                   Q1=quantile(delay,0.25,na.rm=T),
                    Q3=quantile(delay,0.75,na.rm=T))%>%dplyr::mutate(DIQ=Q3-Q1)


write.csv(delay_summary, "delay_summar_FLU_20231130.csv")


delay_summary_season=hospdat_recoded%>%dplyr::select(centre_id,Flu_season,inclusion_date,hospital_entry_date,case_classification_flu)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  group_by(Flu_season)%>% 
  dplyr::summarise(nb_case=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),
                   Q1=quantile(delay,0.25,na.rm=T),
                   Q3=quantile(delay,0.75,na.rm=T))%>%dplyr::mutate(DIQ=Q3-Q1)

write.csv(delay_summary_season, "delay_summary_season_FLU_20231130.csv")


vars_of_interest_flu <- c("death", "intermediate_stay", "icare_stay",
                            "vaccination","comorbidity_good_health","complications",    
                            "case_classification_flu")

all_vars_flu <- c(vars_of_interest_flu,"Flu_season")

table1 <- tbl_summary(hospdat_recoded %>%
                        
                        filter(hospital_entry_date >= as.Date(start_date)) %>%
                        select(all_of(all_vars_flu)),
                      
                      by = "Flu_season", # Grouping variable (am: transmission)
                      statistic = list(
                        #all_continuous() ~ "{mean} ({sd})", # For continuous variables, show mean (SD)
                        all_categorical() ~ "{n} ({p}%)"  # For categorical variables, show count (percent)
                      ),
                      missing = "ifany" # Exclude missing data
) %>%add_p()%>%as_gt() %>% gt::gtsave("tab_1_flu.tex")


# Data Completness check

# vars_of_interest_covid <- c("death", "intermediate_stay", "icare_stay",
#                             "vaccination",
#                             "case_classification_flu")
# vars_of_interest_covid <- c("hospital_entry_date",
#                             "inclusion_date",
#                             #"case_declaration_complete",
#                             "vaccination",
#                             #"vaccination",
#                             #"admission_complete",
#                             "comorbidity_good_health",
#                             #"antiv_infection_ttt",
#                             #"antibodies_ttt",
#                             #"im_strategies_ttt",
#                             "intermediate_stay",
#                             #"intermediate_stay_entry_date",
#                             "icare_stay",
#                             #"icare_stay_entry_date",
#                             "complications",
#                             #"compl_antibiotic_ttt",
#                             #"clinical_complementary_information_complete",
#                             "death",
#                             #"death_date"
#                             "case_classification_flu"#,
#                             #"because_with_admission",
#                             #"patient_follow_up_complete"
# )

all_vars_flu <- c("dags_label", "hospital_entry_date", vars_of_interest_flu,"Flu_season")

h_to_plot <- hospdat_recoded %>%
  #  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_flu)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(Flu_season) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_flu),
                          .fns = ~ 100-round(100 * sum(is.na(.x) / n()), 1),
                          .names = "completeness_pct_{.col}")) %>%
  ungroup() %>%  
  complete(Flu_season)

write.csv(h_to_plot, "completness_summary_season_flu_20231130.csv")

hm_to_plot=as.matrix(h_to_plot[1:5,2:8],ncol=7)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:5,1]))

library(ComplexHeatmap)

Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name = "completness [%]",column_title="Influenza season completness", col = colorRamp2(c(0,50, 100), c("gold", "white","blue")))

pdf( "completness_summary_season_flu_20231130.pdf",width=6,height=4)
print(
Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name = "completness [%]",column_title="Influenza season completness", col = colorRamp2(c(0,50, 100), c("gold", "white","blue")))
)
dev.off()



vars_of_interest_flu <-colnames(hospdat_recoded)[grep("complete",colnames(hospdat_recoded))]

all_vars_flu <- c("dags_label", "hospital_entry_date", vars_of_interest_flu,"Flu_season")


h_to_plot <- hospdat_recoded %>%
  #  filter(!(centre_id %in% list_centers_exclude)) %>%
  filter(hospital_entry_date >= as.Date(start_date)) %>%
  select(all_of(all_vars_flu)) %>%
  #mutate(hospital_entry_month = yearmonth(as.Date(hospital_entry_date))) %>%
  group_by(Flu_season) %>%
  dplyr::summarise(across(.cols = all_of(vars_of_interest_flu),
                          .fns = ~100- round((100 * sum(.x ==0) / n()), 1),
                          .names = "completeness_{.col}")) %>%
  ungroup() %>%  
  complete(Flu_season)

hm_to_plot=as.matrix(h_to_plot[1:5,2:7],ncol=6)
rownames(hm_to_plot)=as.character(unlist(h_to_plot[1:5,1]))
Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name = "completeness [%]",column_title="Influenza season instruments", col = colorRamp2(c(90,95,100), c("gold", "white","blue")))

pdf( "completness_summary_season_flu_20231130_instruments.pdf",width=6,height=4)
print(
Heatmap(t(hm_to_plot),cluster_rows = FALSE,cluster_columns = FALSE,name = "completeness [%]",column_title="Influenza season instruments", col = colorRamp2(c(90,95,100), c("gold", "white","blue")))
)
dev.off()

 
vars_of_interest_flu <- c("delay")

all_vars_flu <- c(vars_of_interest_flu,"Flu_season")

table2 <- tbl_summary(hospdat_recoded %>%
                        
                        filter(hospital_entry_date >= as.Date(start_date)) %>%
                        dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
                        dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
                        select(all_of(all_vars_flu)),
                      
                      by = "Flu_season",
                      missing = "ifany" # Exclude missing data
) %>%add_p()%>%as_gt()%>% gt::gtsave("tab_2_flu.tex")



#df_delay=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_flu)%>%
 # dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  #dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())


pdf("delay_analysis_per_hosp_fig_20231031.pdf", height=6,width=10)

dags=unique(hospdat_recoded$dags_label)

for(i in 1:length(dags)){
  #i=1
  try({
  source("parameters_COVID.R")
  source("data_load_prep.R")
  
  p1= hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
    dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
    dplyr::filter(dags_label==dags[i])%>%
    dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
    ggplot(aes(ymd(inclusion_date),delay))+
    geom_point(color="lightgrey")+geom_smooth()+xlab("inclusion")+ylab("delay [day]")+
    theme_bw()+ theme(legend.position="none")+ggtitle(paste("Delay in COVID-19 cases in",dags[i]))
  
  p2=  hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_covid,dags_label)%>%
    dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_covid=="Community acquired")%>%
    dplyr::filter(dags_label==dags[i])%>%
    dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
    ggplot(aes(factor(dags_label),delay))+
    geom_boxplot(size=0.5)+theme_bw()+theme(legend.position="none")+
    ylab("delay [day]")+xlab("centre")
  
  source("parameters_FLU.R")
  source("data_load_prep.R")
  
  p3= hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_flu,dags_label)%>%
    dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
    dplyr::filter(dags_label==dags[i])%>%
    dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
    ggplot(aes(ymd(inclusion_date),delay))+
    geom_point(color="lightgrey")+geom_smooth()+xlab("inclusion")+ylab("delay [day]")+
    theme_bw()+ theme(legend.position="none")+ggtitle(paste("Delay in influenza cases in",dags[i]))
  
  p4=  hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_flu,dags_label)%>%
    dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
    dplyr::filter(dags_label==dags[i])%>%
    dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>% dplyr::filter(delay>0)%>%
    ggplot(aes(factor(dags_label),delay))+
    geom_boxplot(size=0.5)+theme_bw()+theme(legend.position="none")+
    ylab("delay [day]")+xlab("centre")
  
 print(plot_grid(p1, p2,p3,p4, labels = c('A', 'B','C','D'), label_size = 12,rel_widths = c(.7,.3,.7,.3)))
})  

}

dev.off() 

############################################

setwd("C:/Users/sobel/Desktop/IDMM_projects/additional_requests/Delay analysis")

exportdate <- as.Date("2023-10-17")
cleandate <- as.Date("2023-10-17")

source("parameters_COVID.R")
source("data_load_prep.R")

hospdat_recoded_COVID <- hospdat_recoded

hospdat_recoded_COVID$disease <- "COVID-19"

source("parameters_FLU.R")
source("data_load_prep.R")

hospdat_recoded_FLU <- hospdat_recoded

delay_summary_total=hospdat_recoded%>%dplyr::select(centre_id,inclusion_date,hospital_entry_date,case_classification_flu,dags_label)%>%
  dplyr::filter(!is.na(inclusion_date),!is.na(hospital_entry_date),case_classification_flu=="Community acquired")%>%
  dplyr::filter(ymd(hospital_entry_date)>ymd("2020-01-01"),ymd(hospital_entry_date)<ymd("2023-10-01")) %>%
  dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays())%>%
  dplyr::filter(delay>=0)%>%
  #group_by(dags_label)%>% 
  dplyr::summarise(nb_cases=n(),mean_delay=mean(delay,na.rm=T),
                   sd_delay=sd(delay,na.rm=T),
                   median_delay=median(delay,na.rm=T),q1=quantile(delay,0.25,na.rm=T),q3=quantile(delay,0.75,na.rm=T))%>%
  dplyr::mutate(DIQ=q3-q1)

hospdat_recoded_FLU$disease <- "Influenza"

hospdat_recoded_Both <- rbind(hospdat_recoded_COVID, hospdat_recoded_FLU)

selectev_var=c("record_id","patient_id","centre_id","gender",
"disease","death", "inclusion_date","any_icare_stay_patient","any_intermediate_stay_patient","age_years","bmi","complications","comorbidity_good_health","vaccincovid","vaccination","intermediate_stay","icare_stay","case_classification_covid","case_classification_flu","dags_label")

hospdat_recoded_Both <- hospdat_recoded_Both %>% select(all_of(selectev_var))
all_vars_flu <- c(vars_of_interest_flu,"Flu_season")

table_sum <- tbl_summary(hospdat_recoded_Both %>% select("gender",
"disease","death","hospital_entry_date", "inclusion_date","any_icare_stay_patient","any_intermediate_stay_patient","age_years","bmi","complications","comorbidity_good_health","vaccincovid","vaccination","intermediate_stay","icare_stay","case_classification_covid","case_classification_flu","dags_label")%>%
                        dplyr::mutate(delay=(ymd(inclusion_date)-ymd(hospital_entry_date))/ddays()) %>%
                      by = "disease",
                      missing = "ifany" # Exclude missing data

) %>%add_p()

table_sum%>%as_gt() %>% gt::gtsave("tab_1_both.tex")

# Convert the gt table to a data frame
table_df <- as.data.frame(table_sum)

# Save the data frame as an Excel file
openxlsx::write.xlsx(table_df, "tab_1_both.xlsx")
