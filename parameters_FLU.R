#########################
#         Packages      #
#########################
pacman::p_load("plyr",
               "tidyverse", 
               "janitor",
               "xlsx",
               "lubridate",
               "BiocManager",
               "data.table",
               "Hmisc",
               "knitr",
               "viridis")

`%notin%` <- Negate(`%in%`)

#########################
#       Parameters      #
#########################
centres <- c("HUG", "ALL", "USB", "KSSG", "STGAG", "EOC", "HVS", "CHUV", "INSEL","LUKS", "USZ","KISPI ZH", "KSGR", "OKS", "PED HFR","PED KSW", "SSH", "UKBB", "Hirsl. LU")

disease <- 1 

if (disease %in% c(2,3)){
  disease_nm <- "COVID"
} else if (disease %in% 1){
  disease_nm <- "FLU"
}

#########################
#       Date of data    #
#########################
rootpath <- "../../../"


tdate <- Sys.Date()

### Load FOPH data for wbesite, if not already done today
foph_date <- as.Date("2022-11-16")
foph_folder <- paste0(rootpath,"data/FOPH/",foph_date,"/data/")

### Stuff for prediction code from Maroussia
export_week <- tsibble::yearweek(exportdate)
export_week_start <- as.Date(export_week)


#########################
#   Period to consider  #
#########################
start_date <- as.Date("2017-01-01")
end_date_sunday <- floor_date(exportdate,"week") 
end_date <- exportdate 
week_levels <- str_replace(tsibble::yearweek(seq.Date(from = start_date, 
                                                      to = end_date_sunday, 
                                                      by = "week")),
                           " W", "-")

two_weeks_ago <- end_date_sunday - 14 + 1 
one_week_ago <-  end_date_sunday - 7 + 1

last_phase_st <- as.Date("2022-10-01")

cur_week <- str_replace(tsibble::yearweek(end_date), " W","-")
last_week <- str_replace(tsibble::yearweek(end_date_sunday), " W","-")
two_last_week <- str_replace(tsibble::yearweek(end_date_sunday - 7), " W","-")
first_week <- str_replace(tsibble::yearweek(start_date), " W","-")

#########################
#       create dirs     #
#########################
#dir.create("_output_docx")
