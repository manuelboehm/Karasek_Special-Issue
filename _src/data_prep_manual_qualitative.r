###################
## Call all Data Files
###################

# Einlesen der Daten (read.csv oder alternativ read.csv2 bei deutschem Datenformat [. für Dezimalstellen und ; als Trennzeichen]
df <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/diary study_data_v3.CSV", na = "-999")
week_rv <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/week_review.csv", na = "NA")
holidays <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/holidays.csv", na = "NA")
jobscope <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/jobscope.csv", na = "?")
fbs_complete <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/AARL-BS_w1_all_FBS_complete.csv")
jobscope_correction <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/jobscope_v3.csv")
df_old <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/diary_study_data_old.csv")
fbs_data <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/AARL-BS_w1_TBS.csv")
weights_20 <- read.csv2("_data/Abgleich mit manuell bereinigten Daten/data_prep_qualitative/Gewichtung_Vollzeit_20.csv")
weights_fbs_20 <- weights_20



###################
## Data Preparation 
###################

# Matching mit den Daten aus jobscope_correction
jobscope$jobscope_correction <- NA
jobscope$jobscope_correction <- jobscope_correction$jobscope[match(jobscope$code, jobscope_correction$code)]
jobscope$jobscope_ <- ifelse(is.na(jobscope$jobscope_correction), jobscope$jobscope, jobscope$jobscope_correction)

# Vertauschen der Spaltennamen von von jobscope und jobscope_
names(jobscope)[names(jobscope) == "jobscope"] <- "jobscope_temp"
names(jobscope)[names(jobscope) == "jobscope_"] <- "jobscope"
names(jobscope)[names(jobscope) == "jobscope_temp"] <- "jobscope_"

# Umformatierungen
#df_old$`start_date+time_old` <- as.POSIXct(df_old$`start_date+time_old`, format = "%d.%m.%Y %H:%M")
df_old$`start_date+time_old` <- format(df_old$`start_date+time_old`, format = "%Y-%m-%d %H:%M")
#df_old$`end_date+time_old` <- as.POSIXct(df_old$`end_date+time_old`, format = "%d.%m.%Y %H:%M")
df_old$`end_date+time_old` <- format(df_old$`end_date+time_old`, format = "%Y-%m-%d %H:%M")

#df$`start_date+time` <- as.POSIXct(df$`start_date+time`, format = "%d.%m.%Y %H:%M")
df$`start_date+time` <- format(df$`start_date+time`, format = "%Y-%m-%d %H:%M")
#df$`end_date+time` <- as.POSIXct(df$`end_date+time`, format = "%d.%m.%Y %H:%M")
df$`end_date+time` <- format(df$`end_date+time`, format = "%Y-%m-%d %H:%M")
holidays$date <- as.Date(format(as.POSIXlt(holidays$date, format = "%d.%m.%Y")), "%Y-%m-%d")
week_rv$begin <- as.POSIXct(week_rv$begin, format = "%d.%m.%Y %H:%M")
week_rv$begin <- format(week_rv$begin, format = "%Y-%m-%d %H:%M")
week_rv$end <- as.POSIXct(week_rv$end, format = "%d.%m.%Y %H:%M")
week_rv$end <- format(week_rv$end, format = "%Y-%m-%d %H:%M")
week_rv$fragebogen <- fbs_complete$Fragebogen[match(week_rv$code, fbs_complete$SERIAL)]
# week_rv$sum_rv <- df$sum_rv[match(week_rv$code, df$code)]
week_rv$wt_fbs <- df$wt_fbs[match(week_rv$code, df$code)]

df$cw <- df$calendar_week 
df <- subset(df, select = -calendar_week)

# Wenn Stress = 0 & Coping = -1/na: Coping = 0
df$coping <- ifelse(df$stress == 0 & (df$coping == -1 | is.na(df$coping)),
                    0, df$coping) 
summary(is.na(df$coping)) # View coping from df
summary(is.na(df$stress)) # View stress  from df

# Matching der verschiedenen Tabellen

# holidays-match (tasks)
df$`start_date+time_` <- as.Date(format(as.POSIXlt(df$`start_date+time`, format = "%Y-%m-%d %H:%M")), "%Y-%m-%d")

df$holidays <- holidays$holiday_tasks[match(df$`start_date+time_`, holidays$date)]
# Holidays_ aus manueller Suche übernehmen in holidays
df$holidays[df$holidays_ == 1] <- 1
df <- df[, !(colnames(df) %in% c("start_date+time_", "deputat", "stellenprozent", "fulltime", "week"))]

# holidays-match (week-review)
week_rv$begin_ <- as.Date(format(as.POSIXlt(week_rv$begin, format = "%Y-%m-%d %H:%M")), "%Y-%m-%d")
week_rv$holidays <- holidays$holiday_wr[match(week_rv$begin_, holidays$date)]
# Wenn week_rv_holidays_ = 1, dann week_rv$holidays = 1
week_rv$holidays[week_rv$holidays_ == 1] <- 1

week_rv <- subset(week_rv, select = -begin_)

# Zuordnung der Summe der Tätigkeiten aus dem Wochenrückblick 
df$holidays_week_rv <- week_rv$holidays[match(df$code_week, week_rv$code_week)]

# jobscope-match
# (Es sind nun alle jobscopes+age+gender enthalten, nicht nur die mit Werten in allen drei Arbeitszeitwerten. 
# --> Wird bei späteren Auswertungen evtl. benötigt! Das heißt, Filterung muss passieren, wenn Matching der 3 Werte nicht möglich ist )
df$jobscope_ <- NA
df$jobscope_ <- jobscope$jobscope[match(df$code, jobscope$code)]
df$jobscope <- ifelse(is.na(df$jobscope_), df$jobscope, df$jobscope_)
# df$deputat <- jobscope$deputat[match(df$code, jobscope$code)]

df$deputat_full <- jobscope$deputat_full[match(df$code, jobscope$code)]
df$age_ <- NA
df$age_ <- jobscope_correction$age[match(df$code, jobscope$code)]
df$age <- ifelse(is.na(df$age_), df$age, df$age_)

df$gender_ <- NA
df$gender_ <- jobscope$gender[match(df$code, jobscope$code)]
df$gender <- ifelse(is.na(df$gender_), df$gender, df$gender_)
# df$gender <- jobscope$gender[match(df$code, jobscope$code)] #### Nur valide für Lehrkräfte ohne Leitungsfunktion

# Übertrag des Werts für jobscope FBS in df
df$jobscope_fbs_ <- NA
df$jobscope_fbs_ <- jobscope_correction$jobscope_fbs[match(df$code, jobscope$code)]
df$jobscope_fbs <- ifelse(is.na(df$jobscope_fbs_), df$jobscope, df$jobscope_fbs_)

df <- subset(df, select = -c(age_, jobscope_, gender_, jobscope_fbs_))
# df <- merge(df, jobscope_correction[, c("code", "jobscope_fbs")], by = "code", all.x = TRUE)

df$fbs_wt_short_holid <- fbs_data$AZF_04[match(df$code, fbs_data$SERIAL)]

# Ersetze das Dezimaltrennzeichen von , zu .
df$fbs_wt_short_holid <- gsub(",", ".", df$fbs_wt_short_holid)
# Führe die Umwandlung von character in numeric durch
df$fbs_wt_short_holid <- as.numeric(df$fbs_wt_short_holid)
df$fbs_wt_short_holid <- replace(df$fbs_wt_short_holid, df$fbs_wt_short_holid %in% c(-8, -9), NA)

# Gruppen für die Stichprobe und Gewichtung
age_groups <- cut(df$age,
                  breaks = c(0, 35, 45, 55, Inf),
                  labels = c("u35", "35-u45", "45-u55", "55-65+"),
                  right = FALSE)

gender_groups <- cut(df$gender,
                     breaks = c(1, 2, 3),
                     labels = c("weiblich", "männlich"),
                     right = FALSE)

jobscope_groups_tbs <- cut(df$jobscope,
                           breaks = c(0, 100, Inf),
                           labels = c("Teilzeit", "Vollzeit"),
                           right = FALSE)

jobscope_groups_fbs <- cut(df$jobscope_fbs,
                           breaks = c(0, 100, Inf),
                           labels = c("Teilzeit", "Vollzeit"),
                           right = FALSE)

# Dataframe erweitern um Altersgruppen und Jobscope-Gruppen und Gendergruppen
df <- df %>%
  mutate(age_group = age_groups, 
         gender_group = gender_groups,
         jobscope_group_tbs = jobscope_groups_tbs,
         jobscope_group_fbs = jobscope_groups_fbs)

## N = 141 (TBS und FBS nach Matching)
df$weight_20 <- NA
df$weight_20 <- ifelse(df$jobscope >= 100 & df$gender == 2, df$weight_20 <- 1.20912300929596, ifelse(df$jobscope >= 100 & df$gender == 1, df$weight_20 <- 0.755486942977035, NA)) # männlich = 0, weiblich = 1



###################
## Filters Data Working Time (df_filters_working_time + week_rv_wt)
###################

# filtering was removed as it is not needed currently


###################
## Neue Task Frameworks zuordnen (12 & 3) ############## NOCH aufbereiten
###################

# Lade die Zuordnungstabelle aus der CSV-Datei
# zuordnung_task_new <- read.csv2("zuordnung_new_task.csv")

# the rest was removed as it is not needed currently ...



###################
## Wochentag ermitteln für die Tätigkeiten
###################

# Neue Spalten weekday_start + weekday_end
df <- df %>%
  mutate(weekday_start = weekdays(as.POSIXct(`start_date+time`, format = "%Y-%m-%d %H:%M")))
df <- df %>%
  mutate(weekday_end = weekdays(as.POSIXct(`end_date+time`, format = "%Y-%m-%d %H:%M")))



###################
## Uhrzeit einfügen für die Auswertung der Arbeitszeit abends
###################

# Extrahiere die Uhrzeit aus der Spalte "start_date+time" und "end_date+time"
# start time
df <- df %>%
  mutate(start_time = format(as.POSIXct(`start_date+time`, format = "%Y-%m-%d %H:%M"), "%H:%M"))

# end time 
df <- df %>%
  mutate(end_time = format(as.POSIXct(`end_date+time`, format = "%Y-%m-%d %H:%M"), "%H:%M"))



# prepare variable names in df
df <- df %>%
  rename(sex = gender, act_no = task, activity = task_name)