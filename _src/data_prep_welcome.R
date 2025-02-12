#########
### Data Prep Welcome
#########

welcome_anonymous <- welcome_raw %>%
  select(-MailAdresse, -SETgvMailAdresse, -Email, -Email2)

# clean welcome
# rename variables

welcome_renamed <- welcome_anonymous %>%
  rename(
    version = Version,
    canceled = Canceled, 
    agreement = Zustimmung, 
    code = Code, 
    consent = Einwilligung, 
    consent2 = Einwilligung2, 
    consent3 = Einwilligung3, 
    # jobscope = Stellenumfang, 
    sex = Geschlecht, 
    age = Alter
  )
welcome_renamed$jobscope <- welcome_renamed$Stellenumfang 

# code only in capital letters
welcome_renamed <- welcome_renamed %>%
  mutate(code = toupper(code))

# remove codes used for test purposes (WEITERE?! - check notes!!!)
welcome_filtered <- welcome_renamed %>%
  filter(code != "112UAYBZQB", 
         code != "D3REDEZ6B1", 
         code != "fjf", 
         code != "L9ZA3VX7PK", 
         code != "5KCAHBHQK5")

rm(welcome_renamed)
# rm(welcome_test)

welcome_prep <- welcome_filtered %>%
  filter(canceled == 0, 
         consent == 1)

rm(welcome_filtered)

welcome <- welcome_prep %>%
  select(-canceled, -DeviceID, -consent, -consent2, -consent3, -UPDgvZustimmung, 
         -Interviewer, -CodeReset, -SETgvZustimmung, 
         -SETgvDeviceID, -SETgvUser, -SETgvCode, -TNCodeInput, -agreement, 
         -version)

rm(welcome_prep)

# check email for invalid entries
# welcome_test <- as.data.frame(unique(welcome$email))
jobscope$jobscope_correction <- NA
jobscope$jobscope_correction <- jobscope_correction$jobscope[match(jobscope$code, jobscope_correction$code)]
jobscope$jobscope_ <- ifelse(is.na(jobscope$jobscope_correction), jobscope$jobscope, jobscope$jobscope_correction)

# Vertauschen der Spaltennamen von jobscope und jobscope_ (DAVOR: jobscope = 290 ohne korrigierte jobscopes, mit NAs)
#                                                           (DANACH: jobscope = 344 mit Korrekturen, ohne NAs)
names(jobscope)[names(jobscope) == "jobscope"] <- "jobscope_temp"
names(jobscope)[names(jobscope) == "jobscope_"] <- "jobscope"
names(jobscope)[names(jobscope) == "jobscope_temp"] <- "jobscope_"

### jobscope-match
# (Es sind nun alle jobscopes+age+gender enthalten, nicht nur die mit Werten in allen drei Arbeitszeitwerten. 
# --> Wird bei späteren Auswertungen evtl. benötigt! Das heißt, Filterung muss passieren, wenn Matching der 3 Werte nicht möglich ist )
welcome$jobscope_ <- NA
welcome$jobscope_ <- jobscope$jobscope[match(welcome$code, jobscope$code)]
welcome$jobscope <- ifelse(is.na(welcome$jobscope_), welcome$jobscope, welcome$jobscope_)

# individual changes
welcome <- welcome %>%
  mutate(jobscope = case_when(code == "S1VC8VP43V" ~ 48, 
                              .default = jobscope))
#####
## Q: Where is the cutoff-value for jobscope?! 22 or 25?!
#####

welcome <- welcome %>%
  select(-jobscope_, -Stellenumfang, -gvUser, -id, -Begin, -End, -Handover, 
         -Duration)

# only 1 row for each person
# check 
# welcome_test <- as.data.frame(unique(welcome$code))

welcome_test <- welcome %>% 
  group_by(code) %>% 
  filter(max(row_number()) > 1) %>% 
  ungroup()


# check age for differences in one person
welcome_test <- welcome %>% 
  group_by(code) %>% 
  filter(max(row_number()) > 1) %>% 
  ungroup()

# remove duplicate rows
welcome_unique <- distinct(welcome, code, .keep_all = TRUE) 
welcome <- welcome_unique

rm(welcome_raw)
rm(welcome_anonymous)
rm(welcome_test)
rm(welcome_unique)

rm(jobscope)
rm(jobscope_correction)
