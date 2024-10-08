#########
### Data Prep Activities
#########

# clean activities
# rename variables
activities_renamed <- activities_raw %>%
  rename(
    version = Version,
    canceled = Canceled, 
    code = Code, 
    email = MailAdresse, 
    consent = Zustimmung, 
    category = X00, 
    task_no = Task, 
    other = Tasksnst, 
    begin_date = Datum, 
    begin_time = Start, 
    end_date = X77, 
    end_time = Ende, 
    stress = Stress, 
    coping = Coping, 
    pc_learn = Lernen, 
    learn_descr = Lernaussage, 
    other_descr = Aussage, 
    task = gvTASK01
  )

# code only in capital letters
activities_renamed <- activities_renamed %>%
  mutate(code = toupper(code))

activities_filtered <- activities_renamed %>%
  filter(canceled == 0, 
         consent == 1 | consent == "Ja")

activities_prep <- activities_filtered %>%
  select(-canceled, -DeviceID, -consent, -email, -gvTASK10, -gvTASK02, -gvTASK03, 
         -gvTASK04, -gvTASK05, -gvTASK06, -gvTASK07, -gvTASK08, -gvTASK09, 
         -SETgvZustimmung, -SETMailAdresse, -SETCode, -SETgvUser, -SETgvTASK01, 
         -SETgvTASK02, -SETgvTASK03, -SETgvTASK04, -SETgvTASK05, -SETgvTASK06, 
         -SETgvTASK07, -SETgvTASK08, -SETgvTASK09, -SETgvTASK10, -SETgvDATEstart, 
         -UPDgvDATEstart, -UPDgvTASK10, -UPDgvTASK01, -UPDgvTASK02, -UPDgvTASK03, 
         -UPDgvTASK04, -UPDgvTASK05, -UPDgvTASK06, -UPDgvTASK07, -UPDgvTASK08, 
         -UPDgvTASK09, -UPDsonstTASK, -Interviewer, -gvDATEstart, -version)

rm(activities_filtered)
rm(activities_renamed)
# rm(activities_raw)

# only valid cases (only consideration of codes with code in welcome questionnaire)
combined_df <- activities_prep %>%
  filter(code %in% welcome$code)

# import data from welcome questionnaire
combined_df$age <- welcome$age[match(combined_df$code, welcome$code)]
combined_df$sex <- welcome$sex[match(combined_df$code, welcome$code)]
combined_df$jobscope <- welcome$jobscope[match(combined_df$code, welcome$code)]
  
combined_df <- combined_df %>%
  select(id, code, sex, age, jobscope, gvUser, category, task_no, task, other, 
         stress, coping, pc_learn, learn_descr, other_descr, everything())

# prepare and clean combined dataframe
# delete individual invalid cases
# ...

# add n_entry for number of consequent entries per code
combined_df_test <- combined_df %>%
  group_by(code) %>%
  arrange((Begin)) %>%
  mutate(n_entry = seq_along(id))


