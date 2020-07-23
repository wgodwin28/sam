#############################################################################
#Functions###################################################################
#############################################################################
# use: drop all interviews specified by ID in incoming list
drop_ints <- function(df_name, df_list, bad_interviews){
  pluck(df_list, df_name) %>%
    filter(id %in% setdiff(id, bad_interviews))
}

# add child name to avoid breaking report validation/duplicate check functions
add_childname <- function(df_name, df_list){
  pluck(df_list, df_name) %>%
    mutate(child_firstname=if (exists("child_firstname", where=.)) as.character(child_firstname) else NA)
}

# make first letter of CSPS variable uppercase
upper_csps <- function(df_name, df_list){
  pluck(df_list, df_name) %>%
    mutate(csps_fra=if (exists("csps_fra", where=.)) str_to_sentence(csps_fra) else NA)
}

# convert to date class
convert_date <- function(df_name, df_list){
  pluck(df_list, df_name) %>%
    mutate(start_time=as.Date(received_on))
}
#############################################################################
#Read in data################################################################
#############################################################################
# dirs <- list.dirs(paste0(here::here(), "/monitoring/data/raw/", date))[2:10]
# form_names <- c("eligibility", "baseline", "anthro", "malaria", "rectal", "treatment", "followup", "discharge", "miss_visit")
# 
# # read each form into large list
# df_list <- map(dirs, function(d){
#   t <- read_csv(paste0(d, "/Forms.csv"))
#   names(t) <- str_remove(names(t), "form.")
#   return(t)
# }) %>%
#   setNames(form_names)

in_dir <- paste0(data_dir, date)
form_names <- c("eligibility", "baseline", "anthro", "malaria", "rectal", "treatment", "followup", "discharge", "miss_visit")

# read each form into large list
df_list <- map(form_names, function(f){
  t <- read_csv(paste0(in_dir, "/", f, ".csv"))
  names(t) <- str_remove(names(t), "form.")
  return(t)
}) %>%
  setNames(form_names)

#############################################################################
#Clean data##################################################################
#############################################################################
# make date class
pluck(df_list, "baseline") %<>%
  mutate(child_dob = as.Date(child_dob))

# change csps variable in eligibility form bc it's the only inconsistent one
pluck(df_list, "eligibility") %<>%
  rename(csps_fra=csps)

# add child name variable if missing
df_list <- names(df_list) %>%
  map(add_childname, df_list) %>%
  setNames(form_names)

# make csps uppercase
df_list <- names(df_list) %>%
  map(upper_csps, df_list) %>%
  setNames(form_names)

# convert date variable to date class
df_list <- names(df_list) %>%
  map(convert_date, df_list) %>%
  setNames(form_names)

#anthro fixes###################################################################
# add average weight and height for anthro measurements
pluck(df_list, "anthro") %<>%
  mutate(weight = (weight_1 + weight_2 + weight_3) / 3,
         height = (height_1 + height_2 + height_3) / 3,
         muac = (muac_1 + muac_2 + muac_3) / 3,
         child_dob = as.Date(child_dob),
         age_months = as.integer(age_months),
         ageInDays = ifelse(Dob_known==1, difftime(as.Date(start_time), child_dob), age_months*30))

# fix muac - some are recorded in cm, some in mm. convert everything >45 to all cm
pluck(df_list, "anthro") %<>%
  mutate(muac=if_else(muac>45, muac/10, muac))

# these interviews were coded to the wrong time point or child ID
pluck(df_list, "anthro") %<>%
  mutate(time_point = ifelse(id=="26f7c048-f591-4f07-a1bc-bc88b69cdfeb", "week_2", time_point),
         time_point = ifelse(id=="32255e9a-69e5-41ce-afce-6d5acc8a7d95", "week_2", time_point),
         time_point = ifelse(id=="0d176cc8-df13-432c-bd92-ffa87abb6334", "week_2", time_point),
         time_point = ifelse(id=="60ace046-cb3c-4669-b339-16c5063efd54", "week_2", time_point),
         time_point = ifelse(id=="7e6437c6-22c7-4b25-a0b8-07acd919c03d", "week_6", time_point),
         childID = ifelse(id=="ac830c88-186a-4072-9aa5-a367dbaa0428", "B383329", childID))

pluck(df_list, "followup") %<>%
  mutate(time_point = ifelse(id=="fd6f806b-1a3e-4323-b2c4-ca95d20eb08a", "week_2", time_point),
         time_point = ifelse(id=="37ec9fd5-5924-450e-a7f5-f08368761231", "week_2", time_point),
         time_point = ifelse(id=="1dd9c262-5128-40fb-ac7b-302802601caf", "week_1", time_point))

# height measurement that's way too high-carry over previous week's height value
pluck(df_list, "anthro") %<>%
  mutate(height = ifelse(id=="75a09b86-fe43-4280-939b-b0f3f46542ee",
                         pluck(df_list, "anthro") %>%
                           filter(id=="a347315a-d502-41f3-91fb-07dcc73a5837") %>%
                           select(height) %>%
                           pull(), 
                         height))

#archive interviews##########################################################
# read in list of interviews to drop
int_IDs <- read_csv(paste0(tab_dir, "incident_log_table.csv")) %>%
  pull(unique(interview_id))

# drop bad or duplicate interviews
df_list <- names(df_list) %>%
  map(drop_ints, df_list, int_IDs) %>%
  setNames(form_names)