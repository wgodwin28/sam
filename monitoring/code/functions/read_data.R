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

# add average weight and height for anthro measurements
pluck(df_list, "anthro") %<>%
  mutate(weight = (weight_1 + weight_2 + weight_3) / 3,
         height = (height_1 + height_2 + height_3) / 3,
         muac = (muac_1 + muac_2 + muac_3) / 3,
         child_dob = as.Date(child_dob),
         age_months = as.integer(age_months),
         ageInDays = ifelse(Dob_known==1, difftime(as.Date(start_time), child_dob), age_months*30))

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

#archive interviews##########################################################
# read in list of interviews to drop
int_IDs <- read_csv(paste0(tab_dir, "incident_log_table.csv")) %>%
  pull(unique(interview_id))

# drop bad or duplicate interviews
df_list <- names(df_list) %>%
  map(drop_ints, df_list, int_IDs) %>%
  setNames(form_names)