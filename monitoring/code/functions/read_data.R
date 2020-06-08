#############################################################################
#Read in data################################################################
#############################################################################
date <- format(Sys.Date(), "%d%b%Y")
dirs <- list.dirs(paste0(here::here(), "/monitoring/data/raw/", date))[2:10]
forms <- c("eligibility", "baseline", "anthro", "malaria", "rectal", "treatment", "followup", "discharge", "missVisit")

# read each form into large list
df_list <- map(dirs, function(d){
  t <- read_csv(paste0(d, "/Forms.csv"))
  names(t) <- str_remove(names(t), "form.")
  return(t)
}) %>%
  setNames(forms)

# add average weight and height for anthro measurements
pluck(df_list, "anthro") %<>%
  mutate(weight = (weight_1 + weight_2 + weight_3) / 3,
         height = (height_1 + height_2 + height_3) / 3,
         muac = (muac_1 + muac_2 + muac_3) / 3,
         child_dob = as.Date(child_dob),
         age_months = as.integer(age_months),
         ageInDays = ifelse(Dob_known==1, difftime(Sys.Date(), child_dob), age_months*30))

pluck(df_list, "anthro") %>%
  mutate(age_months = as.integer(age_months))