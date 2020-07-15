# prep data file of first 100 kids enrolled in SAM
# environment should be loaded with data already (refer to weekly report to load in data environment)

prep_ids <- function(N){
  # pull first 100 child IDs
  ids <- pluck(df_list, "eligibility") %>% 
    filter(!is.na(childID)) %>% 
    arrange(start_time) %>% 
    slice_head(n=N) %>% 
    pull(childID)

  # eligibility
  eligibility <- pluck(df_list, "eligibility") %>%
    filter(childID %in% ids) %>%
    select(-c(id, child_firstname, sex, dob_known, child_dob, 
              child_id_number, child_id_barcode, child_id_manual, child_id_verification, start_time,
              treatment_alloc, completed_time, started_time, username, received_on, link))
  
  # baseline
  baseline <- pluck(df_list, "baseline") %>%
    filter(childID %in% ids) %>%
    select(-c(id, csps_fra, gps_csps, child_firstname, sex, Dob_known, child_dob, age_months, 
              child_id_number, child_id_barcode, child_id_manual, child_id_verification, start_time,
              treatment_alloc, completed_time, started_time, username, received_on, link))
  
  # anthro
  anthro <- pluck(df_list, "anthro") %>%
    filter(childID %in% ids & time_point == "baseline") %>%
    mutate(sex2=if_else(sex=="male", "Male", "Female"),
           haz_zscore=who_value2zscore(ageInDays, height, x_var = "agedays", y_var = "htcm", sex = sex2),
           waz_zscore=who_value2zscore(ageInDays, weight, x_var = "agedays", y_var = "wtkg", sex = sex2),
           whz_zscore=
             case_when(
               ageInDays < 730 ~ who_value2zscore(height, weight, x_var = "lencm", y_var = "wtkg", sex = sex2),
               ageInDays >= 730 ~ who_value2zscore(height, weight, x_var = "htcm", y_var = "wtkg", sex = sex2)
             )
    ) %>%
    select(-c(id, csps_fra, child_firstname, sex, Dob_known, child_dob, age_months, 
              child_id_number, child_id_barcode, child_id_manual, child_id_verification, start_time,
              treatment_alloc, completed_time, started_time, username, received_on, link,
              weight_1, weight_2, weight_3, height_1, height_2, height_3, muac_1, muac_2, muac_3, time_point))
  
  # malaria
  malaria <- pluck(df_list, "malaria") %>%
    filter(childID %in% ids) %>%
    select(-c(id, csps_fra, child_firstname, sex, child_dob, age_months, 
              child_id_number, child_id_barcode, child_id_manual, child_id_verification, start_time,
              treatment_alloc, completed_time, started_time, username, received_on, link, time_point))
  
  # rectal
  rectal <- pluck(df_list, "rectal") %>%
    filter(childID %in% ids) %>%
    select(-c(id, csps_fra, child_firstname, sex, child_dob, age_months, dob_known,
              child_id_number, child_id_barcode, child_id_manual, child_id_verification, start_time,
              treatment_alloc, completed_time, started_time, username, received_on, link, sample_id_manual,
              sample_id_verification))
  
  # join together all dataz
  left_join(eligibility,
            baseline,
            by="childID") %>%
    left_join(.,
              anthro,
              by="childID") %>%
    left_join(.,
              malaria,
              by="childID") %>%
    left_join(.,
              rectal,
              by="childID")
}

# run function and save data
prep_ids(100) %>%
  write_csv(., paste0(here::here(), "/monitoring/data/prepped/baseline_100.csv"))

