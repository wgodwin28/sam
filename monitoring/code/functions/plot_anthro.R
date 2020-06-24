#######################################################################################
##Curves Prep##########################################################################
#######################################################################################
#labels for SD in plot
sd_labels <- factor(c("-3 SD", "-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD", "+3 SD"),
                    levels = c("-3 SD", "-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD", "+3 SD"))
p_frmSD <- c(.2, 2.3, 15.9, 50, 84.1, 97.7, 99.8)

#weight
chartdat_w <- data.frame(SD = rep(sd_labels, each = 1818),
                         Day = rep(8:1825, times = 7),
                         weight_male = unlist(map(p_frmSD, function(p) 
                           who_centile2wtkg(8:1825, p, sex="Male"))),
                         weight_female = unlist(map(p_frmSD, function(p) 
                           who_centile2wtkg(8:1825, p, sex="Female"))))

#height
chartdat_h <- data.frame(SD = rep(sd_labels, each = 1818),
                         Day = rep(8:1825, times = 7),
                         height_male = unlist(map(p_frmSD, function(p) 
                           who_centile2htcm(8:1825, p, sex="Male"))),
                         height_female = unlist(map(p_frmSD, function(p) 
                           who_centile2htcm(8:1825, p, sex="Female"))))

#muac
chartdat_m <- data.frame(SD = rep(sd_labels, each = 1818),
                         Day = rep(8:1825, times = 7),
                         muac_male = unlist(map(p_frmSD, function(p) 
                           who_centile2muaccm(8:1825, p, sex="Male"))),
                         muac_female = unlist(map(p_frmSD, function(p) 
                           who_centile2muaccm(8:1825, p, sex="Female"))))

#weight for height 0-2 years
ht_min <- min(pluck(df_list, "anthro") %>% filter(ageInDays < 730) %>% select(height), na.rm = T) %>% round() - 1
ht_max <- max(pluck(df_list, "anthro") %>% filter(ageInDays < 730) %>% select(height), na.rm = T) %>% round() + 1
chartdat_wfh_2 <- data.frame(SD = rep(sd_labels, each = ht_max-ht_min+1),
                             height = rep(ht_min:ht_max, times = 7),
                             wfh_male = unlist(map(p_frmSD, function(p) 
                               who_centile2value(x = ht_min:ht_max, p = p, x_var = "lencm", y_var = "wtkg", sex = "Male"))),
                             wfh_female = unlist(map(p_frmSD, function(p) 
                               who_centile2value(x = ht_min:ht_max, p = p, x_var = "lencm", y_var = "wtkg", sex = "Female"))))

#weight for height 3-5 years
# ht_min <- min(pluck(df_list, "anthro") %>% filter(ageInDays >= 730) %>% select(height)) %>% round() - 1
# ht_max <- max(pluck(df_list, "anthro") %>% filter(ageInDays >= 730) %>% select(height)) %>% round() + 1
# chartdat_wfh_5 <- data.frame(SD = rep(sd_labels, each = ht_max-ht_min+1),
#                              height = rep(ht_min:ht_max, times = 7),
#                              wfh_male = unlist(map(p_frmSD, function(p) 
#                                who_centile2value(x = ht_min:ht_max, p = p, x_var = "htcm", y_var = "wtkg", sex = "Male"))),
#                              wfh_female = unlist(map(p_frmSD, function(p) 
#                                who_centile2value(x = ht_min:ht_max, p = p, x_var = "htcm", y_var = "wtkg", sex = "Female"))))

##check on zscores outside of -6 and 6
pluck(df_list, "anthro") %<>%
  mutate(sex2=if_else(sex=="male", "Male", "Female"),
         haz_zscore=who_value2zscore(ageInDays, height, x_var = "agedays", y_var = "htcm", sex = sex2),
         waz_zscore=who_value2zscore(ageInDays, weight, x_var = "agedays", y_var = "wtkg", sex = sex2),
         whz_zscore=
           case_when(
             ageInDays < 730 ~ who_value2zscore(height, weight, x_var = "lencm", y_var = "wtkg", sex = sex2),
             ageInDays >= 730 ~ who_value2zscore(height, weight, x_var = "htcm", y_var = "wtkg", sex = sex2)
           )
  )
#######################################################################################
##Baseline Scatter#####################################################################
#######################################################################################
#weight versus age
p_male_wfa <- pluck(df_list, "anthro") %>%
  filter(sex=="male" & weight>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, weight)) +
  geom_line(data=chartdat_w, aes(Day, weight_male, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Males") +
  # ylim(min(dt_list$anthro$weight), max(dt_list$anthro$weight)) +
  xlab("Age (days)") +
  ylab("Weight (kg) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

#weight versus age
p_female_wfa <- pluck(df_list, "anthro") %>%
  filter(sex=="female" & weight>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, weight)) +
  geom_line(data=chartdat_w, aes(Day, weight_female, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Females") +
  xlab("Age (days)") +
  ylab("Weight (kg) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

## Height Versus Age ############################
#scatter height versus age (days)
#height versus age
p_male_hfa <- pluck(df_list, "anthro") %>%
  filter(sex=="male" & height>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, height)) +
  geom_line(data=chartdat_h, aes(Day, height_male, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Males") +
  xlab("Age (days)") +
  ylab("Height (cm) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

#height versus age
p_female_hfa <- pluck(df_list, "anthro") %>%
  filter(sex=="female" & height>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, height)) +
  geom_line(data=chartdat_h, aes(Day, height_female, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Females") +
  xlab("Age (days)") +
  ylab("Height (cm) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))


## Height Versus Weight ############################
#scatter height versus weight (kg) 0-2 years
p_male_wfh_2 <- pluck(df_list, "anthro") %>%
  filter(sex=="male" & height>0 & ageInDays < 730 & time_point=="baseline") %>%
  ggplot(aes(height, weight)) +
  geom_line(data=chartdat_wfh_2, aes(height, wfh_male, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Males 0-2 years") +
  xlab("Height (cm) at enrollment") +
  ylab("Weight (kg) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

#height versus weight 0-2 years
p_female_wfh_2 <- pluck(df_list, "anthro") %>%
  filter(sex=="female" & height>0 & weight>0 & ageInDays < 730 & time_point=="baseline") %>%
  ggplot(aes(height, weight)) +
  geom_line(data=chartdat_wfh_2, aes(height, wfh_female, color=SD)) +
  geom_point() +
  theme_bw() +
  ggtitle("Females 0-2 years") +
  xlab("Height (cm) at enrollment") +
  ylab("Weight (kg) at enrollment") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

# MUAC versus age ############################
#males
p_male_mfa <- pluck(df_list, "anthro") %>%
  filter(sex=="male" & muac>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, muac)) +
  geom_line(data=chartdat_m, aes(Day, muac_male, color=SD)) +
  geom_hline(yintercept=c(11.5), linetype="dotted", color="red") +
  # annotate("text", x=1500, y=11.2, label="Referral threshold") +
  geom_point() +
  theme_bw() +
  ggtitle("Males") +
  xlab("Age (days)") +
  ylab("MUAC (cm)") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

#female
p_female_mfa <- pluck(df_list, "anthro") %>%
  filter(sex=="female" & muac>0 & time_point=="baseline") %>%
  ggplot(aes(ageInDays, muac)) +
  geom_line(data=chartdat_m, aes(Day, muac_female, color=SD)) +
  geom_hline(yintercept=c(11.5), linetype="dotted", color="red") +
  geom_point() +
  theme_bw() +
  ggtitle("Females") +
  xlab("Age (days)") +
  ylab("MUAC (cm)") +
  scale_color_discrete(name="WHO Standard") +
  guides(color = guide_legend(reverse = TRUE))

#######################################################################################
##Growth curve over age################################################################
#######################################################################################
## Height versus Age ############################
ids_anthcurve <- pluck(df_list, "anthro") %>%
  filter(time_point=="week_1") %>% distinct(childID) %>% pull

#males
p_male_grow_h <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>%
  select(childID, height, time_point, ageInDays) %>%
  ggplot() +
  geom_line(data=chartdat_h, aes(Day, height_male, color=SD)) +
  geom_point(aes(ageInDays, height, shape=time_point)) +
  geom_line(aes(ageInDays, height, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("Height (cm)") +
  ggtitle("Males")

#females
p_female_grow_h <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>% 
  select(childID, height, time_point, ageInDays) %>%
  ggplot() +
  geom_line(data=chartdat_h, aes(Day, height_female, color=SD)) +
  geom_point(aes(ageInDays, height, shape=time_point)) +
  geom_line(aes(ageInDays, height, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("Height (cm)") +
  ggtitle("Females")

## Weight versus Age ############################
#males
p_male_grow_w <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>% 
  select(childID, weight, time_point, ageInDays) %>%
  ggplot() +
  geom_line(data=chartdat_w, aes(Day, weight_male, color=SD)) +
  geom_point(aes(ageInDays, weight, shape=time_point)) +
  geom_line(aes(ageInDays, weight, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("Weight (kg)") +
  ggtitle("Males")

#females
p_female_grow_w <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>% 
  select(childID, weight, time_point, ageInDays) %>%
  ggplot() +
  geom_line(data=chartdat_w, aes(Day, weight_female, color=SD)) +
  geom_point(aes(ageInDays, weight, shape=time_point)) +
  geom_line(aes(ageInDays, weight, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("Weight (kg)") +
  ggtitle("Females")

## MUAC versus Age ############################
#males
p_male_grow_m <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>% 
  select(childID, muac, time_point, ageInDays) %>%
  ggplot() +
  geom_line(data=chartdat_m, aes(Day, muac_male, color=SD)) +
  geom_point(aes(ageInDays, muac, shape=time_point)) +
  geom_line(aes(ageInDays, muac, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("MUAC (cm)") +
  ggtitle("Males")

#females
p_female_grow_m <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>% 
  select(childID, muac, time_point, ageInDays, Dob_known) %>%
  ggplot() +
  geom_line(data=chartdat_m, aes(Day, muac_female, color=SD)) +
  geom_point(aes(ageInDays, muac, shape=time_point)) +
  geom_line(aes(ageInDays, muac, group=childID)) +
  theme_bw() +
  xlab("Age (days)") +
  ylab("MUAC (cm)") +
  ggtitle("Females")


#######################################################################################
##Growth curve over study period#######################################################
#######################################################################################
# make months variable
pluck(df_list, "anthro") %<>%
  mutate(Months=cut(ageInDays,
                    breaks = seq(1, 1825, length.out=11),
                    labels = c("0-6", "6-12", "12-18", "18-24", "24-30", "30-36", "36-42", "42-48", "48-54", "54-60")))

# plot haz
p_male_study_haz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>%
  select(childID, haz_zscore, time_point, ageInDays, Months) %>%
  ggplot() +
  geom_point(aes(time_point, haz_zscore, group=childID)) +
  geom_line(aes(time_point, haz_zscore, group=childID, color=Months)) +
  theme_bw() +
  scale_color_viridis_d(direction=-1) +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("HAZ-score") +
  ggtitle("Males")

#convert to plotly
p_male_study_haz <- if(make_plotly) ggplotly(p_male_study_haz) else p_male_study_haz

# plot haz for girls
p_female_study_haz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>%
  select(childID, haz_zscore, time_point, ageInDays, Months) %>%
  ggplot() +
  geom_point(aes(time_point, haz_zscore, group=childID)) +
  geom_line(aes(time_point, haz_zscore, group=childID, color=Months)) +
  theme_bw() +
  scale_color_viridis_d(direction=-1) +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("HAZ-score") +
  ggtitle("Females")

#convert to plotly
p_female_study_haz <- if(make_plotly) ggplotly(p_female_study_haz) else p_female_study_haz

# plot waz
p_male_study_waz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>%
  select(childID, waz_zscore, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, waz_zscore, group=childID, text=id)) +
  geom_line(aes(time_point, waz_zscore, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  #scale_color_manual(values=wes_palette(name="GrandBudapest2")) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("WAZ-score") +
  ggtitle("Males")

#convert to plotly
p_male_study_waz <- if(make_plotly) ggplotly(p_male_study_waz) else p_male_study_waz

# plot waz for girls
p_female_study_waz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>%
  select(childID, waz_zscore, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, waz_zscore, group=childID, text=id)) +
  geom_line(aes(time_point, waz_zscore, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  #scale_color_manual(values=wes_palette(name="GrandBudapest2")) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("WAZ-score") +
  ggtitle("Females")

#convert to plotly
p_female_study_waz <- if(make_plotly) ggplotly(p_female_study_waz) else p_female_study_waz

# plot whz
p_male_study_whz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>%
  select(childID, whz_zscore, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, whz_zscore, group=childID, text=id)) +
  geom_line(aes(time_point, whz_zscore, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("WHZ-score") +
  ggtitle("Males")

#convert to plotly
p_male_study_whz <- if(make_plotly) ggplotly(p_male_study_whz) else p_male_study_whz

# plot waz for girls
p_female_study_whz <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>%
  select(childID, whz_zscore, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, whz_zscore, group=childID, text=id)) +
  geom_line(aes(time_point, whz_zscore, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("WHZ-score") +
  ggtitle("Females")

#convert to plotly
p_female_study_whz <- if(make_plotly) ggplotly(p_female_study_whz) else p_female_study_whz

# muac males
p_male_study_muac <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "male") %>%
  select(childID, muac, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, muac, group=childID, text=id)) +
  geom_line(aes(time_point, muac, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("MUAC") +
  ggtitle("Males")

#convert to plotly
p_male_study_muac <- if(make_plotly) ggplotly(p_male_study_muac) else p_male_study_muac

# muac males
p_female_study_muac <- pluck(df_list, "anthro") %>%
  filter(childID %in% ids_anthcurve & sex == "female") %>%
  select(childID, muac, time_point, ageInDays, Months, id) %>%
  ggplot() +
  geom_point(aes(time_point, muac, group=childID, text=id)) +
  geom_line(aes(time_point, muac, group=childID, color=Months)) +
  scale_color_viridis_d(direction=-1) +
  theme_bw() +
  labs(col="Age (months)") +
  xlab("Visit") +
  ylab("MUAC") +
  ggtitle("Females")

#convert to plotly
p_female_study_muac <- if(make_plotly) ggplotly(p_female_study_muac) else p_female_study_muac
