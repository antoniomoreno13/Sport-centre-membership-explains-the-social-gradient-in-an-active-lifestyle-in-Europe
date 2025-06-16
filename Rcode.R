
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("MASS")
library("sfsmisc")
library("QuantPsyc")
library("forcats")
library("lmtest")
library("reshape2")


#### Data Handling ####
# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224,222, 277, 242,291, 236, 237:240, 270, 11, 59:61, 98:100)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:27] <- c("Gender", "Age","Marital status", "Social class subjective", "Type of community", "Sizeofcommunity" ,"Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Ocupation_last_job", "Bills", "Country")
eu2017["survey"] <- "2017" # TIME VARIABLE
names(eu2017)[29:34] <- c("Fitness_centre_venue", "Sport_club_venue","Sport_centre_venue", "Fitness_centre_membership", "Sport_club_membership", "Sociocultural_club_membership")


eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")



#### IPAQ, VPA, MPA, MVPA calculation in METs (and Time per Week) ####

eu2017$Vig_Days <- as.numeric(eu2017$Vig_Days) # TO CONTINIOUS
eu2017$Mod_Days <- as.numeric(eu2017$Mod_Days)
eu2017$Walk_Days <- as.numeric(eu2017$Walk_Days)

eu2017$Vig_Days[which(is.na(eu2017$Vig_Days))] <- 0 # RECODING AND  MISSING VALUES RECOVERY
eu2017$Vig_Days[eu2017$Vig_Days == 8] <- 0
eu2017$Vig_Days[eu2017$Vig_Days == 9] <- NA
eu2017$Vig_Time[which(is.na(eu2017$Vig_Time))] <- "Never do any vigorous physical activity "
eu2017$Vig_Time[eu2017$Vig_Time == "DK"] <- NA

eu2017$Mod_Days[which(is.na(eu2017$Mod_Days))] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 8] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 9] <- NA
eu2017$Mod_Time[which(is.na(eu2017$Mod_Time))] <- "Never do any moderate physical activity "
eu2017$Mod_Time[eu2017$Mod_Time == "DK"] <- NA

eu2017$Walk_Days[eu2017$Walk_Days == 8] <- 0
eu2017$Walk_Days[eu2017$Walk_Days == 9] <- NA
eu2017$Walk_Time[eu2017$Walk_Time == "DK"] <- NA

# INTERVAL MEDIAN VALUES BY PA TIME      
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "Never do any vigorous physical activity " | eu2017$Vig_Time  == "Never do vigorous physical activities"] <- 0 
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "30 minutes or less" ] <- 15   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "More than 120 minutes" ] <- 135   

eu2017$Mod_Time_med [ eu2017$Mod_Time  == "Never do any moderate physical activity " | eu2017$Mod_Time  == "Never do moderate physical activities"] <- 0 
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "30 minutes or less" ] <- 15   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "More than 120 minutes" ] <- 135  

eu2017$Walk_Time_med [eu2017$Walk_Time == "Never walk for 10 minutes at a time"] <- 0
eu2017$Walk_Time_med [eu2017$Walk_Time == "30 minutes or less"] <- 15
eu2017$Walk_Time_med [eu2017$Walk_Time == "31 to 60 minutes"] <- 45
eu2017$Walk_Time_med [eu2017$Walk_Time == "61 to 90 minutes"] <- 75
eu2017$Walk_Time_med [eu2017$Walk_Time == "91 to 120 minutes"] <- 105
eu2017$Walk_Time_med [eu2017$Walk_Time == "More than 120 minutes"] <- 135


# TIME PER WEEK
eu2017$VPA_tot_time <- eu2017$Vig_Days * eu2017$Vig_Time_med
eu2017$MPA_tot_time <- eu2017$Mod_Days * eu2017$Mod_Time_med
eu2017$Walk_tot_time <- eu2017$Walk_Days * eu2017$Walk_Time_med

eu2017$Mod_plus_walk_tottime <- eu2017$MPA_tot_time + eu2017$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

# METS PER WEEK
eu2017$VPA_met <- eu2017$VPA_tot_time * 8
eu2017$MPA_met <- eu2017$MPA_tot_time * 4
eu2017$Walk_met <- eu2017$Walk_tot_time * 3.3 

# COMPUTE MVPA METs 

eu2017$MVPA_met <- eu2017$VPA_met + eu2017$MPA_met + eu2017$Walk_met
eu2017$MVPA_met_outwalk <- eu2017$VPA_met + eu2017$MPA_met

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA)

eu2017$WHO_prev [eu2017$VPA_tot_time >= 75 | eu2017$Mod_plus_walk_tottime >= 150 | (eu2017$Mod_plus_walk_tottime + (2 * eu2017$VPA_tot_time) >= 150)] <- "Active"
eu2017$WHO_prev [which(is.na(eu2017$WHO_prev))] <-"Inactive"
eu2017$WHO_prev [which(is.na(eu2017$Mod_plus_walk_tottime) & (is.na(eu2017$VPA_tot_time)))] <- NA


#### Covariates Reordering, Assigning missing values and dropping useless levels ####
eu2017$Bills[eu2017$Bills == "Refusal (SPONT.)"] <- NA; eu2017$Bills<- fct_drop(eu2017$Bills, only = "Refusal (SPONT.)")

eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Towns and suburbs/ small urban area"] <- "Urban"; eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Cities/ large urban area"] <- "Urban"; 
eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Rural area"] <- "Rural"

eu2017$Age_3clusters [ eu2017$Age >= 18 & eu2017$Age < 45 ] <- "18-44"  
eu2017$Age_3clusters [ eu2017$Age >= 45 & eu2017$Age < 70 ] <- "45-69"  
eu2017$Age_3clusters [ eu2017$Age >= 70] <- "70+"  

eu2017$Age_3clusters2 [ eu2017$Age >= 18 & eu2017$Age < 35 ] <- "18-34"  
eu2017$Age_3clusters2 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters2 [ eu2017$Age >= 65] <- "65+"  

eu2017$Age_3clusters3 [ eu2017$Age < 35 ] <- "15-34"  
eu2017$Age_3clusters3 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters3 [ eu2017$Age >= 65] <- "65+"

eu2017$`Marital status`[eu2017$`Marital status` == "Other (SPONT.)"] <- NA
eu2017$`Marital status`[eu2017$`Marital status` == "Refusal (SPONT.)"] <- NA
eu2017$`Marital status`<- fct_drop(eu2017$`Marital status`, only = c("Refusal (SPONT.)", "Other (SPONT.)"))

eu2017$Education[eu2017$Education == "DK"] <- NA
eu2017$Education[eu2017$Education == "Refusal_duplicated_7"] <- NA
eu2017$Education[eu2017$Education == "Refusal"] <- NA
eu2017$Education[eu2017$Education == "No full-time education"] <- NA
eu2017$Education <- fct_drop(eu2017$Education, only = c("DK", "Refusal_duplicated_7", "Refusal", "No full-time education"))

eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age <= 15] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age > 15 & eu2017$Age < 20] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age >= 20] <- "20 years and older"
eu2017$Education_3cat[eu2017$Education == "Up to 15 years"] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "16-19"] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "20 years and older"] <- "20 years and older"

eu2017$Education_3cat_recod[eu2017$Education_3cat == "Up to 15 years"] <- "Primary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "16-19"] <- "Secondary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "20 years and older"] <- "University"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Supervisor" | eu2017$Ocupation == "Skilled manual worker" | eu2017$Ocupation == "Unskilled manual worker, etc." | eu2017$Ocupation == "Employed position, service job"] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Farmer" | eu2017$Ocupation == "Fisherman" |  eu2017$Ocupation == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation == "Business proprietors, etc." | eu2017$Ocupation == "Employed position, at desk" | eu2017$Ocupation == "Employed position, travelling" | eu2017$Ocupation == "Professional (lawyer, etc.)"] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Employed professional (employed doctor, etc.)"  |  eu2017$Ocupation == "General management, etc."  | eu2017$Ocupation == "Middle management, etc." ] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" | eu2017$Ocupation == "Responsible for ordinary shopping, etc."] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work"] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student"] <- NA

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA


eu2017$MVPA_tot_time_outwalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time
eu2017$MVPA_tot_time_pluswalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time + eu2017$Walk_tot_time

eu2017$Age_6clusters [ eu2017$Age >= 15 & eu2017$Age < 25 ] <- "15-24"  
eu2017$Age_6clusters [ eu2017$Age >= 25 & eu2017$Age < 35 ] <- "25-34"  
eu2017$Age_6clusters [ eu2017$Age >= 35 & eu2017$Age < 45 ] <- "35-44"  
eu2017$Age_6clusters [ eu2017$Age >= 45 & eu2017$Age < 55 ] <- "45-54"  
eu2017$Age_6clusters [ eu2017$Age >= 55 & eu2017$Age < 65 ] <- "55-64"  
eu2017$Age_6clusters [ eu2017$Age >= 65 ] <- "65+"  
eu2017$Age_6clusters <- as.factor(eu2017$Age_6clusters)


eu2017$Sport_centre_venue[which(is.na(eu2017$Sport_centre_venue))] <- "Not mentioned" # RECODING AND  MISSING VALUES RECOVERY
eu2017$Sport_club_venue[which(is.na(eu2017$Sport_club_venue))] <- "Not mentioned" # RECODING AND  MISSING VALUES RECOVERY
eu2017$Fitness_centre_venue[which(is.na(eu2017$Fitness_centre_venue))] <- "Not mentioned" # RECODING AND  MISSING VALUES RECOVERY


eu2017$ALL_Sport_centres_venue [eu2017$Sport_centre_venue == "At a sport centre " | eu2017$Sport_club_venue == "At a sport club "
                                | eu2017$Fitness_centre_venue == "At a health or fitness centre"] <- "At a sport centre"

eu2017$ALL_Sport_centres_venue [eu2017$Sport_centre_venue == "Not mentioned" & eu2017$Sport_club_venue == "Not mentioned"
                & eu2017$Fitness_centre_venue == "Not mentioned"] <- "Not mentioned"
eu2017$ALL_Sport_centres_venue <- as.factor(eu2017$ALL_Sport_centres_venue)

eu2017[48:59] <- lapply(eu2017[48:59], factor) 


#### Filters ####
# REMOVE ILLOGICAL VALUES AND RECOVERING SOME NAs LOGICAL VALUES
eu2017_excluded <- eu2017[c(which((eu2017$Vig_Days == 0 & eu2017$Vig_Time_med > 0) | (eu2017$Mod_Days == 0 & eu2017$Mod_Time_med > 0) | (eu2017$Walk_Days == 0 & eu2017$Walk_Time_med > 0) | (eu2017$Vig_Days > 0 & eu2017$Vig_Time_med == 0) | (eu2017$Mod_Days > 0 & eu2017$Mod_Time_med == 0) | (eu2017$Walk_Days > 0 & eu2017$Walk_Time_med == 0))), ] 
eu2017_excluded <- eu2017_excluded[which(eu2017_excluded$MVPA_met >= 0), ]
eu2017_excluded <- eu2017_excluded[which(eu2017_excluded$Age >= 18), ]

eu2017 <- eu2017[-c(which(eu2017$Vig_Days == 0 & eu2017$Vig_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days == 0 & eu2017$Mod_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days == 0 & eu2017$Walk_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Vig_Days > 0 & eu2017$Vig_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days > 0 & eu2017$Mod_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days > 0 & eu2017$Walk_Time_med == 0)), ] 

# REMOVING NA METS AND SIT TIME VALUES TO GET FINAL DATABASE SAMPLE SIZE
eu2017 <- eu2017[which(eu2017$MVPA_met >= 0), ]
eu2017 <- eu2017[which(eu2017$Age >= 18), ]

eu2017_sport_centre <- eu2017[which(eu2017$ALL_Sport_centres_venue == "At a sport centre"), ]
eu2017_active <- eu2017[which(eu2017$WHO_prev == "Active"), ]


#### Descriptive statistics ####
euMen <- eu2017[which(eu2017$Gender == "Man"),]
euWomen <- eu2017[which(eu2017$Gender == "Woman"),]


eu2017$MVPA_tot_time_pluswalk_2vig <- (2*eu2017$VPA_tot_time) + eu2017$MPA_tot_time + eu2017$Walk_tot_time
eu2017_sport_centre$MVPA_tot_time_pluswalk_2vig <- (2*eu2017_sport_centre$VPA_tot_time) + eu2017_sport_centre$MPA_tot_time + eu2017_sport_centre$Walk_tot_time
eu2017_active$MVPA_tot_time_pluswalk_2vig <- (2*eu2017_active$VPA_tot_time) + eu2017_active$MPA_tot_time + eu2017_active$Walk_tot_time
eu2017_active$WHO_prev2 [(eu2017_active$Mod_plus_walk_tottime + (2 * eu2017_active$VPA_tot_time) >= 150)] <- "Mod active"
eu2017_active$WHO_prev2 [(eu2017_active$Mod_plus_walk_tottime + (2 * eu2017_active$VPA_tot_time) >= 300)] <- "High active"
eu2017_active$WHO_prev2 <- as.factor(eu2017_active$WHO_prev2)

eu2017$WHO_prev2 [(eu2017$Mod_plus_walk_tottime + (2 * eu2017$VPA_tot_time) >= 150)] <- "Mod active"
eu2017$WHO_prev2 [(eu2017$Mod_plus_walk_tottime + (2 * eu2017$VPA_tot_time) >= 300)] <- "High active"
eu2017$WHO_prev2 <- as.factor(eu2017$WHO_prev2)

eu2017_sport_centre$WHO_prev2 [(eu2017_sport_centre$Mod_plus_walk_tottime + (2 * eu2017_sport_centre$VPA_tot_time) >= 150)] <- "Mod active"
eu2017_sport_centre$WHO_prev2 [(eu2017_sport_centre$Mod_plus_walk_tottime + (2 * eu2017_sport_centre$VPA_tot_time) >= 300)] <- "High active"
eu2017_sport_centre$WHO_prev2 <- as.factor(eu2017_sport_centre$WHO_prev2)



## Simple ##
table(eu2017$Gender);prop.table(table(eu2017$Gender))
table(eu2017$Sizeofcommunity);prop.table(table(eu2017$Sizeofcommunity))
table(eu2017$Age_3clusters2);prop.table(table(eu2017$Age_3clusters2))
table(eu2017$`Marital status`);prop.table(table(eu2017$`Marital status`))

table(eu2017$Education_3cat_recod);prop.table(table(eu2017$Education_3cat_recod))
table(eu2017$Ocupation_3cat);prop.table(table(eu2017$Ocupation_3cat))
table(eu2017$Bills);prop.table(table(eu2017$Bills))

table(eu2017$ALL_Sport_centres_venue);prop.table(table(eu2017$ALL_Sport_centres_venue))

mean(eu2017$Age); sd(eu2017$Age)
mean(eu2017$MVPA_tot_time_pluswalk_2vig); sd(eu2017$MVPA_tot_time_pluswalk_2vig)
mean(eu2017$MVPA_met); sd(eu2017$MVPA_met)
table(eu2017$WHO_prev);prop.table(table(eu2017$WHO_prev))
table(eu2017$WHO_prev2);prop.table(table(eu2017$WHO_prev2))


## ACROSS SOME VARIABLES ##
eu2017 %>% group_by(Education_3cat_recod) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> educ_desc_ALL


eu2017 %>% group_by(Ocupation_3cat) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> ocup_desc_ALL

eu2017 %>% group_by(Bills) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> bills_desc_ALL

eu2017 %>% group_by(ALL_Sport_centres_venue) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> all_sport_venue_desc_ALL


eu2017 %>% group_by(Education_3cat_recod, ALL_Sport_centres_venue) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> educ_sport_venue_desc_ALL

eu2017 %>% group_by(Ocupation_3cat, ALL_Sport_centres_venue) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> ocup_sport_venue_desc_ALL

eu2017 %>% group_by(Bills, ALL_Sport_centres_venue) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk_2vig), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk_2vig), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk_2vig, 0.75)) -> bills_sport_venue_desc_ALL



library(descr)

crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Gender, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Sizeofcommunity, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Age_3clusters2, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$`Marital status`, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Education_3cat_recod, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Bills, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$ALL_Sport_centres_venue, eu2017$Ocupation_3cat, prop.c = T,digits = 2, chisq = F, plot = F)


crosstab(eu2017$WHO_prev, eu2017$Gender, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$WHO_prev, eu2017$Sizeofcommunity, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$WHO_prev, eu2017$Age_3clusters2, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$WHO_prev, eu2017$`Marital status`, prop.c = T,digits = 2, chisq = T, plot = F)
crosstab(eu2017$WHO_prev, eu2017$Education_3cat_recod, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$WHO_prev, eu2017$Bills, prop.c = T,digits = 2, chisq = F, plot = F)
crosstab(eu2017$WHO_prev, eu2017$Ocupation_3cat, prop.c = T,digits = 2, chisq = F, plot = F)


eu2017 %>%  group_by(ALL_Sport_centres_venue) %>% 
    summarise(n = n(), 
              mean_age = mean(Age, na.rm = T),
              sd_age = sd(Age, na.rm = T))

eu2017 %>%  group_by(WHO_prev) %>% 
    summarise(n = n(), 
              mean_age = mean(Age, na.rm = T),
              sd_age = sd(Age, na.rm = T))



#### Binomial logistic regression ####
library("moments")
library(sjPlot)
set.seed(123456)

eu2017$Gender <- relevel(eu2017$Gender, ref = "Woman")
eu2017$Sizeofcommunity <- relevel(eu2017$Sizeofcommunity, ref = "Rural area")
eu2017$Education_3cat_recod <- relevel(eu2017$Education_3cat_recod, ref = "Primary")
eu2017$Ocupation_3cat <- relevel(eu2017$Ocupation_3cat, ref = "V-VII")
eu2017$Bills <- relevel(eu2017$Bills, ref = "Most of the time")
eu2017$WHO_prev <- relevel(eu2017$WHO_prev, ref = "Inactive")
eu2017$ALL_Sport_centres_venue <- relevel(eu2017$ALL_Sport_centres_venue, ref = "Not mentioned")
eu2017$`Marital status` <- relevel(eu2017$`Marital status`, ref = "Single hh without children (9,11,13 in d7)")

model1a <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` + Education_3cat_recod, data = eu2017, family = binomial, na.action = na.exclude)
model1b <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` +  Education_3cat_recod + ALL_Sport_centres_venue, data = eu2017, family = binomial, na.action = na.exclude)

model2a <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` + Ocupation_3cat, data = eu2017, family = binomial, na.action = na.exclude)
model2b <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` + Ocupation_3cat + ALL_Sport_centres_venue, data = eu2017, family = binomial, na.action = na.exclude)

model3a <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` + Bills, data = eu2017, family = binomial, na.action = na.exclude)
model3b <- glm(WHO_prev ~ Age + Gender + Sizeofcommunity + `Marital status` + Bills + ALL_Sport_centres_venue, data = eu2017, family = binomial, na.action = na.exclude)

tab_model(model1a, model1b, model2a, model2b, model3a, model3b, digits.re = 3)



#### Mediation analyses with lavaan ####
library(lavaan)

densityPlot(eu2017$MVPA_tot_time_pluswalk_2vig)

eu2017$Gender_cont [ eu2017$Gender  == "Woman" ] <- 0
eu2017$Gender_cont [ eu2017$Gender  == "Man" ] <- 1

eu2017$Sizeofcommunity_cont [ eu2017$Sizeofcommunity  == "Rural area" ] <- 0
eu2017$Sizeofcommunity_cont [ eu2017$Sizeofcommunity  == "Towns and suburbs/ small urban area" ] <- 1
eu2017$Sizeofcommunity_cont [ eu2017$Sizeofcommunity  == "Cities/ large urban area" ] <- 2

eu2017$ALL_Sport_centres_venue_cont [ eu2017$ALL_Sport_centres_venue  == "Not mentioned" ] <- 0
eu2017$ALL_Sport_centres_venue_cont [ eu2017$ALL_Sport_centres_venue  == "At a sport centre" ] <- 1

eu2017$Bills_cont [ eu2017$Bills  == "Most of the time" ] <- 0
eu2017$Bills_cont [ eu2017$Bills  == "From time to time" ] <- 1
eu2017$Bills_cont [ eu2017$Bills  == "Almost never/never" ] <- 2

eu2017$Ocupation_3cat_cont [ eu2017$Ocupation_3cat  == "V-VII" ] <- 0
eu2017$Ocupation_3cat_cont [ eu2017$Ocupation_3cat  == "III-IV" ] <- 1
eu2017$Ocupation_3cat_cont [ eu2017$Ocupation_3cat  == "I-II" ] <- 2

eu2017$Education_3cat_recod_cont [ eu2017$Education_3cat_recod  == "Primary" ] <- 0
eu2017$Education_3cat_recod_cont [ eu2017$Education_3cat_recod  == "Secondary" ] <- 1
eu2017$Education_3cat_recod_cont [ eu2017$Education_3cat_recod  == "University" ] <- 2

eu2017$WHO_prev_cont [ eu2017$WHO_prev  == "Inactive" ] <- 0
eu2017$WHO_prev_cont [ eu2017$WHO_prev  == "Active" ] <- 1

eu2017$Marital_status_cont [ eu2017$`Marital status`  == "Single hh without children (9,11,13 in d7)" ] <- 0
eu2017$Marital_status_cont [ eu2017$`Marital status`  == "Single hh with children (10,12,14 in d7)" ] <- 1
eu2017$Marital_status_cont [ eu2017$`Marital status`  == "Multiple hh without children (1, 5 in d7)" ] <- 2
eu2017$Marital_status_cont [ eu2017$`Marital status`  == "Multiple hh with children (2-4, 6-8 in d7)" ] <- 3



set.seed(123456)
specmod_educ <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont
MVPA_tot_time_pluswalk_2vig ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'


fitmod_educ <- sem(specmod_educ, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ, fit.measures=T, rsquare=T, ci=T)



specmod_ocup <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont 
MVPA_tot_time_pluswalk_2vig ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup <- sem(specmod_ocup, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup, fit.measures=T, rsquare=T, standardized=T, ci=T)





specmod_bills <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont 
MVPA_tot_time_pluswalk_2vig ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills <- sem(specmod_bills, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills, fit.measures=T, rsquare=T, standardized=T, ci=T)




# Sensitive analyses with ACTIVE & VENUE
specmod_educ2 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ2 <- sem(specmod_educ2, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ2, fit.measures=T, rsquare=T, standardized=T, ci=T)



specmod_ocup2 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup2 <- sem(specmod_ocup2, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup2, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills2 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills2 <- sem(specmod_bills2, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills2, fit.measures=T, rsquare=T, standardized=T, ci=T)





# Sensitive analyses with Physical activity quartiles & VENUE
quantile(eu2017$MVPA_tot_time_pluswalk_2vig, na.rm = T)
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig <= 105] <- 0
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 105 & eu2017$MVPA_tot_time_pluswalk_2vig <= 315] <- 1
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 315 & eu2017$MVPA_tot_time_pluswalk_2vig <= 735] <- 2
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 735] <- 3


specmod_educ3 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ3 <- sem(specmod_educ3, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ3, fit.measures=T, rsquare=T, standardized=T, ci=T)




specmod_ocup3 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup3 <- sem(specmod_ocup3, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup3, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills3 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills3 <- sem(specmod_bills3, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills3, fit.measures=T, rsquare=T, standardized=T, ci=T)







# Analyses with Walking & VENUE
specmod_educ4 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ4 <- sem(specmod_educ4, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ4, fit.measures=T, rsquare=T, standardized=T, ci=T)




specmod_ocup4 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup4 <- sem(specmod_ocup4, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup4, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills4 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills4 <- sem(specmod_bills4, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills4, fit.measures=T, rsquare=T, standardized=T, ci=T)


#### Supplementary analyses ####
# excluded sample
table(eu2017_excluded$Gender);prop.table(table(eu2017_excluded$Gender))
table(eu2017_excluded$Sizeofcommunity);prop.table(table(eu2017_excluded$Sizeofcommunity))
table(eu2017_excluded$Age_3clusters2);prop.table(table(eu2017_excluded$Age_3clusters2))
table(eu2017_excluded$`Marital status`);prop.table(table(eu2017_excluded$`Marital status`))

table(eu2017_excluded$Education_3cat_recod);prop.table(table(eu2017_excluded$Education_3cat_recod))
table(eu2017_excluded$Ocupation_3cat);prop.table(table(eu2017_excluded$Ocupation_3cat))
table(eu2017_excluded$Bills);prop.table(table(eu2017_excluded$Bills))

table(eu2017_excluded$Sport_centre_venue);prop.table(table(eu2017_excluded$Sport_centre_venue))
table(eu2017_excluded$Sport_club_venue);prop.table(table(eu2017_excluded$Sport_club_venue))
table(eu2017_excluded$Fitness_centre_venue);prop.table(table(eu2017_excluded$Fitness_centre_venue))
table(eu2017_excluded$ALL_Sport_centres_venue);prop.table(table(eu2017_excluded$ALL_Sport_centres_venue))

table(eu2017_excluded$Sociocultural_club_membership);prop.table(table(eu2017_excluded$Sociocultural_club_membership))
table(eu2017_excluded$Sport_club_membership);prop.table(table(eu2017_excluded$Sport_club_membership))
table(eu2017_excluded$Fitness_centre_membership);prop.table(table(eu2017_excluded$Fitness_centre_membership))
table(eu2017_excluded$ALL_Sport_centres_membership);prop.table(table(eu2017_excluded$ALL_Sport_centres_membership))

mean(eu2017_excluded$Age); sd(eu2017_excluded$Age)


# profile of those using sport centres
table(eu2017_sport_centre$Gender);prop.table(table(eu2017_sport_centre$Gender))
table(eu2017_sport_centre$Sizeofcommunity);prop.table(table(eu2017_sport_centre$Sizeofcommunity))
table(eu2017_sport_centre$Age_3clusters2);prop.table(table(eu2017_sport_centre$Age_3clusters2))
table(eu2017_sport_centre$`Marital status`);prop.table(table(eu2017_sport_centre$`Marital status`))

table(eu2017_sport_centre$Education_3cat_recod);prop.table(table(eu2017_sport_centre$Education_3cat_recod))
table(eu2017_sport_centre$Ocupation_3cat);prop.table(table(eu2017_sport_centre$Ocupation_3cat))
table(eu2017_sport_centre$Bills);prop.table(table(eu2017_sport_centre$Bills))

table(eu2017_sport_centre$Sport_centre_venue);prop.table(table(eu2017_sport_centre$Sport_centre_venue))
table(eu2017_sport_centre$Sport_club_venue);prop.table(table(eu2017_sport_centre$Sport_club_venue))
table(eu2017_sport_centre$Fitness_centre_venue);prop.table(table(eu2017_sport_centre$Fitness_centre_venue))
table(eu2017_sport_centre$ALL_Sport_centres_venue);prop.table(table(eu2017_sport_centre$ALL_Sport_centres_venue))

table(eu2017_sport_centre$Sociocultural_club_membership);prop.table(table(eu2017_sport_centre$Sociocultural_club_membership))
table(eu2017_sport_centre$Sport_club_membership);prop.table(table(eu2017_sport_centre$Sport_club_membership))
table(eu2017_sport_centre$Fitness_centre_membership);prop.table(table(eu2017_sport_centre$Fitness_centre_membership))
table(eu2017_sport_centre$ALL_Sport_centres_membership);prop.table(table(eu2017_sport_centre$ALL_Sport_centres_membership))

mean(eu2017_sport_centre$Age); sd(eu2017_sport_centre$Age)
mean(eu2017_sport_centre$MVPA_tot_time_pluswalk_2vig); sd(eu2017_sport_centre$MVPA_tot_time_pluswalk_2vig)
mean(eu2017_sport_centre$MVPA_met); sd(eu2017_sport_centre$MVPA_met)
table(eu2017_sport_centre$WHO_prev);prop.table(table(eu2017_sport_centre$WHO_prev))
table(eu2017_sport_centre$WHO_prev2);prop.table(table(eu2017_sport_centre$WHO_prev2))


# Only physically active regression models
library("moments")
library(sjPlot)
set.seed(123456)

eu2017_active$Gender <- relevel(eu2017_active$Gender, ref = "Woman")
eu2017_active$Sizeofcommunity <- relevel(eu2017_active$Sizeofcommunity, ref = "Rural area")
eu2017_active$Education_3cat_recod <- relevel(eu2017_active$Education_3cat_recod, ref = "Primary")
eu2017_active$Ocupation_3cat <- relevel(eu2017_active$Ocupation_3cat, ref = "V-VII")
eu2017_active$Bills <- relevel(eu2017_active$Bills, ref = "Most of the time")
eu2017_active$WHO_prev2 <- relevel(eu2017_active$WHO_prev2, ref = "Mod active")
eu2017_active$ALL_Sport_centres_venue <- relevel(eu2017_active$ALL_Sport_centres_venue, ref = "Not mentioned")
eu2017_active$`Marital status` <- relevel(eu2017_active$`Marital status`, ref = "Single hh without children (9,11,13 in d7)")


model1a_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Education_3cat_recod, data = eu2017_active, family = binomial, na.action = na.exclude)
model1b_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Education_3cat_recod + ALL_Sport_centres_venue, data = eu2017_active, family = binomial, na.action = na.exclude)

model2a_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Ocupation_3cat, data = eu2017_active, family = binomial, na.action = na.exclude)
model2b_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Ocupation_3cat + ALL_Sport_centres_venue, data = eu2017_active, family = binomial, na.action = na.exclude)

model3a_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Bills, data = eu2017_active, family = binomial, na.action = na.exclude)
model3b_active <- glm(WHO_prev2 ~ Age + Gender + Sizeofcommunity + `Marital status` + Bills + ALL_Sport_centres_venue, data = eu2017_active, family = binomial, na.action = na.exclude)

tab_model(model1a_active, model1b_active, model2a_active, model2b_active, model3a_active, model3b_active, digits.re = 3)


library(lavaan)

eu2017_active$Gender_cont [ eu2017_active$Gender  == "Woman" ] <- 0
eu2017_active$Gender_cont [ eu2017_active$Gender  == "Man" ] <- 1

eu2017_active$Sizeofcommunity_cont [ eu2017_active$Sizeofcommunity  == "Rural area" ] <- 0
eu2017_active$Sizeofcommunity_cont [ eu2017_active$Sizeofcommunity  == "Towns and suburbs/ small urban area" ] <- 1
eu2017_active$Sizeofcommunity_cont [ eu2017_active$Sizeofcommunity  == "Cities/ large urban area" ] <- 2

eu2017_active$ALL_Sport_centres_venue_cont [ eu2017_active$ALL_Sport_centres_venue  == "Not mentioned" ] <- 0
eu2017_active$ALL_Sport_centres_venue_cont [ eu2017_active$ALL_Sport_centres_venue  == "At a sport centre" ] <- 1

eu2017_active$ALL_Sport_centres_membership_cont [ eu2017_active$ALL_Sport_centres_membership  == "Not mentioned" ] <- 0
eu2017_active$ALL_Sport_centres_membership_cont [ eu2017_active$ALL_Sport_centres_membership  == "Membership in a sport centre" ] <- 1

eu2017_active$Bills_cont [ eu2017_active$Bills  == "Most of the time" ] <- 0
eu2017_active$Bills_cont [ eu2017_active$Bills  == "From time to time" ] <- 1
eu2017_active$Bills_cont [ eu2017_active$Bills  == "Almost never/never" ] <- 2

eu2017_active$Ocupation_3cat_cont [ eu2017_active$Ocupation_3cat  == "V-VII" ] <- 0
eu2017_active$Ocupation_3cat_cont [ eu2017_active$Ocupation_3cat  == "III-IV" ] <- 1
eu2017_active$Ocupation_3cat_cont [ eu2017_active$Ocupation_3cat  == "I-II" ] <- 2

eu2017_active$Education_3cat_recod_cont [ eu2017_active$Education_3cat_recod  == "Primary" ] <- 0
eu2017_active$Education_3cat_recod_cont [ eu2017_active$Education_3cat_recod  == "Secondary" ] <- 1
eu2017_active$Education_3cat_recod_cont [ eu2017_active$Education_3cat_recod  == "University" ] <- 2

eu2017_active$WHO_prev_cont [ eu2017_active$WHO_prev2  == "Mod active" ] <- 0
eu2017_active$WHO_prev_cont [ eu2017_active$WHO_prev2  == "High active" ] <- 1

eu2017_active$Marital_status_cont [ eu2017_active$`Marital status`  == "Single hh without children (9,11,13 in d7)" ] <- 0
eu2017_active$Marital_status_cont [ eu2017_active$`Marital status`  == "Single hh with children (10,12,14 in d7)" ] <- 1
eu2017_active$Marital_status_cont [ eu2017_active$`Marital status`  == "Multiple hh without children (1, 5 in d7)" ] <- 2
eu2017_active$Marital_status_cont [ eu2017_active$`Marital status`  == "Multiple hh with children (2-4, 6-8 in d7)" ] <- 3


set.seed(123456)
specmod_educ_active <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont
WHO_prev_cont ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'


fitmod_educ_active <- sem(specmod_educ_active, data = eu2017_active, se = "bootstrap", std.ov = T)
summary(fitmod_educ_active, fit.measures=T, rsquare=T, ci=T)



specmod_ocup_active <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup_active <- sem(specmod_ocup_active, data = eu2017_active, se = "bootstrap", std.ov = T)
summary(fitmod_ocup_active, fit.measures=T, rsquare=T, standardized=T, ci=T)





specmod_bills_active <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age + Marital_status_cont
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age + Marital_status_cont
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills_active <- sem(specmod_bills_active, data = eu2017_active, se = "bootstrap", std.ov = T)
summary(fitmod_bills_active, fit.measures=T, rsquare=T, standardized=T, ci=T)




