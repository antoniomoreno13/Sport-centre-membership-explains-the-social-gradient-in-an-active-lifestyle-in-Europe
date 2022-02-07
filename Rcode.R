
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
eu2017 <- read.spss("C:/Users/Desktop/eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224,222, 277, 242,291, 236, 237:240, 270, 11, 59:61)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:27] <- c("Gender", "Age","Marital status", "Social class subjective", "Type of community", "Sizeofcommunity" ,"Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Ocupation_last_job", "Bills", "Country")
eu2017["survey"] <- "2017" # TIME VARIABLE
names(eu2017)[29:32] <- c("Fitness_centre_venue", "Sport_club_venue","Sport_centre_venue")

eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The upper middle class of society"] <- "The higher class of society"
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The lower middle class of society"] <- "The working class of society"


eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")


#### Sitting time ####

eu2017$Sit[eu2017$Sit == "DK"] <- NA
eu2017$Sit_med [eu2017$Sit == "1 hour or less"] <- 30
eu2017$Sit_med [eu2017$Sit == "1 hour to 1h30min" | eu2017$Sit == "1 hour to 1 hour and 30 minutes"] <- 75
eu2017$Sit_med [eu2017$Sit == "1h31min to 2h30min" | eu2017$Sit == "1 hour 31 minutes to 2 hours 30 minutes"] <- 120
eu2017$Sit_med [eu2017$Sit == "2h31min to 3h30min" | eu2017$Sit == "2 hours 31 minutes to 3 hours 30 minutes"] <- 180
eu2017$Sit_med [eu2017$Sit == "3h31min to 4h30min" | eu2017$Sit == "3 hours 31 minutes to 4 hours 30 minutes"] <- 240
eu2017$Sit_med [eu2017$Sit == "4h31min to 5h30min" | eu2017$Sit == "4 hours 31 minutes to 5 hours 30 minutes"] <- 300
eu2017$Sit_med [eu2017$Sit == "5h31min to 6h30min" | eu2017$Sit == "5 hours 31 minutes to 6 hours 30 minutes"] <- 360
eu2017$Sit_med [eu2017$Sit == "6h31min to 7h30min" | eu2017$Sit == "6 hours 31 minutes to 7 hours 30 minutes"] <- 420
eu2017$Sit_med [eu2017$Sit == "7h31min to 8h30min" | eu2017$Sit == "7 hours 31 minutes to 8 hours 30 minutes"] <- 480
eu2017$Sit_med [eu2017$Sit == "More than 8h30min" | eu2017$Sit == "More than 8 hours and 30 minutes"] <- 540


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

eu2017$`Type of community`[eu2017$`Type of community` == "DK"] <- NA; eu2017$`Type of community`[eu2017$`Type of community` == "Small or middle sized town"] <- "Small/middle town";
eu2017$`Type of community`<- fct_drop(eu2017$`Type of community`, only = c("DK", "Small or middle sized town"))

eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Small/middle town"] <- "Urban"; eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Large town"] <- "Urban"; 
eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Rural area or village"] <- "Rural"

eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Towns and suburbs/ small urban area"] <- "Urban"; eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Cities/ large urban area"] <- "Urban"; 
eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Rural area"] <- "Rural"



eu2017$`Social class subjective`[eu2017$`Social class subjective` == "DK"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "None (SPONT.)"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "Other (SPONT.)"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "Refusal (SPONT.)"] <- NA
eu2017$`Social class subjective`<- fct_drop(eu2017$`Social class subjective`, only = c("The upper middle class of society", "The lower middle class of society", "Refusal (SPONT.)", "Other (SPONT.)","None (SPONT.)" , "DK"))


eu2017$Age_3clusters [ eu2017$Age >= 18 & eu2017$Age < 45 ] <- "18-44"  
eu2017$Age_3clusters [ eu2017$Age >= 45 & eu2017$Age < 70 ] <- "45-69"  
eu2017$Age_3clusters [ eu2017$Age >= 70] <- "70+"  

eu2017$Age_3clusters2 [ eu2017$Age >= 18 & eu2017$Age < 35 ] <- "18-34"  
eu2017$Age_3clusters2 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters2 [ eu2017$Age >= 65] <- "65+"  

eu2017$Age_3clusters3 [ eu2017$Age < 35 ] <- "15-34"  
eu2017$Age_3clusters3 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters3 [ eu2017$Age >= 65] <- "65+"

eu2017$Age_9clusters [ eu2017$Age == 18] <- "18"  
eu2017$Age_9clusters [ eu2017$Age == 19] <- "19"  
eu2017$Age_9clusters [ eu2017$Age >= 20 & eu2017$Age < 30 ] <- "20-29"  
eu2017$Age_9clusters [ eu2017$Age >= 30 & eu2017$Age < 40 ] <- "30-39"  
eu2017$Age_9clusters [ eu2017$Age >= 40 & eu2017$Age < 50 ] <- "40-49"  
eu2017$Age_9clusters [ eu2017$Age >= 50 & eu2017$Age < 60 ] <- "50-59"  
eu2017$Age_9clusters [ eu2017$Age >= 60 & eu2017$Age < 70 ] <- "60-69"  
eu2017$Age_9clusters [ eu2017$Age >= 70 & eu2017$Age < 80 ] <- "70-79"  
eu2017$Age_9clusters [ eu2017$Age >= 80 ] <- "80+"  


eu2017$Country_rec <- eu2017$Country; eu2017$Country_rec <- fct_expand(eu2017$Country_rec, c("DE Germany", "UK United Kingdom")) 
eu2017$Country_rec[eu2017$Country_rec == "DE-W - Germany - West" | eu2017$Country_rec =="DE-E Germany East" ] <- "DE Germany"
eu2017$Country_rec[eu2017$Country_rec ==  "GB-NIR Northern Ireland" | eu2017$Country_rec =="GB-GBN - Great Britain"] <- "UK United Kingdom"
eu2017$Country_rec<- fct_drop(eu2017$Country_rec, only = c("DE-W - Germany - West", "DE-E Germany East", "GB-NIR Northern Ireland", "GB-GBN - Great Britain", 
       "LI - Liechtenstein (NOT INCLUDED)", "IS - Iceland (NOT INCLUDED)", "CH - Switzerland (NOT INCLUDED)", "NO - Norway (NOT INCLUDED)",
       "RS - Serbia (NOT INCLUDED)", "ME - Montenegro (NOT INCLUDED)", "MK - Makedonia/FYROM (NOT INCLUDED)", "CY-TCC - Cyprus TCC (NOT INCLUDED)",
       "TR - Turkey (NOT INCLUDED)", "-"))



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



eu2017$Ocupation_5cat[eu2017$Ocupation == "Employed position, service job" | eu2017$Ocupation == "Supervisor" | eu2017$Ocupation == "Skilled manual worker" | eu2017$Ocupation == "Unskilled manual worker, etc." ] <- "Manual Workers"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Farmer" | eu2017$Ocupation == "Fisherman" | eu2017$Ocupation == "Professional (lawyer, etc.)" | eu2017$Ocupation == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation == "Business proprietors, etc." | eu2017$Ocupation == "Employed position, at desk" | eu2017$Ocupation == "Employed position, travelling" |  eu2017$Ocupation == "General management, etc."  | eu2017$Ocupation == "Middle management, etc."] <- "Self-employed or White collars workers"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Unemployed, temporarily not working" | eu2017$Ocupation == "Responsible for ordinary shopping, etc."] <- "Economically Inactive"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Retired, unable to work"] <- "Retirees"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Student"] <- "Students"




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

eu2017 <- eu2017[-c(which(eu2017$Vig_Days == 0 & eu2017$Vig_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days == 0 & eu2017$Mod_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days == 0 & eu2017$Walk_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Vig_Days > 0 & eu2017$Vig_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days > 0 & eu2017$Mod_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days > 0 & eu2017$Walk_Time_med == 0)), ] 

# REMOVING NA METS AND SIT TIME VALUES TO GET FINAL DATABASE SAMPLE SIZE
eu2017 <- eu2017[which(eu2017$MVPA_met >= 0), ]
eu2017 <- eu2017[which(eu2017$Age >= 18), ]




#### Descriptive statistics ####
euMen <- eu2017[which(eu2017$Gender == "Man"),]
euWomen <- eu2017[which(eu2017$Gender == "Woman"),]


eu2017$MVPA_tot_time_pluswalk_2vig <- (2*eu2017$VPA_tot_time) + eu2017$MPA_tot_time + eu2017$Walk_tot_time



## Simple ##
table(eu2017$Gender);prop.table(table(eu2017$Gender))
table(eu2017$Sizeofcommunity);prop.table(table(eu2017$Sizeofcommunity))
table(eu2017$Age_6clusters);prop.table(table(eu2017$Age_6clusters))

table(eu2017$Education_3cat_recod);prop.table(table(eu2017$Education_3cat_recod))
table(eu2017$Ocupation_3cat);prop.table(table(eu2017$Ocupation_3cat))
table(eu2017$Bills);prop.table(table(eu2017$Bills))

table(eu2017$Sport_centre_venue);prop.table(table(eu2017$Sport_centre_venue))
table(eu2017$Sport_club_venue);prop.table(table(eu2017$Sport_club_venue))
table(eu2017$Fitness_centre_venue);prop.table(table(eu2017$Fitness_centre_venue))
table(eu2017$ALL_Sport_centres_venue);prop.table(table(eu2017$ALL_Sport_centres_venue))


mean(eu2017$MVPA_tot_time_pluswalk_2vig); sd(eu2017$MVPA_tot_time_pluswalk_2vig)
mean(eu2017$MVPA_met); sd(eu2017$MVPA_met)
table(eu2017$WHO_prev);prop.table(table(eu2017$WHO_prev))



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




set.seed(123456)
specmod_educ <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont
MVPA_tot_time_pluswalk_2vig ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'


fitmod_educ <- sem(specmod_educ, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ, fit.measures=T, rsquare=T, ci=T)



specmod_ocup <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont 
MVPA_tot_time_pluswalk_2vig ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup <- sem(specmod_ocup, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup, fit.measures=T, rsquare=T, standardized=T, ci=T)





specmod_bills <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age
MVPA_tot_time_pluswalk_2vig ~ b*ALL_Sport_centres_venue_cont 
MVPA_tot_time_pluswalk_2vig ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills <- sem(specmod_bills, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills, fit.measures=T, rsquare=T, standardized=T, ci=T)




# Sensitive analyses with ACTIVE & VENUE
specmod_educ5 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ5 <- sem(specmod_educ5, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ5, fit.measures=T, rsquare=T, standardized=T, ci=T)



specmod_ocup5 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup5 <- sem(specmod_ocup5, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup5, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills5 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age
WHO_prev_cont ~ b*ALL_Sport_centres_venue_cont 
WHO_prev_cont ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills5 <- sem(specmod_bills5, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills5, fit.measures=T, rsquare=T, standardized=T, ci=T)





# Sensitive analyses with Physical activity quartiles & VENUE
quantile(eu2017$MVPA_tot_time_pluswalk_2vig, na.rm = T)
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig <= 105] <- 0
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 105 & eu2017$MVPA_tot_time_pluswalk_2vig <= 315] <- 1
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 315 & eu2017$MVPA_tot_time_pluswalk_2vig <= 735] <- 2
eu2017$MVPA_quartiles_cont [eu2017$MVPA_tot_time_pluswalk_2vig > 735] <- 3


specmod_educ7 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ7 <- sem(specmod_educ7, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ7, fit.measures=T, rsquare=T, standardized=T, ci=T)




specmod_ocup7 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup7 <- sem(specmod_ocup7, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup7, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills7 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age
MVPA_quartiles_cont ~ b*ALL_Sport_centres_venue_cont 
MVPA_quartiles_cont ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills7 <- sem(specmod_bills7, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills7, fit.measures=T, rsquare=T, standardized=T, ci=T)







# Analyses with Walking & VENUE
specmod_educ8 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont  + Age
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Education_3cat_recod_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_educ8 <- sem(specmod_educ8, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_educ8, fit.measures=T, rsquare=T, standardized=T, ci=T)




specmod_ocup8 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont  + Age
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Ocupation_3cat_cont + Gender_cont + Sizeofcommunity_cont + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_ocup8 <- sem(specmod_ocup8, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_ocup8, fit.measures=T, rsquare=T, standardized=T, ci=T)






specmod_bills8 <- '#simple mediation
ALL_Sport_centres_venue_cont ~ a*Bills_cont + Gender_cont + Sizeofcommunity_cont + Age
Walk_tot_time ~ b*ALL_Sport_centres_venue_cont 
Walk_tot_time ~ c*Bills_cont + Gender_cont + Sizeofcommunity_cont  + Age
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'

fitmod_bills8 <- sem(specmod_bills8, data = eu2017, se = "bootstrap", std.ov = T)
summary(fitmod_bills8, fit.measures=T, rsquare=T, standardized=T, ci=T)





#### PLOTS ####
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggalluvial)

## Alluvial plot ## 
Educ_alluvial <- read.csv2("C:/Users/Desktop/Educ_alluvial.csv", dec = ",", header = F, sep = ";")
Educ_alluvial$V1 <- factor(Educ_alluvial$V1, levels=c('University', 'Secondary', 'Primary'))

alluvial_educ <- ggplot(data = Educ_alluvial,
       aes(axis1 = V1, axis2 = V2, y = V3)) +
  scale_x_discrete(limits = c("V1", "V2"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = V2))+
  geom_stratum(width = 0.2, fill="grey99") +
  geom_text(stat = "stratum", size=2.75, aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), line = element_blank(),
        title = element_blank(), legend.position = "none")+
  scale_fill_brewer(palette="Set1")
alluvial_educ


Ocup_alluvial <- read.csv2("C:/Users/Desktop/Ocup_alluvial.csv", dec = ",", header = F, sep = ";")
Ocup_alluvial$V1 <- factor(Ocup_alluvial$V1, levels=c('I-II', 'III-IV', 'V-VII'))

alluvial_ocup <- ggplot(data = Ocup_alluvial,
       aes(axis1 = V1, axis2 = V2, y = V3)) +
  scale_x_discrete(limits = c("V1", "V2"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = V2))+
  geom_stratum(width = 0.2, fill="grey99") +
  geom_text(stat = "stratum",size=2.75, aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), line = element_blank(),
        title = element_blank(), legend.position = "none")+
  scale_fill_brewer(palette="Set1")



Bills_alluvial <- read.csv2("C:/Users/Desktop/Bills_alluvial.csv", dec = ",", header = F, sep = ";")
Bills_alluvial$V1 <- factor(Bills_alluvial$V1, levels=c('Almost never/never', 'From time to time', 'Most of the time'))

alluvial_bills <- ggplot(data = Bills_alluvial,
       aes(axis1 = V1, axis2 = V2, y = V3)) +
  scale_x_discrete(limits = c("V1", "V2"), expand = c(.05, .05)) +
  geom_alluvium(aes(fill = V2))+
  geom_stratum(width = 0.2, fill="grey99") +
  geom_text(stat = "stratum",size=2.75, aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), line = element_blank(),
        title = element_blank(), legend.position = "none")+
  scale_fill_brewer(palette="Set1")



ggsave("Alluvials.pdf", width = 7, height = 5, units = "in", dpi = 600)





## Violin plots ##
violin_educ <- melt(eu2017, id = c( "ID", "ALL_Sport_centres_venue", "MVPA_tot_time_pluswalk_2vig", "Walk_tot_time"),
                     measure = c("Education_3cat_recod"),
                     variable.name = "SES",
                     value.name = "value")
violin_educ <- violin_educ[which(complete.cases(violin_educ)),]


violinplot_educ <- ggplot(violin_educ, aes(x=value, y=MVPA_tot_time_pluswalk_2vig, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") +
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+ ylim(0,4000)+
  labs(x="Educational attainment", y = "Moderate-to-vigorous PA (min/wk)")+
  scale_fill_brewer(palette="Set1")


violinplot_educ2 <- ggplot(violin_educ, aes(x=value, y=Walk_tot_time, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") +
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+ ylim(0,1000)+
  labs(x="Educational attainment", y = "Walking PA (min/wk)")+
  scale_fill_brewer(palette="Set1")



violin_ocup <- melt(eu2017, id = c( "ID", "ALL_Sport_centres_venue", "MVPA_tot_time_pluswalk_2vig", "Walk_tot_time"),
                     measure = c("Ocupation_3cat"),
                     variable.name = "SES",
                     value.name = "value")
violin_ocup <- violin_ocup[which(complete.cases(violin_ocup)),]
violin_ocup$value <- fct_relevel(violin_ocup$value, c("V-VII", "III-IV", "I-II"))

violinplot_ocup <- ggplot(violin_ocup, aes(x=value, y=MVPA_tot_time_pluswalk_2vig, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") +
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+ ylim(0,4000)+
  labs(x="Ocupational social class", y = "Moderate-to-vigorous PA (min/wk)")+
  scale_fill_brewer(palette="Set1")


violinplot_ocup2 <- ggplot(violin_ocup, aes(x=value, y=Walk_tot_time, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") +
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+ ylim(0,1000)+
  labs(x="Ocupational social class", y = "Walking PA (min/wk)")+
  scale_fill_brewer(palette="Set1")





violin_bills <- melt(eu2017, id = c( "ID", "ALL_Sport_centres_venue", "MVPA_tot_time_pluswalk_2vig", "Walk_tot_time"),
             measure = c("Bills"),
             variable.name = "SES",
             value.name = "value")
violin_bills <- violin_bills[which(complete.cases(violin_bills)),]
violin_bills$value <- fct_relevel(violin_bills$value, c("Most of the time", "From time to time", "Almost never/never"))


violinplot_bills <- ggplot(violin_bills, aes(x=value, y=MVPA_tot_time_pluswalk_2vig, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") +
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+ ylim(0,4000)+
  labs(x="Economic issues", y = "Moderate-to-vigorous PA (min/wk)")+
  scale_fill_brewer(palette="Set1")

violinplot_bills2 <- ggplot(violin_bills, aes(x=value, y=Walk_tot_time, by=ALL_Sport_centres_venue)) + 
  geom_violin(position=position_dodge(-1), fill="gainsboro") + ylim(0,1000)+
  geom_boxplot(width=0.1, position=position_dodge(-1), aes(fill=ALL_Sport_centres_venue), outlier.alpha = 0) +
  theme_minimal()+ theme(legend.title = element_blank())+
  labs(x="Economic issues", y = "Walking PA (min/wk)")+
  scale_fill_brewer(palette="Set1")




a <- ggarrange(alluvial_educ, 
          alluvial_ocup, 
          alluvial_bills,
          ncol = 1, nrow = 3, common.legend = F)
a

b <- ggarrange(violinplot_educ, violinplot_educ2, 
               violinplot_ocup, violinplot_ocup2, 
               violinplot_bills, violinplot_bills2,
               ncol = 2, nrow = 3, common.legend = T)
b



ggarrange(a,b, ncol = 2, widths = c(1,2))

ggsave("violin2.tiff", width = 17.5, height = 8, units = "in", dpi = 600)


