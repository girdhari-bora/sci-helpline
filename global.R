library(data.table)
library(dplyr)

#change in global

sci_dump1 <- read.csv("./Data/sci_dump.csv", 
                      header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)

sci_dump1 <- as.data.table(sci_dump1)

sci_dump1 <- sci_dump1[as.Date(sci_dump1$start) > as.Date("2017-08-24")]

sci_dump1[which(sci_dump1$X_2_Duration_of_Call == 55 )]$X_2_Duration_of_Call <- "5.5"
sci_dump1[which(sci_dump1$X_2_Duration_of_Call == 516 )]$X_2_Duration_of_Call <- "5.16"
sci_dump1[which(sci_dump1$X_2_Duration_of_Call == 251 )]$X_2_Duration_of_Call <- "2.51"

sci_dump1 <- sci_dump1[sci_dump1$X_3_Date_of_call != "n/a" | sci_dump1$X_4_Time_of_call != "n/a" ,]

sci_dump <- sci_dump1[c(!(sci_dump1$X_1_1_Outbound_Call_Status == "n/a" & sci_dump1$X_1_Call_Type == "outbound"))]

sci_dump[sci_dump == "n/a"] <- ""

sci_dump$District[sci_dump$District == ""] <- "Other"

colnames(sci_dump)[colnames(sci_dump) == "X_uuid"] <- "uuid"

###############################################################REFERRAL CASES CSV READ

d_referral_cases <- read.csv("./Data/referral_final_new.csv", 
                      header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
d_referral_cases[d_referral_cases == "Don’t know" | d_referral_cases ==  "Don't know"] <- "Don't Know"

d_referral_cases[is.na(d_referral_cases)] <- ""

# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "Antibioticss" , replacement= "Antibiotics"),
#                                 stringsAsFactors = FALSE)
# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= " Antibioticss" , replacement= "Antibiotics"),
#                                stringsAsFactors = FALSE)
# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= " Antibioticss " , replacement= "Antibiotics"),
#                                stringsAsFactors = FALSE)
# d_referral_cases[is.na(d_referral_cases)] <- ""


# d_referral_cases$treatment_given_1 <- paste(d_referral_cases$treatment_given_1, d_referral_cases$treatment_given_not_recovered_1, sep = "")
# d_referral_cases$treatment_given_2 <- paste(d_referral_cases$treatment_given_2, d_referral_cases$treatment_given_not_recovered_2, sep = "")
# d_referral_cases$treatment_given_3 <- paste(d_referral_cases$treatment_given_3, d_referral_cases$treatment_given_not_recovered_3, sep = "")
# d_referral_cases$treatment_given_4 <- paste(d_referral_cases$treatment_given_4, d_referral_cases$treatment_given_not_recovered_4, sep = "")

# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "NA", replacement= ""),
#                                stringsAsFactors = FALSE)

d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern=" anti diarrheal dose" , replacement= "Anti Diarrhoeal Dose"),
                             stringsAsFactors = FALSE  )
d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= " ORS - Zinc", replacement= "ORS+Zinc"),
                               stringsAsFactors = FALSE)
d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "ORS - Zinc", replacement= "ORS+Zinc"),
                               stringsAsFactors = FALSE)
d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "ORS – Zinc", replacement= "ORS+Zinc"),
                               stringsAsFactors = FALSE)
d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "ORS - Antibiotic", replacement= "ORS-Antibiotic"),
                               stringsAsFactors = FALSE)
d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "ORS Antibiotic", replacement= "ORS-Antibiotic"),
                               stringsAsFactors = FALSE)




d_referral_cases$ref_followup_status_1[d_referral_cases$ref_followup_status_1 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_1 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_1 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_1 == "Call not received"|
                                         d_referral_cases$ref_followup_status_1 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_1 == "Not reachable"] <- "Unreachable"
d_referral_cases$ref_followup_status_2[d_referral_cases$ref_followup_status_2 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_2 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_2 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_2 == "Call not received"|
                                         d_referral_cases$ref_followup_status_2 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_2 == "Not reachable"|
                                         d_referral_cases$ref_followup_status_2 == " Not reachable."] <- "Unreachable"
d_referral_cases$ref_followup_status_3[d_referral_cases$ref_followup_status_3 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_3 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_3 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_3 == "Call not received"|
                                         d_referral_cases$ref_followup_status_3 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_3 == "Not reachable"|
                                         d_referral_cases$ref_followup_status_3 == " Not reachable."] <- "Unreachable"

d_referral_cases$ref_followup_status_3[d_referral_cases$ref_followup_status_3 == "received"] <- "Received"

d_referral_cases$ref_followup_status_4[d_referral_cases$ref_followup_status_4 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_4 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_4 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_4 == "Call not received"|
                                         d_referral_cases$ref_followup_status_4 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_4 == "Not reachable"] <- "Unreachable"

d_referral_cases$ref_followup_status_4[d_referral_cases$ref_followup_status_4 == "received"] <- "Received"


d_referral_cases$ref_followup_status_1[d_referral_cases$ref_followup_status_1 == "Received"] <- "Followup-1"
d_referral_cases$ref_followup_status_2[d_referral_cases$ref_followup_status_2 == "Received"] <- "Followup-2"
d_referral_cases$ref_followup_status_3[d_referral_cases$ref_followup_status_3 == "Received"] <- "Followup-3"
d_referral_cases$ref_followup_status_4[d_referral_cases$ref_followup_status_4 == "Received"] <- "Followup-4"



# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= " Antibiotic" , replacement= "Antibiotics"),
#                                stringsAsFactors = FALSE)
# d_referral_cases <- data.frame(sapply(d_referral_cases, gsub, pattern= "Antibiotic" , replacement= "Antibiotics"),
#                                stringsAsFactors = FALSE)




# treatment_list_string <- paste(filter(d_referral_cases, d_referral_cases$ref_followup_status == "Received")$treatment_given,collapse=",")
# treatment_list_vec <- strsplit(treatment_list_string, ",")[[1]]
# # treatment_list_vec[treatment_list_vec == " anti diarrheal dose"] <- "Anti Diarrhoeal Dose"
# # treatment_list_vec[treatment_list_vec == "Antibiotic" | treatment_list_vec == " Antibiotic" ] <- "Antibiotics"
# # treatment_list_vec[treatment_list_vec ==  "ORS - Zinc" | treatment_list_vec == "ORS – Zinc"
# #                    | treatment_list_vec == "ORS Antibiotic"| treatment_list_vec == "ORS - Antibiotic"
# #                    | treatment_list_vec ==  " ORS - Zinc" ] <- "ORS+Zinc"
# treatment_list_vec <- treatment_list_vec[treatment_list_vec!= ""]

# t1 <- as.Date(filter(d_referral_cases, d_referral_cases$ref_patient_status == "Recovered")$ref_date, format = "%d/%m/%Y" )
# t2 <- as.Date(filter(d_referral_cases, d_referral_cases$ref_patient_status == "Recovered")$recovery_date, format = "%d/%m/%Y" )

