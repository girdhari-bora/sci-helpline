library(dygraphs)
library(xts)
library(highcharter)
library("highcharter")
library("magrittr")
library("dplyr")
library(koboloadeR)
library(rio)

# count <- kobo_datasets(user = c("tattvafoundation","tattva_2017"), 
#                        api = "https://kc.humanitarianresponse.info/api/v1/")  
# 
# kobo_submission_count("153088", api = "https://kc.humanitarianresponse.info/api/v1/",user = c("tattvafoundation","tattva"))
# 
# if(nrow(hwc_main) < count) {
  
sci_dump <- kobo_data_downloader("156941",user = c("tattvafoundation","tattva_2017"), 
                                   api = "https://kc.humanitarianresponse.info/api/v1/", check = FALSE)

# saveRDS(sci_dump, file = "./Data/sci_dump.rds", compress = FALSE)

write.csv(sci_dump,"./Data/sci_dump.csv")

sci_dump1 <- read.csv("./Data/sci_dump.csv", 
                      header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)

convert("./Data/sci_dump.csv","./Data/sci_dump1.rds")

write.csv(sci_dump,"./Data/sci_dump.csv")
  


sci_dump1[sci_dump1 == "n/a"] <- ""
 
sci_dump <- sci_dump1[-c(12,16,20:25,56,97:109,114,122:124,130:131,214,215,235,237,239,240,244,245
                         ,247,260:262,267,269:310,326,510),]
sci_dump$District[sci_dump$District == ""] <- "Other"

write.csv(sci_dump,"sci_dump_06-01-2018.csv")

# sci_dump <- read.csv("./Data/sci_dump.csv", 
#                      header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
# sci_dump[sci_dump == "n/a"] <- ""
# 
# sci_dump <- sci_dump[-c(12,16,20:25,56,97:109,114,122:124,130:131,214,235,237,239,240,244,245
#                         ,247,260:262,267,269:310),]

aa <- sci_dump %>% 
  group_by(X_3_Date_of_call,X_1_Call_Type) %>% summarise(n=n())



# aa_total<- sci_dump %>% 
#   group_by(X_3_Date_of_call) %>% summarise(n=n())
# aa_total <- aa_total[c(-1),]
# colnames(aa_total)[1] <- "Date"
# colnames(aa_total)[2] <- "Call_Nos_total"
# x0 <- xts(aa_total, as.Date(aa_total$Date, format = "%Y-%m-%d")) 


aa <- aa[c(-1),]
aa_in <- filter(aa, X_1_Call_Type == "inbound") %>% select(X_3_Date_of_call,n)
colnames(aa_in)[1] <- "Date"
colnames(aa_in)[2] <- "Call_Nos"
x1 <- xts(aa_in, as.Date(aa_in$Date, format = "%Y-%m-%d")) 

aa_out <- filter(aa, X_1_Call_Type == "outbound") %>% select(X_3_Date_of_call,n)
colnames(aa_out)[1] <- "Date"
colnames(aa_out)[2] <- "Call_Nos"
x2 <- xts(aa_out, as.Date(aa_out$Date, format = "%Y-%m-%d")) 


arr_dis <- cbind(x1, x2)
# arr_dis <- cbind(arr_dis, x0)

arr_dis[is.na(arr_dis)] <- ""



dygraph(arr_dis, main = "SDI Daily Helpline Calls") %>%
  dyAxis("y", label = "No. of calls",valueRange = c(0,30)) %>%
  dyAxis("x", label = "Date" , drawGrid = FALSE) %>%
  dyOptions(includeZero = TRUE,
            axisLineColor = "navy",
            gridLineColor = "lightblue") %>%
  dySeries("Call_Nos", label = "Inbound") %>%
  dySeries("Call_Nos.1", label = "Outbound") %>%
  # dySeries("Call_Nos_total", label = "Total") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
              highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)%>%
  # dyRoller(rollPeriod = 1)%>%
  dyRangeSelector(height = 20, strokeColor = "")




######################### HIST Time of the day


temp1 <- filter(sci_dump,sci_dump$X_4_Time_of_call !="" )
thour <- temp1$X_4_Time_of_call
thour <-as.integer(gsub( ":.*$", "", thour ))


hchart(thour,color="#fca8c7",breaks=10)%>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "SCI SDI Helpline: Time of call",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This histogram is based on the the recorded discharge time,from the hospital register") %>%
  hc_xAxis(title = list(text = "Call Time (Hours of Day)"),
           opposite = FALSE
           # plotLines = list(
           #   list(label = list(text = "Mean"),
           #        color = "#F4133C",
           #        width = 1.5,
           #        value = mean(dataset$Time.of.SHH.death.LAMA.Referred..Hour)))
           ) %>%
  
  hc_yAxis(title = list(text = "Number of Calls"), allowDecimals = FALSE) %>%
  
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)


######################### PIE Caller Sex


temp2 <- filter(sci_dump,sci_dump$X_5_Sex_of_the_caller !="" )
gender <- temp2$X_5_Sex_of_the_caller
# thour <-as.integer(gsub( ":.*$", "", thour ))


hchart(gender, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Callers by Gender",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)

######################### PIE Type of Caller


temp3 <- filter(sci_dump,sci_dump$X_7_Type_of_caller !="" )
caller_type <- temp3$X_7_Type_of_caller
caller_type[caller_type == "father"] <- "Father"
caller_type[caller_type == "family_member"] <- "Family Members"
caller_type[caller_type == "anm"] <- "ANM"
caller_type[caller_type == "_asha"] <- "ASHA"
caller_type[caller_type == "mother"] <- "Mother"
caller_type[caller_type == "children__18_years"] <- "Children above 18yrs"
caller_type[caller_type == "aww"] <- "AWW"
caller_type[caller_type == "_other"] <- "Others"

hchart(caller_type, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Who Called: By Role",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)


######################### PIE Reason for call


temp4 <- filter(sci_dump,sci_dump$X_13_Reason_of_the_call.grievance == "True" | sci_dump$X_13_Reason_of_the_call.information == "True" 
                | sci_dump$X_13_Reason_of_the_call.other == "True" )
reason <- select(temp4, X_13_Reason_of_the_call.grievance, X_13_Reason_of_the_call.information,
       X_13_Reason_of_the_call.other)

colnames(reason)[1] <- "Grievance"
colnames(reason)[2] <- "Information Seeking"
colnames(reason)[3] <- "Others"

reason[reason == "True"] <- 1
reason[reason == "False"] <- 0
reason$Grievance <- as.integer(reason$Grievance)
reason$`Information Seeking` <- as.integer(reason$`Information Seeking`)
reason$Others <- as.integer(reason$Others)

reason_all <- c(rep("Greivance",sum(reason$Grievance)),rep("Infomration Seeking",sum(reason$`Information Seeking`))
                , rep("Others",sum(reason$Others)))


# reason_1 <- data.frame( Grievance = c(sum(reason$Grievance)), Information = c(sum(reason$`Information Seeking`)), Others =  c(sum(reason$Others))
#                         )


hchart(reason_all, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Who Called: By Role",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)


######################### PIE Information sought


temp5 <- filter(sci_dump,sci_dump$X_13_Reason_of_the_call.information == "True" & sci_dump$Type_of_information.diarrhoea == "True" )
direason <- select(temp5, Type_of_information.diarrhoea, Type_of_information.other)

colnames(direason)[1] <- "Diarrhoea"
colnames(direason)[2] <- "Other Info"


direason[direason == "True"] <- 1
direason[direason == "False"] <- 0
direason$Diarrhoea <- as.integer(direason$Diarrhoea)
direason$`Other Info` <- as.integer(direason$`Other Info`)


direason_all <- c(rep("Diarrhoea",sum(direason$Diarrhoea)),rep("Other Info",sum(direason$`Other Info`)))


# reason_1 <- data.frame( Grievance = c(sum(reason$Grievance)), Information = c(sum(reason$`Information Seeking`)), Others =  c(sum(reason$Others))
#                         )


hchart(direason_all, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "What Information Was Sought?",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)

######################### PIE What DIARRHOEA INFO sought


temp6 <- filter(sci_dump,sci_dump$X_13_Reason_of_the_call.information == "True" & sci_dump$Type_of_information.diarrhoea == "True" )

diinforeason <- select(temp6, X_20_Issue_category.diarrhoea_symp,
                   X_20_Issue_category.diarrhoea_prev,X_20_Issue_category.diarrhoea_trea)

colnames(diinforeason)[1] <- "Diarrhoea Symptoms"
colnames(diinforeason)[2] <- "Diarrhoea Prevention"
colnames(diinforeason)[3] <- "Diarrhoea Treatment"

diinforeason[diinforeason == "True"] <- 1
diinforeason[diinforeason == "False"] <- 0
diinforeason$`Diarrhoea Symptoms` <- as.integer(diinforeason$`Diarrhoea Symptoms`)
diinforeason$`Diarrhoea Prevention` <- as.integer(diinforeason$`Diarrhoea Prevention`)
diinforeason$`Diarrhoea Treatment` <- as.integer(diinforeason$`Diarrhoea Treatment`)

diinforeason_all <- c(rep("Diarrhoea Symptoms",sum(diinforeason$`Diarrhoea Symptoms`)),
                  rep("Diarrhoea Prevention",sum(diinforeason$`Diarrhoea Prevention`)),
                  rep( "Diarrhoea Treatment",sum(diinforeason$`Diarrhoea Treatment`))
                  )


# reason_1 <- data.frame( Grievance = c(sum(reason$Grievance)), Information = c(sum(reason$`Information Seeking`)), Others =  c(sum(reason$Others))
#                         )


hchart(diinforeason_all, type = "column", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Diarrohea Information for?",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)


######################### Case Referred 


temp7 <- filter(sci_dump,sci_dump$X_23_1_Case_referred_ != "")

referral_all <- select(temp7, X_23_1_Case_referred_ ,Case_Referral)
referral_yes <- filter(referral_all, Case_Referral != "")   

referral_yes[referral_yes == "chc"] <- "CHC"
referral_yes[referral_yes == "phc"] <- "PHC"
referral_yes[referral_yes == "anm"] <- "ANM"
referral_yes[referral_yes == "asha"] <- "ASHA"
referral_yes[referral_yes == "other"] <- "Other"



hchart(referral_all$X_23_1_Case_referred_, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Cases Referred (of all calls)",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)



hchart(referral_yes$Case_Referral, type = "pie", name = "No. of callers") %>% 
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Cases referred to ?",
           style = list(fontWeight = "bold")) %>%
  # hc_subtitle(text = "This pie chart is based on the the recorded mode of transportation,from the hospital register") %>%
  # hc_xAxis(title = list(text = "Number of Days"),
  #          opposite = FALSE,
  #          plotLines = list(
  #            list(label = list(text = "Mean"),
  #                 color = "#F4133C",
  #                 width = 1.5,
  #                 value = mean(as.integer(stay))))) %>%
  # 
  # hc_yAxis(title = list(text = "Number of Pregnant Women"), allowDecimals = FALSE) %>%
  hc_credits(enabled = TRUE,
             text = "Source: SCI SDI Call centre data,2017-18",
             style = list(fontSize = "9px")) %>%
  hc_exporting(enabled = TRUE)


class(sci_dump)
sss <- as.data.table(sci_dump)

sss_temp <- sss[,c(13,14,16,7,8,10,17,41,42,44)]
colnames(sss_temp)[3] <- "Village"
colnames(sss_temp)[4] <- "Date"
colnames(sss_temp)[5] <- "Time"
colnames(sss_temp)[6] <- "Caller Name"
colnames(sss_temp)[7] <- "Mobile"
colnames(sss_temp)[8] <- "If Referred"
colnames(sss_temp)[9] <- "Referred to"
colnames(sss_temp)[10] <- "Referral Issue"



DT::datatable(sss_temp)


############################
ib <- NULL
ob <- NULL
ref <- NULL

month_all <- unique(as.character(sort(factor((months(as.Date(sci_dump$X_3_Date_of_call))),levels=month.name))))

for (i in 1:length(month_all))
{
  ib[i] <- filter(sci_dump,X_1_Call_Type =="inbound", months(as.Date(sci_dump$X_3_Date_of_call)) == month_all[i])%>%
    summarise(n=n())
  ob[i] <- filter(sci_dump,X_1_Call_Type =="outbound", months(as.Date(sci_dump$X_3_Date_of_call)) == month_all[i])%>%
    summarise(n=n())
  
  ref[i] <- filter(sci_dump,X_23_1_Case_referred_=="yes",
                  X_13_Reason_of_the_call.grievance =="False",months(as.Date(sci_dump$X_3_Date_of_call)) == month_all[i])%>%
    summarise(n=n())
}

xx <- cbind(month_all,ib,ob,ref)

hc <- highchart() %>% 
  hc_xAxis(categories = unlist(xx[,1])) %>% 
  hc_add_series(name = "Inbound", data = unlist(xx[,2])) %>% 
  hc_add_series(name = "Outbound", data = unlist(xx[,3])) %>% 
  hc_add_series(name = "Referral", data = unlist(xx[,4])) %>%
  # hc_add_series(name = "Other city",
  #               data = (citytemp$tokyo + citytemp$london)/2)

hc_chart(type = "column")
         # ,
         # options3d = list(enabled = TRUE, beta = 15, alpha = 15))
hc


xx[,1] <- factor(xx[,1],levels=month.name)
xx = xx[order(xx[,1],decreasing=FALSE),]











d_referral_cases1 <- inner_join(sci_dump,d_referral_cases , by= "uuid")
t1 <- as.Date(filter(d_referral_cases1, d_referral_cases1$ref_patient_status == "Recovered")$ref_date, format = "%d/%m/%Y" )
t2 <- as.Date(filter(d_referral_cases1, d_referral_cases1$ref_patient_status == "Recovered")$recovery_date, format = "%d/%m/%Y" )
t3 <- as.integer(t2-t1)





unique(d_referral_cases$ref_followup_status_2)

d_referral_cases$ref_followup_status_1[d_referral_cases$ref_followup_status_1 == "Mobile switched off" | 
                                       d_referral_cases$ref_followup_status_1 == " Mobile switched off" | 
                                       d_referral_cases$ref_followup_status_1 == " Call not received"| 
                                       d_referral_cases$ref_followup_status_1 == "Call not received"|
                                       d_referral_cases$ref_followup_status_1 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_1 == "Not reachable"] <- "Not Reachable"
d_referral_cases$ref_followup_status_2[d_referral_cases$ref_followup_status_2 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_2 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_2 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_2 == "Call not received"|
                                         d_referral_cases$ref_followup_status_2 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_2 == "Not reachable"|
                                         d_referral_cases$ref_followup_status_2 == " Not reachable."] <- "Not Reachable"
d_referral_cases$ref_followup_status_3[d_referral_cases$ref_followup_status_3 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_3 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_3 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_3 == "Call not received"|
                                         d_referral_cases$ref_followup_status_3 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_3 == "Not reachable"|
                                         d_referral_cases$ref_followup_status_3 == " Not reachable."] <- "Not Reachable"

d_referral_cases$ref_followup_status_3[d_referral_cases$ref_followup_status_3 == "received"] <- "Received"

d_referral_cases$ref_followup_status_4[d_referral_cases$ref_followup_status_4 == "Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_4 == " Mobile switched off" | 
                                         d_referral_cases$ref_followup_status_4 == " Call not received"| 
                                         d_referral_cases$ref_followup_status_4 == "Call not received"|
                                         d_referral_cases$ref_followup_status_4 == "Refuse to talk"|
                                         d_referral_cases$ref_followup_status_4 == "Not reachable"] <- "Not Reachable"

d_referral_cases$ref_followup_status_4[d_referral_cases$ref_followup_status_4 == "received"] <- "Received"


[d_referral_cases== "Mobile switched off"] 
<- "Not reachable"
d_referral_cases[d_referral_cases$ref_followup_status_1 == "Mobile switched off",] <- "Not reachable"

############################################################################################################################### 

d_referral_cases_1 <- inner_join(sci_dump,d_referral_cases , by= "uuid")
summary(d_referral_cases_1$ref_followup_status_1)

table(d_referral_cases_1$ref_patient_status_1)

filter(d_referral_cases, !(d_referral_cases$uuid %in% sci_dump$uuid) )

library(zoo)

a <- format(as.yearmon(sci_dump$X_3_Date_of_call) , format="%b-%Y")

aa <- sort(unique(as.yearmon(sci_dump$X_3_Date_of_call))) 
aa
port_mon <- c("jan", "fev", "mar", "abr", "mai", "jun",
              "jul", "ago", "set", "out", "nov", "dez")

sort(unique (a))
month_all <- unique(as.character(sort(factor((months(as.Date(sci_dump$X_3_Date_of_call))),levels=month.name))))

month_all <- unique(format(as.Date(sci_dump$X_3_Date_of_call) , format="%b%Y"))
month_all

filter(d_referral_cases, ref_followup_status_1 == "Followup-1", d_referral_cases$ref_patient_status_1 == ""  )
table(d_referral_cases$ref_followup_status_1)


# d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")

vec <- c("ref_followup_status_1","ref_patient_status_1","treatment_given_1",
          "ref_followup_status_2","ref_patient_status_2","treatment_given_2",
          "ref_followup_status_3","ref_patient_status_3","treatment_given_3",
          "ref_followup_status_4","ref_patient_status_4","treatment_given_4")

a <- d_referral_cases %>% 
  group_by_(.dots = vec) %>%
    summarise(`Cases` = n())



