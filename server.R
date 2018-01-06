
library(shiny)
library(shinydashboard)
# library(ggplot2)
# library(ggthemes)
# library(scales)
# library(magrittr)
# library(scales)
# library(lattice)
library(dplyr)
library(shiny)
# library(leaflet)
# library(RColorBrewer)
# library(sp)
# library(ggmap)
# library(htmltools)
# library(tidyr)
# library(stringr)
# library(tools)
# library(data.table)
# library(chron)
# library(xlsx)
# library("rio")
# library(collapsibleTree)
# library(ggplot2)
# library(ggthemes)
# library(scales)
# library(dygraphs)
# library(xts)
# library(magrittr)
# library(highcharter)
# library(dplyr)
# library(DT)
# library(data.table)
# library(shinyjs)
# library(htmltools)
# library(lubridate)
# library(dygraphs)
# library(tidyr)
# require(colorspace)
# library(lazyeval)
# library(shinythemes)
# library(ggvis)
library(collapsibleTree)


# #############READ DATA FILES INTO DATAFRAMES
# 
# sci_dump1 <- read.csv("./Data/sci_dump.csv", 
#                       header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
# sci_dump1[sci_dump1 == "n/a"] <- ""
# 
# sci_dump <- sci_dump1[-c(12,16,20:25,56,97:109,114,122:124,130:131,214,215,235,237,239,240,244,245
#                          ,247,260:262,267,269:310,326,510),]
# sci_dump$District[sci_dump$District == ""] <- "Other"
# 
# ####################


shinyServer(function(input,session, output) {
  
  
datasetInput <- reactive({
    
    validate(
      need(input$district != "", "Please select one or more district")
    )
    
    sci_dump_temp <- sci_dump[sci_dump$District %in% input$district,]
    return(filter(sci_dump_temp, 
                  as.Date(sci_dump_temp$X_3_Date_of_call) >= input$dateRange[1],
                  as.Date(sci_dump_temp$X_3_Date_of_call) <= input$dateRange[2]))
  })

datasetInput1 <- reactive({
  
  validate(
    need(input$district != "", "Please select one or more district")
  )
  
  sci_dump_temp1 <- sci_dump[sci_dump$District %in% input$district,]
  return(sci_dump_temp1)
  # return(filter(sci_dump_temp, 
  #               as.Date(sci_dump_temp$X_3_Date_of_call) >= input$dateRange[1],
  #               as.Date(sci_dump_temp$X_3_Date_of_call) <= input$dateRange[2]))
})
  
  
observeEvent(input$dateRange[1], {
    end_date = input$dateRange[2]
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if (input$dateRange[2] < input$dateRange[1]) {
      end_date = input$dateRange[1]
    }
    updateDateRangeInput(session,"dateRange", start=input$dateRange[1], end=end_date )
  })
  

########################CALCULATIONS  
 
 
  output$totalCalls <- renderValueBox({
    
     valueBox(
       format(nrow(datasetInput()),nsmall=0, big.mark=","), "Total Calls", icon = icon("phone-square"), color = "aqua"
    ) 
    
  })
  
  output$inboundCalls <- renderValueBox({
    
    valueBox(
      format(nrow(datasetInput()[datasetInput()$X_1_Call_Type == "inbound",]), nsmall=0, big.mark=","), "Inbound Calls", icon = icon("arrow-down"), color = "fuchsia"
    )  
    
  })
  
  output$outboundCalls <- renderValueBox({
    
    valueBox(
      format(nrow(datasetInput()[datasetInput()$X_1_Call_Type == "outbound",]), nsmall=0, big.mark=","), "Outbound Calls", icon = icon("arrow-up"), color = "orange"
    ) 
    
  })
  
  output$casesReferred <- renderValueBox({
    
    
    valueBox(
      format(nrow(filter(datasetInput(),X_23_1_Case_referred_=="yes",
                  X_13_Reason_of_the_call.grievance =="False")), nsmall=0, big.mark=","), 
      "Referred Cases", icon = icon("user-md"), color = "red"
    )
    
  })
  
  
######################################### for timeseries dygraph
  
  output$dailyCalls <- renderDygraph ({
    
    
    validate(
      need((input$dateRange[2] - input$dateRange[1]) > 1, "This graph will only show for dates with a difference of 2 or more days")
    )
    
    aa <- datasetInput() %>% 
      group_by(X_3_Date_of_call,X_1_Call_Type) %>% summarise(n=n())
    
    aa <- aa[c(-1),]
    aa_in <- filter(aa, X_1_Call_Type == "inbound") %>% select(X_3_Date_of_call,n)
    colnames(aa_in)[1] <- "Date"
    colnames(aa_in)[2] <- "Call_Nos_ib"
   
    x1 <- xts(aa_in, as.Date(aa_in$Date, format = "%Y-%m-%d")) 
    
    aa_out <- filter(aa, X_1_Call_Type == "outbound") %>% select(X_3_Date_of_call,n)
    colnames(aa_out)[1] <- "Date"
    colnames(aa_out)[2] <- "Call_Nos_ob"
    x2 <- xts(aa_out, as.Date(aa_out$Date, format = "%Y-%m-%d")) 
    
    max_range_yaxis <- max(max(aa_in$Call_Nos_ib),max(aa_out$Call_Nos_ob))
    arr_dis <- cbind(x1, x2)
    # arr_dis <- cbind(arr_dis, x0)
    
    # arr_dis[is.na(arr_dis)] <- ""
    
    
    
    dygraph(arr_dis
            # , main = "SDI Daily Helpline Calls"
            ) %>%
      dyCSS("dygraph.css") %>%
      dyAxis("y", label = "No. of calls",valueRange = c(0,max_range_yaxis+3)) %>%
      dyAxis("x", label = "Date" , drawGrid = TRUE) %>%
      dyOptions(
                colors = RColorBrewer::brewer.pal(2, "Spectral"),
                includeZero = TRUE,
                axisLineColor = "navy",
                gridLineColor = "lightblue",
                connectSeparatedPoints = TRUE,
                rightGap = 55
                # ,
                # drawGapEdgePoints = TRUE
                ) %>%
      dySeries("Call_Nos_ib", label = "Inbound") %>%
      dySeries("Call_Nos_ob", label = "Outbound") %>%
      # dySeries("Call_Nos_total", label = "Total") %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5),
                  highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE)%>%
      # dyRoller(rollPeriod = 1)%>%
      dyRangeSelector(height = 15, strokeColor = "")
    
    
    
  })
 
#################################################################################################################    
  
  
  # sketch1 <- htmltools::withTags(table(
  #   class = "display",
  #   style = "bootstrap",
  #   tableHeader(c("ID", colnames(get_dt())))
  #   # tableFooter(c("", c("",0,0)))
  # ))
  
 opts1 <- list( 
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    
    searchHighlight = TRUE,
    # columnDefs = list(list(targets = c(1:10), searchable = FALSE)),
    pageLength = 10,
    
    dom = 'lfrtiBp',
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
   )

 #################################################################################################################    
 
 get_dt <- reactive({

    sci_data <- datasetInput()
    sss <- as.data.table(sci_data)

    sss_temp <- sss[,c(13,14,16,7,8,10,17,41,42,44)]
    colnames(sss_temp)[3] <- "Village"
    colnames(sss_temp)[4] <- "Date"
    colnames(sss_temp)[5] <- "Time"
    colnames(sss_temp)[6] <- "Caller Name"
    colnames(sss_temp)[7] <- "Mobile"
    colnames(sss_temp)[8] <- "If Referred"
    colnames(sss_temp)[9] <- "Referred to"
    colnames(sss_temp)[10] <- "Referral Issue"

     return(sss_temp)

 })
  
#################################################################################################################    
 
  
 output$dtcallList = DT::renderDataTable(
    
       DT::datatable(get_dt() , 
                     # container = sketch1, 
                     options = opts1, 
                     selection = 'multiple',
                     class = 'cell-border stripe',
                     # caption = 'Table 1: SDI Helpline Call Details',
                     extensions = 'Buttons',
                     filter = 'bottom'
    )
  )
  

  #################################### "Month wise Call Summary" #############################################################################    
  
  dt_monthly <- reactive({
    
    ib <- NULL
    ob <- NULL
    ref <- NULL
    # format(as.Date(sci_dump$X_3_Date_of_call) , format="%b %Y") 
    month_all <-sort(unique(as.yearmon(datasetInput()$X_3_Date_of_call))) 
    # month_all <- unique(as.character(sort(factor((months(as.Date(datasetInput()$X_3_Date_of_call))),levels=month.name))))
    month_all <- as.character(month_all)
    # print(month_all)
    
    # for (i in 1:length(month_all))
    for (i in seq(1:length(month_all)))
    {
      ib[i] <- filter(datasetInput(),X_1_Call_Type =="inbound", as.yearmon(datasetInput()$X_3_Date_of_call) == month_all[i])%>%
        summarise(n=n())
      ob[i] <- filter(datasetInput(),X_1_Call_Type =="outbound", as.yearmon(datasetInput()$X_3_Date_of_call) == month_all[i])%>%
        summarise(n=n())
      
      # ref[i] <- filter(datasetInput(),X_23_1_Case_referred_=="yes",
      #                  X_13_Reason_of_the_call.grievance =="False",months(as.Date(datasetInput()$X_3_Date_of_call)) == month_all[i])%>%
      #   summarise(n=n())
      ref[i] <- filter(datasetInput(),X_23_1_Case_referred_=="yes",
                       X_13_Reason_of_the_call.grievance =="False",as.yearmon(datasetInput()$X_3_Date_of_call) == month_all[i])%>%
        summarise(n=n())
    }
    
    df <- cbind(month_all,ib,ob,ref)
    # print(df)
    
    return(df)
    
  })
  
  
output$barmonthlyCalls <- renderHighchart({
  
  xx <- dt_monthly()
  
  validate(
     need(length(unlist(xx[,"month_all"])) >1, "This graph needs dates across two months atleast. Intervals in the same month are not shown right now")
  )
    
    
    
    # print(length(unlist(xx[,"month_all"])))
    # print(unlist(xx[,"ib"]))
    # print(unlist(xx[,"ob"]))
    # print(unlist(xx[,"ref"]))
    
    hc <- highchart() %>% 
      hc_xAxis(categories = unlist(xx[,1])
               # ,
               # title = list(text = "Month")
               ) %>% 
      hc_add_series(name = "Inbound", data = unlist(xx[,2]), color = "fuchsia") %>% 
      hc_add_series(name = "Outbound", data = unlist(xx[,3]), color = "orange") %>% 
      hc_add_series(name = "Referral", data = unlist(xx[,4]), color = "red") %>%
      # hc_add_series(name = "Other city",
      #               data = (citytemp$tokyo + citytemp$london)/2)
      
      # hc_title(text = paste0("SDI Monthly Summary and Referrals"
      #                        # ,
      #                        # span(" (","N=",nrow(temp1),")", style="color:#e32c3e")
      #                        ),
      #          style = list(fontWeight = "bold")) %>%
      # hc_subtitle(text = "This histogram is based on the the recorded discharge time,from the hospital register") %>%

      hc_plotOptions(
        column = list(
          # colors = brewer.pal(3,"RdYlBu"),
          # type = "pie",
          # name = "No. of Calls",
          # colorByPoint = TRUE,
          # center = c('55%', '50%'),
          # size = 170,
          dataLabels = list(enabled = TRUE,
                            format = '{point.y}'
                            # format = '{point.name}: ({point.percentage:.1f}%)'
                            # {point.percentage:.1f}
          ))) %>%
      hc_yAxis(title = list(text = "Number of Calls"), allowDecimals = FALSE) %>%
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)%>%
      hc_chart(type = "column") 
    
      hc
      

    
    
  })
  
  
  
  
  #################################### "Time of Call" #############################################################################    
 
  
 output$histcallTime <- renderHighchart({
    
     sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    temp1 <- filter(sci_dump_temp,sci_dump_temp$X_4_Time_of_call !="" )
    thour_temp0 <- temp1$X_4_Time_of_call
    thour_temp1 <- gsub(":", ".", thour_temp0)
    thour_temp2 <- substr(thour_temp1, start = 1, stop = 5)
    thour <-as.numeric(thour_temp2)
    

    hchart(thour,color="#fca8c7",breaks=8)%>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Time of Call",span(" (","N=",format(nrow(temp1), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
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
      
      hc_plotOptions( 
        column = list(
                      colors = brewer.pal(10,"RdYlBu"),
                      # type = "pie",
                      # name = "No. of Calls", 
                      # colorByPoint = TRUE,
                      # center = c('55%', '50%'),
                      # size = 170, 
                      dataLabels = list(enabled = TRUE,
                                        format = '{point.y}'
                                        # format = '{point.name}: ({point.percentage:.1f}%)'
                                        # {point.percentage:.1f}
                      ))) %>% 
      hc_yAxis(title = list(text = "Number of Calls"), allowDecimals = FALSE) %>%
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
  })

###################################Call Status (all Calls)##############################################################################    
 
  output$histcallStatus <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    # temp1 <- filter(sci_dump_temp,sci_dump_temp$X_4_Time_of_call !="" )
    call_status <- sci_dump_temp$X_1_1_Outbound_Call_Status
    
    call_status[call_status == ""] <- "Inbound"
    call_status[call_status == "call_received"] <- "Outbound"
    call_status[call_status == "call_received_"] <- "Refused To Talk"
    call_status[call_status == "mobile_phone_n"] <- "Not Reachable"
    call_status[call_status == "mobile_phone_n_1"] <- "Switched Off"
    call_status[call_status == "call_not_recei"] <- "Not Received"
    
    
    hchart(call_status,type = "pie", name = "No. of callers")%>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Call Status (all Calls)",span(" (","N=",format(nrow(sci_dump_temp), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      # hc_subtitle(text = "This histogram is based on the the recorded discharge time,from the hospital register") %>%
      # hc_xAxis(title = list(text = "Call Time (Hours of Day)"),
      #          opposite = FALSE
      #          # plotLines = list(
      #          #   list(label = list(text = "Mean"),
      #          #        color = "#F4133C",
      #          #        width = 1.5,
      #          #        value = mean(dataset$Time.of.SHH.death.LAMA.Referred..Hour)))
      # ) %>%
      
      # hc_yAxis(title = list(text = "Number of Calls"), allowDecimals = FALSE) %>%
    
    hc_plotOptions( pie = list(colors = brewer.pal(6,"Set3"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('55%', '50%'),
                               size = 170, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
############################################Duration of Call#####################################################################    
  
  
  output$histDuration <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    temp1 <- filter(sci_dump_temp,sci_dump_temp$X_2_Duration_of_Call !="" )
    # thour <- temp1$X_2_Duration_of_Call
    # thour <-as.integer(gsub( ":.*$", "", thour ))
    
   
    
    hchart(as.integer(temp1$X_2_Duration_of_Call),color="#beffb8",breaks=15)%>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Duration of Call",span(" (","N=",format(nrow(temp1), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      # hc_subtitle(text = "This histogram is based on the the recorded discharge time,from the hospital register") %>%
      hc_xAxis(title = list(text = "Duration of call (mins)"),
               opposite = FALSE,
                plotLines = list(
                  list(label = list(text = paste0("Mean","(",
                                                  round(mean(as.integer(temp1$X_2_Duration_of_Call)),digits=1),
                                                  " mins",")")),
                       color = "#F4133C",
                       width = 1.0,
                       value = mean(as.integer(temp1$X_2_Duration_of_Call))))
      ) %>%
      
      hc_yAxis(title = list(text = "Number of Calls"), allowDecimals = FALSE) %>%
      
      hc_plotOptions( 
        column = list(
          # colors = brewer.pal(10,"RdYlBu"),
          # type = "pie",
          # name = "No. of Calls", 
          # colorByPoint = TRUE,
          # center = c('55%', '50%'),
          # size = 170, 
          dataLabels = list(enabled = TRUE,
                            format = '{point.y}'
                            # format = '{point.name}: ({point.percentage:.1f}%)'
                            # {point.percentage:.1f}
          ))) %>% 
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
######################################Who called: By Gender###########################################################################    
  
  
 output$histGender <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    temp2 <- filter(sci_dump_temp,sci_dump_temp$X_5_Sex_of_the_caller !="" )
    gender <- temp2$X_5_Sex_of_the_caller
    gender[gender == "male"] <- "Male"
    gender[gender == "female"] <- "Female"
    # thour <-as.integer(gsub( ":.*$", "", thour ))
    
    
    hchart(gender, type = "pie", name = "No. of callers") %>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Who called: By Gender",span(" (","N=",format(nrow(temp2), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      hc_plotOptions( pie = list(colors = brewer.pal(2,"YlOrBr"),
                                 # type = "pie",
                                 name = "No. of Calls", 
                                 colorByPoint = TRUE,
                                 # center = c('55%', '50%'),
                                 size = 260, 
                                 dataLabels = list(enabled = TRUE,
                                                   format = '{point.name}: ({point.percentage:.1f}%)'
                                                   # {point.percentage:.1f}
                                 ))) %>% 
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
  
  })

#########################################"Who Called: By Role########################################################################    
 
  output$histcallerType <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    
    temp3 <- filter(sci_dump_temp,sci_dump_temp$X_7_Type_of_caller !="" )
    caller_type <- temp3$X_7_Type_of_caller
    caller_type[caller_type == "father"] <- "Father"
    caller_type[caller_type == "family_member"] <- "Family Members"
    caller_type[caller_type == "anm"] <- "ANM"
    caller_type[caller_type == "_asha"] <- "ASHA"
    caller_type[caller_type == "mother"] <- "Mother"
    caller_type[caller_type == "children__18_years"] <- "Children above 18yrs"
    caller_type[caller_type == "aww"] <- "AWW"
    caller_type[caller_type == "_other"] <- "Others"
    caller_type[caller_type == "pregnant_women"] <- "Pregnant Women"
    
    labels <- unique(caller_type)
    value <- NULL

    for(i in 1:length(labels))
    {
      value[i] <- table(caller_type)[labels[i]]

    }
    
    # print(length(labels))
    # print(labels)
    # print(length(value))
    # print(value)
    
    # hchart(caller_type, type = "pie", name = "No. of callers") %>% 
    highchart() %>%  
    hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Who Called: By Role",span(" (","N=",format(nrow(temp3), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%

      hc_add_series_labels_values(labels, value,
                                  # colors = brewer.pal(9,"Set3"),
                                  type = "pie",
                                  name = "No. of Calls", 
                                  colorByPoint = TRUE,
                                  center = c('55%', '50%'),
                                  size = 190, 
                                  dataLabels = list(enabled = TRUE,
                                                    format = '{point.name}: ({point.percentage:.1f}%)'
                                                    # {point.percentage:.1f}
                                                    )) %>%
      
      hc_plotOptions( pie = list(colors = brewer.pal(9,"Pastel1")
                                  )) %>% 
      
      # hc_plotOptions(pie = list(
      #   dataLabels = list(enabled = TRUE)
      # )) %>% 
    # hc_plotOptions(
    #   dataLabels = list(enabled = TRUE,
    #                                  format = '{point.name}: {point.percentage:.1f} %')
    #   ) %>%
    
    hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
##########################################Why calls were made?#######################################################################    
  
  output$histcallerReason <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    
    temp4 <- filter(sci_dump_temp,sci_dump_temp$X_13_Reason_of_the_call.grievance == "True" | sci_dump_temp$X_13_Reason_of_the_call.information == "True" 
                    | sci_dump_temp$X_13_Reason_of_the_call.other == "True" )
    reason <- select(temp4, X_13_Reason_of_the_call.grievance, X_13_Reason_of_the_call.information,
                     X_13_Reason_of_the_call.other)
    
    # print(nrow(reason))
    
    colnames(reason)[1] <- "Report Grievance"
    colnames(reason)[2] <- "Get Information"
    colnames(reason)[3] <- "Others"
    
    reason[reason == "True"] <- 1
    reason[reason == "False"] <- 0
    reason$`Report Grievance` <- as.integer(reason$`Report Grievance`)
    reason$`Get Information` <- as.integer(reason$`Get Information`)
    reason$Others <- as.integer(reason$Others)
    
    reason_all <- c(rep("Report Grievance",sum(reason$`Report Grievance`)),rep("Health Information",sum(reason$`Get Information`))
                    , rep("Others",sum(reason$Others)))
    
    hchart(reason_all, type = "column", name = "No. of callers") %>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Why calls were made?",span(" (","N=",format(nrow(temp4), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
     
      # hc_plotOptions(
      #   
      #   column = list(
      #     colorByPoint = TRUE
      #   )
      # ) %>%
      
      hc_plotOptions( 
            column = list(colors = brewer.pal(3,"Set3"),
                                 # type = "pie",
                                 name = "No. of Calls", 
                                 colorByPoint = TRUE,
                                 # center = c('55%', '50%'),
                                 size = 170, 
                                 dataLabels = list(enabled = TRUE,
                                                   format = '{point.y}'
                                                   # format = '{point.name}: ({point.percentage:.1f}%)'
                                                   # {point.percentage:.1f}
                                 ))) %>% 
    hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      
    hc_exporting(enabled = TRUE)
    
    
  })
  
###################################What Health Information Sought?##############################################################################    
  

output$histinfoSought <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    
    temp5 <- filter(sci_dump_temp,sci_dump_temp$X_13_Reason_of_the_call.information == "True")
    # & sci_dump_temp$Type_of_information.diarrhoea == "True" )
    direason <- select(temp5, Type_of_information.diarrhoea, Type_of_information.other)
    
    colnames(direason)[1] <- "Diarrhoea"
    colnames(direason)[2] <- "Other Info"
    
    
    direason[direason == "True"] <- 1
    direason[direason == "False"] <- 0
    direason$Diarrhoea <- as.integer(direason$Diarrhoea)
    direason$`Other Info` <- as.integer(direason$`Other Info`)
    
    
    direason_all <- c(rep("Diarrhoea",sum(direason$Diarrhoea)),rep("Other Info",sum(direason$`Other Info`)))
    
    
    hchart(direason_all, type = "pie", name = "No. of callers") %>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("What Health Information Sought?",span(" (","N=",format(nrow(temp5), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      
      hc_plotOptions( pie = list(colors = brewer.pal(2,"Paired"),
                                 # type = "pie",
                                 name = "No. of Calls", 
                                 colorByPoint = TRUE,
                                 # center = c('55%', '50%'),
                                 size = 200, 
                                 dataLabels = list(enabled = TRUE,
                                                   format = '{point.name}: ({point.percentage:.1f}%)'
                                                   # {point.percentage:.1f}
                                 ))) %>% 
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
  
###########################################Diarrohea Health Information For?######################################################################    
  
  
  output$histinfoDiarrhea <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    
    temp6 <- filter(sci_dump_temp,sci_dump_temp$X_13_Reason_of_the_call.information == "True" & sci_dump_temp$Type_of_information.diarrhoea == "True" )
    
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
      hc_title(text = paste0("Diarrohea Health Information For?",span(" (","N=",format(nrow(temp6), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      
      hc_plotOptions( 
        column = list(colors = brewer.pal(3,"RdYlBu"),
                      # type = "pie",
                      name = "No. of Calls", 
                      colorByPoint = TRUE,
                      # center = c('55%', '50%'),
                      size = 170, 
                      dataLabels = list(enabled = TRUE,
                                        format = '{point.y}'
                                        # format = '{point.name}: ({point.percentage:.1f}%)'
                                        # {point.percentage:.1f}
                      ))) %>% 
      
      
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
    
  })
  
################################### Were Diarrohea Cases Referred ? ##############################################################################    
  
  output$histReferral <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    temp6 <- filter(sci_dump_temp,sci_dump_temp$X_13_Reason_of_the_call.information == "True" & 
                      sci_dump_temp$Type_of_information.diarrhoea == "True" )
    # temp7 <- filter(sci_dump_temp,sci_dump_temp$X_23_1_Case_referred_ != "")
    
    referral_all <- select(temp6, X_23_1_Case_referred_ ,Case_Referral)
    # referral_yes <- filter(referral_all, Case_Referral != "")   
    
    referral_all$X_23_1_Case_referred_ [referral_all$X_23_1_Case_referred_ == ""] <- "no"
    referral_all[referral_all == "yes"] <- "Yes"
    referral_all[referral_all == "no"] <- "No"
    
   
    hchart(referral_all$X_23_1_Case_referred_, type = "pie", name = "No. of callers") %>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Were Diarrohea Cases Referred ?",span(" (","N=",format(nrow(temp6), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
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
      
      hc_plotOptions( pie = list(colors = brewer.pal(2,"Set3"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('55%', '50%'),
                               size = 240, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
    
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
    
    
  })  
    
###################################### Diarrohea Cases referred To ? ###########################################################################    
  
  output$histreferredTo <- renderHighchart({
    
    sci_dump_temp <- datasetInput()
    # sci_dump_temp <- filter(datasetInput(), X_1_Call_Type == "inbound")
    temp7 <- filter(sci_dump_temp,sci_dump_temp$X_13_Reason_of_the_call.information == "True" & 
                      sci_dump_temp$Type_of_information.diarrhoea == "True" )
    
    referral_all <- select(temp7, X_23_1_Case_referred_ ,Case_Referral)
    # referral_all$Case_Referral [referral_all$Case_Referral == ""] <- "other"
    
    referral_yes <- filter(referral_all, Case_Referral != "")   
    
    referral_yes[referral_yes == "chc"] <- "CHC"
    referral_yes[referral_yes == "phc"] <- "PHC"
    referral_yes[referral_yes == "anm"] <- "ANM"
    referral_yes[referral_yes == "asha"] <- "ASHA"
    referral_yes[referral_yes == "other"] <- "Other"
    
    hchart(referral_yes$Case_Referral, type = "pie", name = "No. of callers") %>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Diarrohea Cases referred To ?",span(" (","N=",format(nrow(referral_yes), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
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
      hc_plotOptions( pie = list(colors = brewer.pal(5,"Pastel1"),
                                 # type = "pie",
                                 name = "No. of Calls", 
                                 colorByPoint = TRUE,
                                 # center = c('55%', '50%'),
                                 size = 230, 
                                 dataLabels = list(enabled = TRUE,
                                                   format = '{point.name}: ({point.percentage:.1f}%)'
                                                   # {point.percentage:.1f}
                                 ))) %>% 
    
    
     hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
   
    
  })  
    
######################################REFERRAL TAB CHARTS START HERE ###########################################################################    
  

output$pieCallStatus <- renderHighchart({
  
  d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")
  
  hchart(d_referral_cases$ref_followup_status, type = "pie", name = "No. of callers") %>% 
    hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = paste0("Referral Follow-up Call Status",span(" (","N=",format(nrow(d_referral_cases), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
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
    hc_plotOptions( pie = list(colors = brewer.pal(5,"Pastel1"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('55%', '50%'),
                               size = 180, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
    
    
    hc_credits(enabled = TRUE,
               text = "Source: SCI SDI Helpline data,2017-18",
               href = "https://www.savethechildren.in/",
               style = list(fontSize = "9px")) %>%
    hc_exporting(enabled = TRUE)
  

}) 

output$pieCasesStatus <- renderHighchart({
  
  d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")
  
  hchart(filter(d_referral_cases, d_referral_cases$ref_followup_status == "Received")$ref_patient_status, type = "pie", name = "No. of callers") %>% 
    hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = paste0("Current Status of Referred Cases",span(" (","N=",format(nrow(filter(d_referral_cases, d_referral_cases$ref_followup_status == "Received")), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
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
    hc_plotOptions( pie = list(colors = brewer.pal(5,"Pastel1"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('55%', '50%'),
                               size = 180, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
    
    
    hc_credits(enabled = TRUE,
               text = "Source: SCI SDI Helpline data,2017-18",
               href = "https://www.savethechildren.in/",
               style = list(fontSize = "9px")) %>%
    hc_exporting(enabled = TRUE)
  
  
}) 


output$colTreatmentGiven <- renderHighchart({
  
  d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")
  treatment_list_string <- paste(filter(d_referral_cases, d_referral_cases$ref_followup_status == "Received")$treatment_given,collapse=",")
  treatment_list_vec <- strsplit(treatment_list_string, ",")[[1]]
  treatment_list_vec <- trimws(treatment_list_vec, which = c("both"))
  # treatment_list_vec[treatment_list_vec == " anti diarrheal dose"] <- "Anti Diarrhoeal Dose"
  # treatment_list_vec[treatment_list_vec == "Antibiotic" | treatment_list_vec == " Antibiotic" ] <- "Antibiotics"
  # treatment_list_vec[treatment_list_vec ==  "ORS - Zinc" | treatment_list_vec == "ORS – Zinc"
  #                    | treatment_list_vec == "ORS Antibiotic"| treatment_list_vec == "ORS - Antibiotic"
  #                    | treatment_list_vec ==  " ORS - Zinc" ] <- "ORS+Zinc"
  treatment_list_vec <- treatment_list_vec[treatment_list_vec!= ""]
  

 hchart(treatment_list_vec, type = "column", name = "No. of Cases") %>%
    hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = paste0("What Treatment was Taken?",span(" (","N=",format(nrow(filter(d_referral_cases, d_referral_cases$ref_followup_status == "Received")), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
             style = list(fontWeight = "bold")) %>%

    # hc_plotOptions(
    #
    #   column = list(
    #     colorByPoint = TRUE
    #   )
    # ) %>%

    hc_plotOptions(
      column = list(colors = brewer.pal(5,"Accent"),
                    # type = "pie",
                    name = "No. of Cases",
                    colorByPoint = TRUE,
                    # center = c('55%', '50%'),
                    size = 170,
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.y}'
                                      # format = '{point.name}: ({point.percentage:.1f}%)'
                                      # {point.percentage:.1f}
                    ))) %>%
    hc_credits(enabled = TRUE,
               text = "Source: SCI SDI Helpline data,2017-18",
               href = "https://www.savethechildren.in/",
               style = list(fontSize = "9px")) %>%

    hc_exporting(enabled = TRUE)


})

output$histRecoveryDays <- renderHighchart({
  
  d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")
  
  t1 <- as.Date(filter(d_referral_cases, d_referral_cases$ref_patient_status == "Recovered")$ref_date, format = "%d/%m/%Y" )
  t2 <- as.Date(filter(d_referral_cases, d_referral_cases$ref_patient_status == "Recovered")$recovery_date, format = "%d/%m/%Y" )
  

  hchart(as.integer(t2-t1),color="#fca8c7",breaks=10)%>%
    hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = paste0("Days Taken for Recovery",span(" (","N=",format(nrow(filter(d_referral_cases, d_referral_cases$ref_patient_status == "Recovered")), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
             style = list(fontWeight = "bold")) %>%
    # hc_subtitle(text = "This histogram is based on the the recorded discharge time,from the hospital register") %>%
    hc_xAxis(title = list(text = "Duration(in Days)"),
             opposite = FALSE
             # plotLines = list(
             #   list(label = list(text = "Mean"),
             #        color = "#F4133C",
             #        width = 1.5,
             #        value = mean(dataset$Time.of.SHH.death.LAMA.Referred..Hour)))
    ) %>%

    hc_plotOptions(
      column = list(
        colors = brewer.pal(10,"RdYlBu"),
        # type = "pie",
        # name = "No. of Calls",
        # colorByPoint = TRUE,
        # center = c('55%', '50%'),
        # size = 170,
        dataLabels = list(enabled = TRUE,
                          format = '{point.y}'
                          # format = '{point.name}: ({point.percentage:.1f}%)'
                          # {point.percentage:.1f}
        ))) %>%
    hc_yAxis(title = list(text = "Number of Cases"), allowDecimals = FALSE) %>%

    hc_credits(enabled = TRUE,
               text = "Source: SCI SDI Helpline data,2017-18",
               href = "https://www.savethechildren.in/",
               style = list(fontSize = "9px")) %>%
    hc_exporting(enabled = TRUE)


})

output$dtCrosstab <- renderTable ({
  
  my_tbl = addmargins(table(one_group, another_group))
  printed_tbl = as.data.frame.matrix(my_tbl)
  print_tbl
}, include.rownames=TRUE)



#################################### TREE DIAGRAM ###############################################

output$dTree <- renderCollapsibleTree({
  
  d_referral_cases <- inner_join(datasetInput(),d_referral_cases , by= "uuid")
  
   vec <- c("ref_followup_status_1","ref_patient_status_1","treatment_given_1",
            "ref_followup_status_2","ref_patient_status_2","treatment_given_2",
            "ref_followup_status_3","ref_patient_status_3","treatment_given_3",
            "ref_followup_status_4","ref_patient_status_4","treatment_given_4")
    
   d_referral_cases %>%
     
      group_by_(.dots = vec) %>%
      summarise(`Cases` = n()) %>%
                # ,`Percentage of Cases`= n()*100/nrow(datasetInput()) ) %>%
      collapsibleTreeSummary(
        # hierarchy = c("Arrived from ?","Mode of Transportation used","Maternal complication at the time of arrival",
        # "Arrival Point at DH","Maternal complication during the stay at DH","Outcome of Delivery","Outcome of Mother"),
        hierarchy =  c("ref_followup_status_1","ref_patient_status_1","treatment_given_1",
                       "ref_followup_status_2","ref_patient_status_2","treatment_given_2",
                       "ref_followup_status_3","ref_patient_status_3","treatment_given_3",
                       "ref_followup_status_4","ref_patient_status_4","treatment_given_4"),
        # c("ref_followup_status","ref_patient_status","treatment_given"),
        # hierarchy = input$hierarchy,
        root = "Ref Cases",
        width = 1200,
        attribute = "Cases",
        # nodeSize = "Cases",
        # zoomable = FALSE,
        maxPercent = 50
        # fill = c(
        #   # The root
        #   "seashell",
        #   # Unique regions
        #   rep("brown", length(unique(d_referral_cases$ref_followup_status_1))),
        #   # Unique classes per region
        #   rep("khaki", length(unique(paste(d_referral_cases$ref_followup_status_1,d_referral_cases$ref_patient_status_1)))),
        #   # Unique names per region
        #   rep("forestgreen", length(unique(paste(species$NAME, species$REGION))))
        # )
      )
   
})

######################## TREE ENDS ##############################################################################


    
  
  
  
  
  
  
})