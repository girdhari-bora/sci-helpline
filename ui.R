
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
# library(ggplot2)
# library(ggthemes)
# library(scales)
# library(magrittr)
# library(scales)
# library(lattice)
library(dplyr)
# library(shiny)
# library(leaflet)
library(RColorBrewer)
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
library(dygraphs)
library(xts)
# library(magrittr)
library(highcharter)
library(dplyr)
library(DT)
library(data.table)
library(shinyjs)
library(htmltools)
# library(lubridate)
# library(dygraphs)
# library(tidyr)
# require(colorspace)
# library(lazyeval)
# library(shinythemes)
# library(ggvis)
library(apputils)
library(collapsibleTree)

# detach("package:plyr", unload=TRUE) 
# testData <- read.csv("./Data/PrePostScienceMaths2015.csv",header = TRUE)
# #testData4 <- read.csv("./Data/Attendance.csv",header = TRUE)
# testData4 <- read.csv("./Data/Attendance_Consolidated1.csv",header = TRUE)
# schoolName <- read.csv("./Data/schoolName.csv",header = TRUE)
# #testData5 <- read.csv("./Data/PrePostHindi2015.csv",header = TRUE)
# StudentMaster <- read.csv("./Data/StudentProfileData.csv",header = T, na.strings = c("NA",""," "))


# css <- "
# #large .selectize-input { line-height: 35px; }
# #large .selectize-dropdown { line-height: 25px; }"








#############READ DATA FILES INTO DATAFRAMES

# sci_dump1 <- read.csv("./Data/sci_dump.csv", 
#                       header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
# sci_dump1[sci_dump1 == "n/a"] <- ""
# 
# sci_dump <- sci_dump1[-c(12,16,20:25,56,97:109,114,122:124,130:131,214,215,235,237,239,240,244,245
#                          ,247,260:262,267,269:310,326,510),]
# sci_dump$District[sci_dump$District == ""] <- "Other"

####################


#sidebar and related functions

dbHeader <- dashboardHeader(title = "SDI Helpline Dashboard", titleWidth = 250,
                            # tags$li(a(href = 'http://shinyapps.company.com',
                            #           icon("power-off"),
                            #           title = "Back to Apps Home"),
                            #         class = "dropdown"),
                            tags$li(a(href = 'https://www.savethechildren.in/',
                                      img(src = 'favicon.png',
                                          title = "Save the Children Website", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:3px;"),
                                    class = "dropdown"))

sidebar <- dashboardSidebar(
  
  
  sidebarMenu( id = "sidebarmenu",  
               
              tags$head(
                 tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}
                                 .selectize-dropdown .active {background: yellow !important;}"))
                 ),
               
               selectInput('district', 'District', c("Bahraich","Balrampur", "Pilibhit", "Shrawasti","Other"), selected = c("Bahraich","Balrampur", "Pilibhit", "Shrawasti","Other"),multiple=TRUE, selectize=TRUE),
              
               dateRangeInput('dateRange',
                             label = "Period (since start by default)",
                             start = min(as.Date(sci_dump$X_3_Date_of_call), na.rm = TRUE),
                             end = max(as.Date(sci_dump$X_3_Date_of_call), na.rm = TRUE),
                             min = min(as.Date(sci_dump$X_3_Date_of_call), na.rm = TRUE),
                             max = max(as.Date(sci_dump$X_3_Date_of_call), na.rm = TRUE),
                             separator = " to ", format = "yyyy-mm-dd ",
                             startview = 'month', language = 'en', weekstart = 1
                        ),
               
               br(),
               # menuItem("Referral Tree", tabName = "referralTree", icon = icon("user-md")),
               
               menuItem("Daily Call Records", tabName = "dailyCalls", icon = icon("table")),
               br(),
               
               # convertMenuItem(menuItem("Referral Tree", tabName = "referralTree", icon = icon("user-md"),selected = FALSE,
               #                          menuSubItem(text = 'Percentage or Numbers', tabName='submitFilters')),"referralTree"),
               # 
               # conditionalPanel("input.sidebarmenu == 'submitFilters'", #fixed
               #                  # sliderInput("b", "Under sidebarMenu", 1, 100, 50),
               #                  selectInput('label', 'Label Attribute', c("Number of Cases","Percentage of Cases"), multiple=FALSE, selectize=TRUE)
               # ),
               # 
               # br(), 
               
               menuItem("Key Indicators (all cases)", tabName = "indicators", icon = icon("line-chart")),
                br(),
               menuItem("Diarrhoea Referral Cases", tabName = "referral", icon = icon("medkit")),
                br()
               
               ),
  
  dashboard_footer(src = ' ' , href = "https://www.tattvafoundation.org/",label = "Analytics support: Tattva Foundation", width = "100%", height = "50px",
                                italic = TRUE, bold = TRUE,
                  style = "text-align:center;align: center; padding: 0px; margin: 0px;")
  
  
)


#dashboard main body related functions

body <- dashboardBody ( 
 
  tags$head(tags$link(rel = "icon", href = "./www/favicon.ico?v=1.1")),
  
 
  tabItems(
    
    
    tabItem(tabName = "dailyCalls", 
            
            fluidRow(
              valueBoxOutput("totalCalls" , width = 3),
              valueBoxOutput("inboundCalls", width = 3),
              valueBoxOutput("outboundCalls",width = 3),
              valueBoxOutput("casesReferred",width = 3)
            ),
            
            
            fluidRow(
              box(dygraphOutput("dailyCalls",height = "400px"), width = 12,collapsible = TRUE,
                  status = "success",solidHeader = TRUE,title = "SDI Daily Helpline Calls"),
              box(highchartOutput("barmonthlyCalls", width = "100%", height = "350px"), width = 12,collapsible = TRUE,
                  status = "success",solidHeader = TRUE,title = "SDI Monthly Call Summary and Referrals"),
              box(title = "Table: Helpline Call Details", DT::dataTableOutput('dtcallList'), width = 12,collapsible = TRUE, collapsed = TRUE,
                  status = "success",solidHeader = TRUE)
             
            )), 
    
    
    tabItem(tabName = "indicators", 
            
            #         fluidRow(
            #           
            #           valueBoxOutput("capsularCourses"),
            #           valueBoxOutput("studentsServed"),
            #           valueBoxOutput("hoursTaught")
            #         ),
            #         
            fluidRow(
              box(highchartOutput("histcallTime", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histcallStatus", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histDuration", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histGender", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histcallerType", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histcallerReason", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              
              
             
              box(highchartOutput("histinfoSought", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histinfoDiarrhea", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              
              box(highchartOutput("histReferral", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histreferredTo", width = "100%", height = "350px"), width = 6,collapsible = TRUE)
              
            )
            
           
    ),
    
    tabItem(tabName = "referral", 
         
            fluidRow(
              box(collapsibleTreeOutput("dTree", width = "100%", height = "350px"), width = 12,
                 title = "Diarrohea Referral Tree (click on the nodes to drill-down)", collapsible = TRUE, status = "success",solidHeader = TRUE),
              box(highchartOutput("pieCallStatus", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("pieCasesStatus", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("colTreatmentGiven", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("histAgeAtRecovery", width = "100%", height = "350px"), width = 6,collapsible = TRUE),
              box(highchartOutput("scatterAgeRecovTreat", width = "100%", height = "350px"), width = 6,collapsible = TRUE)
            )
      )
    
    
  )
  
  
)





ui<- dashboardPage( title="Save The Children",
  
  dbHeader, 
  sidebar,
  body
)





