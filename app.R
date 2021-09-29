##############################################################################
# Purpose: POINT data visualization
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
# Dashboard help: 
# https://rstudio.github.io/shinydashboard/structure.html
#
# Adding text to your shiny app
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
#
# Icons:
# https://fontawesome.com/v4.7.0/icons/
##############################################################################
# Import libraries
library(shiny) # for dashboard
library(shinydashboard) # for tabs
library(dplyr) # filtering
library(RColorBrewer)
#library(haven)
#library(DT) # for displaying tables
library(ggplot2) # for figures
#library(data.table)
#library(reshape2) # for data manipulation
library(tools)
library(tidyverse) # adds to title case
library(scales) # add comma to output
library(googlesheets4) # for reading data

##############################################################################
# LOAD DATA
#=============================================================================
#gs4_auth()
gsheet_link <- "https://docs.google.com/spreadsheets/d/1L9wrys7FT2FqWOhBSWbctMw0F_5GvllrnHdgo1C9Fdk/edit?usp=sharing"
gs4_deauth()
df <- read_sheet(gsheet_link)
#df <- read.csv("data/covid_cases_nsw - Sheet1.csv")
df$date <- as.Date(df$date, format = "%Y-%m-%d")

#-----------------------------------------------------------------------------
# Create variables
date_latest <- max(df$date)
date_earliest <- max(df$date)-13

# dates for first and second doses are slow, so if current date is missing use previous dates data
date_latest_doses <- date_latest
while(is.na(df$doses_1st_24hr[df$date == date_latest_doses])){
  date_latest_doses <- date_latest-1
}

#-----------------------------------------------------------------------------
# Create rate variable

# dose rate
df <- df %>%
  mutate(
    dose_1st_rate = dose_1st_percent/lag(dose_1st_percent),
    dose_2nd_rate = dose_2nd_percent/lag(dose_2nd_percent)
  )

#-----------------------------------------------------------------------------
# rename variables for nicer look
df_plot <- data.frame(df)
new <- names(df_plot)
new <- toTitleCase(str_replace_all(new, "_", " "))
names(df_plot) <- new

#-----------------------------------------------------------------------------
## TIDY DATA
# date, variable, value
df_plot <- reshape2::melt(df_plot, id.var = "Date")
df_plot$Date <- as.Date(df_plot$Date, format = "%Y-%m-%d")
df_plot$variable <- as.factor(df_plot$variable)
df_plot$value <- as.numeric(df_plot$value)

# vaccine increase from previous date
vaccinations_today <- df$doses_total_NSW[df$date == date_latest]
vaccinations_yesterday <- df$doses_total_NSW[df$date == date_latest-1]

# calculate the rate
dose_1st_7d_rate <- round(mean(df$dose_1st_rate[df$date > (date_latest - 7)]),2)
dose_2nd_7d_rate <- round(mean(df$dose_2nd_rate[df$date > (date_latest - 7)]),2)

# based on current rate how many days until 80%? (Current % / rate)
dose_1st_90p_prediction <- (90 - df$dose_1st_percent[df$date == date_latest])/dose_1st_7d_rate
dose_2nd_80p_prediction <- (80 - df$dose_2nd_percent[df$date == date_latest])/dose_2nd_7d_rate


# configuration
df_vars <- colnames(df)
size_line=2
size_point=4
value_box_width=4
row_1_col = 'light-blue'
row_2_col = 'light-blue'

##############################################################################
# functions for plots
#source("functions/utilities.R")

##############################################################################
# TAB 1: USER INTERFACE
##############################################################################
# Define UI for application that draws a histogram
ui <- dashboardPage(
  #=========================================================================
  # START DASHBOARD
  #=========================================================================
  # MAIN TITLE
  dashboardHeader(title =  "COVID-19 Tracker NSW",
                  titleWidth = 300,
                  tags$li(class="dropdown",
                          tags$a(href='https://github.com/philliphungerford/covid-tracker-au-nsw', icon('github'), "Source Code", target="_blank")),
                  tags$li(class="dropdown",
                          tags$a(href='https://philliphungerford.github.io', icon('globe'), "Learn More", target="_blank"))
  ),
  #=========================================================================
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # icons from (https://fontawesome.com/v4.7.0/icons/)
      menuItem("Overview", tabName = "overview", icon = icon("desktop")),#
      menuItem("Breakdown", tabName = "breakdown", icon = icon("bar-chart")),#
      menuItem("Vaccinations", tabName = "vaccinations", icon = icon("medkit")),#
      menuItem("Information", tabName = "information", icon = icon("info"))#
    )
  ),
  #=========================================================================
  ## Body content
  dashboardBody(
    # make background white
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                '))),
    value = tags$p("my info box message", style = "font-size: 50%;"),
    
    tabItems(
      #-----------------------------------------------------------------
      # TAB 1: Overview
      tabItem(tabName = "overview",
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 1 - First 4 boxes
              
              fluidRow(
                
                column(width=3,
                       valueBox(
                         value =  format(date_latest, "%a %b %d"),
                         "Date updated",
                         icon = icon("calendar-o"),
                         color = row_1_col,
                         width = NULL)),
                
                column(width=3,
                       valueBox(
                         value = paste0(df$dose_1st_percent[df$date == date_latest],"%"),
                         "Have received their 1st Dose",
                         icon = icon("male"),
                         color = row_1_col,
                         width = NULL)),
                
                column(width=3,
                       valueBox(
                         value = paste0(dose_1st_7d_rate,"%"),
                         "1st Dose Daily Rate (7 day average)",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL)),
                
                column(width=3,
                       valueBox(
                         value = round(dose_1st_90p_prediction,0),
                         "Days until 90% first dose (prediction based on 7d avg)",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL))
                
                ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 2 - Second row of 4 boxes
              
              fluidRow(
                
                column(width=3,
                       valueBox(
                         value =  format((date_latest + dose_2nd_80p_prediction), "%a %b %d"),
                         "Predicted date until 80% double dose",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL)),

                column(width=3,
                       valueBox(
                         value = paste0(df$dose_2nd_percent[df$date == date_latest],"%"),
                         "Have received their 2nd Dose",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL)),
               
                column(width=3,
                      valueBox(
                        value = paste0(dose_2nd_7d_rate,"%"),
                        "2nd Dose Daily Rate (7 day average)",
                        icon = icon("line-chart"),
                        color = row_1_col,
                        width = NULL)),
               
                column(width=3,
                       valueBox(
                         value = round(dose_2nd_80p_prediction,0),
                         "Days until 80% double dose (prediction based on 7d avg)",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL))
               
               ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 3 - Checkbox and Plot
              
              fluidRow(
                
                column(width=3,
                           # Input: Selector for variable to plot against mpg ----
                           checkboxGroupInput(inputId = "variable",
                                       label = "Variable:", 
                                       choices = 
                                         list(
                                           "Percent 1st Dose" = "Dose 1st Percent",
                                           "Percent 2nd Dose" = "Dose 2nd Percent",
                                           "Percent 1st Dose Rate" = "Dose 1st Rate",
                                           "Percent 2nd Dose Rate" = "Dose 2nd Rate",
                                           "New Cases total (24hrs)" = "Total New Cases",
                                           "New local cases (24hrs)" = "Num New Cases",
                                           "Overseas Acquired (24hrs)" = "Overseas Acquired",
                                           "Died (24hrs)" = "Died",
                                           "Tests (24hrs)" = "Tests 24hrs",
                                           "Hospitalised (24hrs)"= "Hospitalised",
                                           "Total Cases since Jan 25 2020" = "Cases 20200125",
                                           "Total cases since recent outbreak ( Jun 16 2021)" = "Cases 20210616 Outbreak",
                                           "Total Tests" = "Total Tests",
                                           "In hospital" = "In Hospital",
                                           "In ICU" = "Icu",
                                           "Total deaths" = "Deaths",
                                           "On ventilator" = 'Ventilator',
                                           
                                           # vaccinations
                                           "Doses: Total for NSW" = "Doses Total NSW",
                                           "Doses: Total from NSW Health" = "Doses Total nswHealth Cum",
                                           "Doses: Total from GP Network" = "Doses Total Gp Cum",
                                           "Doses: First cumulative" = "Doses 1st Cum",
                                           "Doses: Second cumulative" = "Doses 2nd Cum", 
                                           "Doses: First dose rate per 24hrs (NSW Health)" = "Doses 1st 24hr",
                                           "Doses: Second dose rate per 24hrs (NSW Health)" = "Doses 2nd 24hr",
                                           "Doses: Total dose rate in past 24hr (NSW Health)" = "Doses Total 24hr"
                                         ),
                                       selected = c("Dose 1st Percent","Dose 2nd Percent") , width = NULL),
                           
                           # Pass in Date objects
                           dateRangeInput("dateRange", "Date range:",
                                          start = date_latest-6,
                                          end = date_latest,
                                          min = "2021-08-01",
                                          max = date_latest,
                                          width = NULL)
                       ),
                
                column(width=9,
                           plotOutput(outputId = "graph_1", width = NULL, height=600)
                       ),
 
                

              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 4 - Notes
              
              fluidRow(
                
                column(width=12,
                       p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx"),". Made by Phillip Hungerford. For more details visit my ", a("website", href="https://philliphungerford.github.io"), ".")
                       
                       )
              )
          ),
      
      #-----------------------------------------------------------------
      # TAB 2: Breakdown
      tabItem(tabName = "breakdown",
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("COVID-19 Tracker NSW"),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health.", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx")),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 1 - Date
              
              h1("Date"),

              
              fluidRow(
                
                column(width=3,
                  valueBox(
                    value =  format(date_latest, "%a %b %d"),
                    "Date updated",
                    icon = icon("calendar-o"),
                    color = "red",
                    width=NULL)),
                
                column(width=3,
                  valueBox(
                    value =  date_latest - as.Date("2021-06-25"),
                    "Days since June 25th lockdown",
                    icon = icon("calendar-o"),
                    color = "red",
                    width=NULL))
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 2 - Overview
              
              h1("Overview"),
              
              fluidRow(
                
                column(width=3,
                      valueBox(
                        value = comma(sum(df$cases_20200125[which(df$date == date_latest)])),
                        "Total COVID cases since pandemic",
                        icon = icon("male"),
                        color = "red",
                        width=NULL)),
                
                column(width=3,
                      valueBox(
                        value = comma(sum(df$num_new_cases[which(df$date >= date_earliest)])),
                        "Total cases in past 14 days",
                        icon = icon("male"),
                        color = "red",
                        width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = comma(df$doses_total_NSW[df$date == date_latest]),
                         "Total Vaccinations in NSW",
                         icon = icon("male"),
                         color = "red",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = comma(vaccinations_today - vaccinations_yesterday),
                         "Difference in vaccines from yesterday",
                         icon = icon("line-chart"),
                         color = "red",
                         width=NULL))
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 3 - Deaths
              
              fluidRow(
                
                column(width=3,
                       valueBox(
                         value = df$deaths[df$date == date_latest],
                         "Total deaths",
                         icon = icon("user-times"),
                         color = "red",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = comma(df$total_tests[df$date == date_latest]),
                         "Total tests",
                         icon = icon("thermometer"),
                         color = "red",
                         width=NULL))
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 4 - Past 24 hours
              
              h1("Past 24hrs from 8pm last night:"),

              fluidRow(
                
                column(width=3,
                       valueBox(
                         value = df$num_new_cases[df$date == date_latest],
                         "New Cases",
                         icon = icon("male"),
                         color = "red",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = df$in_hospital[df$date == date_latest],
                         "Currently in hospital",
                         icon = icon("hospital-o"),
                         color = "red",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = df$icu[df$date == date_latest],
                         "Currently in ICU",
                         icon = icon("bed"),
                         color = "red",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value = df$ventilator[df$date == date_latest],
                         "Currently on a ventilator",
                         icon = icon("heartbeat"),
                         color = "red",
                         width=NULL))
                
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Notes
              
              br(),
              br(),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx"),". Made by Phillip Hungerford. For more details visit my ", a("website", href="https://philliphungerford.github.io"), ".")
      ),
      #-----------------------------------------------------------------
      # TAB 3: Vaccinations
      tabItem(tabName = "vaccinations",
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("COVID-19 Tracker NSW"),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health.", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx")),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 1 - Date
              
              h1("Date"),
              
              fluidRow(
                
                column(width=3,
                       valueBox(
                         value =  format(date_latest, "%a %b %d"),
                         "Date updated",
                         icon = icon("calendar-o"),
                         color = "green",
                         width=NULL)),
                
                column(width=3,
                       valueBox(
                         value =  date_latest - as.Date("2021-06-25"),
                         "Days since June 25th lockdown",
                         icon = icon("calendar-o"),
                         color = "green",
                         width=NULL))
              ),
              
              fluidRow(
              
                ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 2 - Total dose breakdown
              
              h1("Vaccinations Overview"),
              
              fluidRow(

                column(width=3,
                  valueBox(
                    value = comma(df$doses_total_NSW[df$date == date_latest]),
                    "Total Doses administered in NSW",
                    icon = icon("medkit"),
                    color = "green",
                    width=NULL)),
                
                column(width=3,
                  valueBox(
                    value = comma(df$doses_total_nswHealth_cum[df$date == date_latest]),
                    "Total Doses administered from NSW Health",
                    icon = icon("medkit"),
                    color = "green",
                    width=NULL)),
                
                column(width=3,
                  valueBox(
                    value = comma(df$doses_total_gp_cum[df$date == date_latest]),
                    "Total Doses administered from GP network",
                    icon = icon("medkit"),
                    color = "green",
                    width=NULL)),
                
                column(width=3,
                   valueBox(
                     value = comma(vaccinations_today - vaccinations_yesterday),
                     "Difference in vaccines from yesterday",
                     icon = icon("line-chart"),
                     color = "green",
                     width=NULL))
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 3 - Date of doses data
              h1("Vaccinations by dose"),
              p("These are vaccinations administered by NSW Health"),
              
              fluidRow(
                column(width=3,
                       valueBox(
                         value =  format(date_latest_doses, "%a %b %d"),
                        "Date updated",
                        icon = icon("calendar-o"),
                        color = "green",
                        width=NULL))
                
                ),

              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 4 - First dose background
              
              fluidRow(
                
                column(width=3,
                       valueBox(
                         value = comma(df$doses_1st_24hr[df$date == date_latest_doses]),
                        "First Dose administered in 24hrs",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL)),
                
                column(width=3,
                      valueBox(
                        value = comma(df$doses_2nd_24hr[df$date == date_latest_doses]),
                        "Second Dose administered in 24hrs",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL)),
                
                column(width=3,
                      valueBox(
                        value = comma(df$doses_total_24hr[df$date == date_latest]),
                        "Total Doses administered in 24hrs",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL))
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              ## Row 5 - Cumulative
              
              fluidRow(

                column(width=3,
                      valueBox(
                        value = comma(df$doses_1st_cum[df$date == date_latest_doses]),
                        "First Dose administered",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL)),
                
                column(width=3, 
                      valueBox(
                        value = comma(df$doses_2nd_cum[df$date == date_latest_doses]),
                        "Second Dose administered",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL)),
                
                column(width=3,      
                      valueBox(
                        value = comma(df$doses_total_nswHealth_cum[df$date == date_latest]),
                        "Total Doses administered from NSW Health",
                        icon = icon("medkit"),
                        color = "green",
                        width=NULL))
              ),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx"),". Made by Phillip Hungerford. For more details visit my ", a("website", href="https://philliphungerford.github.io"), ".")
      ),
      
      #-----------------------------------------------------------------
      # TAB 3: Information
      tabItem(tabName = "information",
              h2("Information"),
              p("Made by Phillip Hungerford. For more details visit my:", a("website", href="https://philliphungerford.github.io")),
              p("If you would like to make a request, email me on phillip.hungerford@gmail.com")
      )
      #-----------------------------------------------------------------
      
    ),
    # tabItems
  ) # body
  #=========================================================================
  # END DASHBOARD
  #=========================================================================
) # dashboard page

##############################################################################
# TAB 2: SERVER
##############################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  #=========================================================================
  # Start server
  #=========================================================================
  # set seed for replicability
  set.seed(122)
  # PLOT 3: BPI plot
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  
  output$graph_1 <- renderPlot({
    
    tmp_vars <- c(input$variable, input$variable2)
    
    # select variables that have been checked in box
    tmp <- df_plot[df_plot$variable %in% tmp_vars,]
    
    # filter data by date
    tmp <- tmp %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    # generate plot
    ggplot(data = tmp, aes(x = Date, y = value, color = variable)) + 
      geom_line(size=size_line) +
      geom_point(size=size_point) +
      scale_x_date(date_labels="%d %b",date_breaks  ="1 day") + 
      labs(x = "Date",
           y = "Number of Cases",
           title="Trend over time") +
      scale_color_brewer(palette = 'Blues') + 
      theme_classic() +
      theme(legend.position = "bottom",
            legend.title=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    
  })
  
  #=========================================================================
  # End server
  #=========================================================================
}

##############################################################################
# RUN APPLICATION
##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################
################################### END ######################################
##############################################################################