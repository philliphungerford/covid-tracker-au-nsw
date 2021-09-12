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
gsheet_link <- "https://docs.google.com/spreadsheets/d/1xgt7th62OGahzON01Oxb6phMknu6ffmkdVxUJA-9MBQ/edit?usp=sharing"
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
df_plot$value <- as.integer(df_plot$value)

# vaccine increase from previous date
vaccinations_today <- df$doses_total_NSW[df$date == date_latest]
vaccinations_yesterday <- df$doses_total_NSW[df$date == date_latest-1]

# configuration
df_vars <- colnames(df)
size_line=2
size_point=4
value_box_width=4
row_1_col = 'blue'
row_2_col = 'green'

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
              
              # Row 1 are boxes
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
                         value = df$num_new_cases[df$date == date_latest],
                         "New local cases",
                         icon = icon("male"),
                         color = row_1_col,
                         width = NULL)),
                column(width=3,
                       valueBox(
                         value = comma(sum(df$cases_20200125[which(df$date == date_latest)])),
                         "Cases since pandemic",
                         icon = icon("line-chart"),
                         color = row_1_col,
                         width = NULL)),
                column(width=3,
                       valueBox(
                         value = comma(df$doses_total_NSW[df$date == date_latest]),
                         "Total Vaccinations in NSW",
                         icon = icon("medkit"),
                         color = row_1_col,
                         width = NULL))
                ),
              
              fluidRow(

               column(width=3,
                         valueBox(
                           value = df$in_hospital[df$date == date_latest],
                           "Currently in hospital",
                           icon = icon("hospital-o"),
                           color = row_2_col,
                           width = NULL)),
               column(width=3,
                       valueBox(
                         value = df$icu[df$date == date_latest],
                         "Currently in ICU",
                         icon = icon("bed"),
                         color = row_2_col,
                         width = NULL)),
                      column(width=3,
                       # Ventilator
                       valueBox(
                         value = df$ventilator[df$date == date_latest],
                         "Currently on a ventilator",
                         icon = icon("heartbeat"),
                         color = row_2_col,
                         width = NULL)),
               column(width=3,
                      valueBox(
                        value = df$deaths[df$date == date_latest],
                        "Total deaths",
                        icon = icon("user-times"),
                        color = row_2_col,
                        width = NULL))
               
               ),
              
              fluidRow(
                column(width=3,
                           # Input: Selector for variable to plot against mpg ----
                           checkboxGroupInput(inputId = "variable",
                                       label = "Variable:", 
                                       choices = 
                                         list(
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
                                       selected = c("Num New Cases","In Hospital") , width = NULL),
                           
                           # Pass in Date objects
                           dateRangeInput("dateRange", "Date range:",
                                          start = date_latest-30,
                                          end = date_latest,
                                          min = "2021-08-01",
                                          max = date_latest,
                                          width = NULL)
                       ),
                column(width=9,
                           plotOutput(outputId = "graph_1", width = NULL, height=600)
                       ),
 
                

              ),
              fluidRow(
                column(width=12,
                       p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx"),". Made by Phillip Hungerford. For more details visit my ", a("website", href="https://philliphungerford.github.io"), ".")
                       
                       )
              )
          ),
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      #-----------------------------------------------------------------
      # TAB 3: Acknowledgements
      tabItem(tabName = "breakdown",
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("COVID-19 Tracker NSW"),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health.", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx")),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # SECTION 1 = DATE
              h1("Date"),
              # Info boxes for Overview
              # DATE
              fluidRow(
                valueBox(
                  value =  format(date_latest, "%a %b %d"),
                  "Date updated",
                  icon = icon("calendar-o"),
                  color = "blue"),
                valueBox(
                  value =  date_latest - as.Date("2021-06-25"),
                  "Days since June 25th lockdown",
                  icon = icon("calendar-o"),
                  color = "blue")
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # SECTION 2 = OVERVIEW
              h1("Overview"),
              # CASE NUMBERS (RED)
              fluidRow(                
                column(12),
                
                # Total Cases
                valueBox(
                  value = comma(sum(df$cases_20200125[which(df$date == date_latest)])),
                  "Total COVID cases since pandemic",
                  icon = icon("male"),
                  color = "red"),
                
                # Total in past 14 days
                valueBox(
                  value = comma(sum(df$num_new_cases[which(df$date >= date_earliest)])),
                  "Total cases in past 14 days",
                  icon = icon("male"),
                  color = "red")
                
                
              ),
              # DATE
              fluidRow(
                column(12),
                # TOTAL VACCINATIONS NSW
                valueBox(
                  value = comma(df$doses_total_NSW[df$date == date_latest]),
                  "Total Vaccinations in NSW",
                  icon = icon("male"),
                  color = "green"),
                
                # INCREASE IN VAC FROM PREVIOUS DAY
                valueBox(
                  value = comma(vaccinations_today - vaccinations_yesterday),
                  "Difference in vaccines from yesterday",
                  icon = icon("line-chart"),
                  color = "green"),
                
                # Tests
                valueBox(
                  value = paste0(round((((df$doses_total_NSW[df$date == date_latest])/10000000)*100),0),"%"),
                  "Percent of 10 million goal",
                  icon = icon("map"),
                  color = "green")
              ),
              
              # DEATHS & HOSPITALISED
              fluidRow(
                column(12),
                # TOTAL DEATHS
                valueBox(
                  value = df$deaths[df$date == date_latest],
                  "Total deaths",
                  icon = icon("user-times"),
                  color = "orange"),
                
                # Tests
                valueBox(
                  value = comma(df$total_tests[df$date == date_latest]),
                  "Total tests",
                  icon = icon("thermometer"),
                  color = "orange")
              ),
              
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # SECTION 3 = VACCINATIONS
              h1("Vaccinations"),
              # CUMULATIVE DOSES
              fluidRow(
                column(12),
                ## DOSES
                # Tests
                valueBox(
                  value = comma(df$doses_total_24hr[df$date == date_latest]),
                  "Total Doses administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                
                ## DOSES
                valueBox(
                  value = comma(df$doses_total_nswHealth_cum[df$date == date_latest]),
                  "Total Doses administered from NSW Health",
                  icon = icon("medkit"),
                  color = "green"),
                
                valueBox(
                  value = comma(df$doses_total_gp_cum[df$date == date_latest]),
                  "Total Doses administered from GP network",
                  icon = icon("medkit"),
                  color = "green"),
                valueBox(
                  value = comma(df$doses_total_NSW[df$date == date_latest]),
                  "Total Doses administered in NSW",
                  icon = icon("medkit"),
                  color = "green")
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # SECTION 3 = VACCINATIONS
              h1("Vaccinations by dose"),
              p("These are vaccinations administered by NSW Health"),
              fluidRow(
                valueBox(
                  value =  format(date_latest_doses, "%a %b %d"),
                  "Date updated",
                  icon = icon("calendar-o"),
                  color = "green"),
                
                column(12),
                ## DOSES
                # Tests
                valueBox(
                  value = comma(df$doses_1st_24hr[df$date == date_latest_doses]),
                  "First Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                # Tests
                valueBox(
                  value = comma(df$doses_2nd_24hr[df$date == date_latest_doses]),
                  "Second Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                
              ),
              # CUMULATIVE DOSES
              fluidRow(
                column(12),
                
                ## DOSES
                
                # Tests
                valueBox(
                  value = comma(df$doses_1st_cum[df$date == date_latest_doses]),
                  "First Dose administered",
                  icon = icon("medkit"),
                  color = "green"),
                
                # Tests
                valueBox(
                  value = comma(df$doses_2nd_cum[df$date == date_latest_doses]),
                  "Second Dose administered",
                  icon = icon("medkit"),
                  color = "green"),
                
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("Past 24hrs from 8pm last night:"),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # 2 rows of 3 
              fluidRow(
                column(12),
                # New Cases
                valueBox(
                  value = df$num_new_cases[df$date == date_latest],
                  "New Cases",
                  icon = icon("male"),
                  color = "red"),
                
                # # Infectious
                # valueBox(
                #   value = df$infectious_24hrs[df$date == date_latest],
                #   "Infectious in the community",
                #   icon = icon("map"),
                #   color = "red")
                
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              fluidRow(
                column(12),
                # In hospital
                valueBox(
                  value = df$in_hospital[df$date == date_latest],
                  "Currently in hospital",
                  icon = icon("hospital-o"),
                  color = "orange"),
                
                # ICU
                valueBox(
                  value = df$icu[df$date == date_latest],
                  "Currently in ICU",
                  icon = icon("bed"),
                  color = "orange"),
                
                # Ventilator
                valueBox(
                  value = df$ventilator[df$date == date_latest],
                  "Currently on a ventilator",
                  icon = icon("heartbeat"),
                  color = "orange")
              ),
              
              
              br(),
              br(),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is based on daily 11am updates and are sourced from", a("NSW Health", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx"),". Made by Phillip Hungerford. For more details visit my ", a("website", href="https://philliphungerford.github.io"), ".")
      ),
      
      #-----------------------------------------------------------------
      # TAB Last: Information
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
      scale_color_brewer(palette = "Set1") +
      theme_light() +
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