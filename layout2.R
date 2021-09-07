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
      menuItem("Overview", tabName = "overview", icon = icon("columns")),#
      menuItem("Vaccinations", tabName = "vaccinations", icon = icon("medkit")),#
      menuItem("Information", tabName = "information", icon = icon("info"))#
    )
  ),
  #=========================================================================
  ## Body content
  dashboardBody(
    
    tabItems(
      #-----------------------------------------------------------------
      # TAB 1: Overview
      tabItem(tabName = "overview",
              
              fluidRow(
                column(width=3,
                       valueBox(
                         value =  format(date_latest, "%a %b %d"),
                         "Date updated",
                         icon = icon("calendar-o"),
                         color = "blue",
                         width = NULL),
                       
                       valueBox(
                         value =  date_latest - as.Date("2021-06-25"),
                         "Days since June 25th lockdown",
                         icon = icon("calendar-o"),
                         color = "blue",
                         width = NULL)),
                
                column(width=3, 
                       # INCREASE IN VAC FROM PREVIOUS DAY
                       valueBox(
                         value = comma(vaccinations_today - vaccinations_yesterday),
                         "Difference in vaccines from yesterday",
                         icon = icon("line-chart"),
                         color = "blue",
                         width = NULL),
                       
                       # Total Cases
                       valueBox(
                         value = comma(sum(df$cases_20200125[which(df$date == date_latest)])),
                         "Total COVID cases since pandemic",
                         icon = icon("male"),
                         color = "red",
                         width = NULL)),
                
                column(width=3,
                       # Total in past 14 days
                       valueBox(
                         value = comma(sum(df$num_new_cases[which(df$date >= date_earliest)])),
                         "Total cases in past 14 days",
                         icon = icon("male"),
                         color = "red",
                         width = NULL),
                       
                       # TOTAL VACCINATIONS NSW
                       valueBox(
                         value = comma(df$doses_total_NSW[df$date == date_latest]),
                         "Total Vaccinations in NSW",
                         icon = icon("male"),
                         color = "green",
                         width = NULL)),
                
                column(width=3,
                       # TOTAL DEATHS
                       valueBox(
                         value = df$deaths[df$date == date_latest],
                         "Total deaths",
                         icon = icon("user-times"),
                         color = "orange",
                         width = NULL),
                       
                       # Tests
                       valueBox(
                         value = comma(df$total_tests[df$date == date_latest]),
                         "Total tests",
                         icon = icon("thermometer"),
                         color = "orange",
                         width = NULL))
              ),
              
              fluidRow(
                
                # column 2
                column(width = 12,
                       box(
                         title = "", width = NULL, status = 'primary',
                         plotOutput(outputId = "graph_1", width = NULL),
                         # Input: Selector for variable to plot against mpg ----
                         selectInput(inputId = "variable",
                                     label = "Variable:", 
                                     choices = 
                                       list(
                                         "New local cases " = "Num New Cases",
                                         "Overseas Acquired" = "Overseas Acquired",
                                         "Total New Cases" = "Total New Cases",
                                         "Total Cases since Jan 25 2020" = "Cases 20200125",
                                         "Total cases since recent outbreak ( Jun 16 2021)" = "Cases 20210616 Outbreak",
                                         "Total Tests" = "Tests 24hrs",
                                         "Total Tests in last 24 hours" = "Total Tests",
                                         "In hospital" = "Hospitalised",
                                         "In ICU" = "Icu",                                     
                                         "Total deaths" = "Deaths",
                                         "On ventilator" = 'Ventilator',
                                         
                                         # vaccinations
                                         "Doses: in past 24hr by NSW Health" = "Doses Total 24hr",
                                         "Doses: Total for NSW" = "Doses Total NSW",
                                         "Doses: Total from NSW Health" = "Doses Total nswHealth Cum",
                                         "Doses: Total from GP Network" = "Doses Total Gp Cum",
                                         "Doses: First cumulative" = "Doses 1st Cum",
                                         "Doses: Second cumulative" = "Doses 2nd Cum", 
                                         "First dose (NSW Health)" = "Doses 1st 24hr",
                                         "Second dose (NSW Health)" = "Doses 2nd 24hr"
                                       ),
                                     selected = "Num New Cases", width = NULL),
                         selectInput(inputId = "variable2",
                                     label = "Variable2:", 
                                     choices = 
                                       list(
                                         "New local cases " = "Num New Cases",
                                         "Overseas Acquired" = "Overseas Acquired",
                                         "Total New Cases" = "Total New Cases",
                                         "Total Cases since Jan 25 2020" = "Cases 20200125",
                                         "Total cases since recent outbreak ( Jun 16 2021)" = "Cases 20210616 Outbreak",
                                         "Total Tests" = "Tests 24hrs",
                                         "Total Tests in last 24 hours" = "Total Tests",
                                         "In hospital" = "Hospitalised",
                                         "In ICU" = "Icu",                                     
                                         "Total deaths" = "Deaths",
                                         "On ventilator" = 'Ventilator',
                                         
                                         # vaccinations
                                         "Doses: in past 24hr by NSW Health" = "Doses Total 24hr",
                                         "Doses: Total for NSW" = "Doses Total NSW",
                                         "Doses: Total from NSW Health" = "Doses Total nswHealth Cum",
                                         "Doses: Total from GP Network" = "Doses Total Gp Cum",
                                         "Doses: First cumulative" = "Doses 1st Cum",
                                         "Doses: Second cumulative" = "Doses 2nd Cum", 
                                         "First dose (NSW Health)" = "Doses 1st 24hr",
                                         "Second dose (NSW Health)" = "Doses 2nd 24hr"
                                       ),
                                     selected = "Num New Cases", width = NULL),
                         
                         # Pass in Date objects
                         dateRangeInput("dateRange", "Date range:",
                                        start = "2021-08-01",
                                        end = date_latest,
                                        min = "2021-08-01",
                                        max = date_latest,
                                        width = NULL),
                         
                         
                         
                       ))
              ),
              
              br(),
              p("Made by Phillip Hungerford"),
              p("For more details visit my:", a("website", href="https://philliphungerford.github.io")),
              p("If you would like to make a request, email me on phillip.hungerford@gmail.com")
      ),
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      #-----------------------------------------------------------------
      # TAB 3: Acknowledgements
      tabItem(tabName = "vaccinations",
              h1("Vaccinations by dose"),
              p("These are vaccinations administered by NSW Health"),
              fluidRow(
                valueBox(
                  value =  format(date_latest_doses, "%a %b %d"),
                  "Date updated",
                  icon = icon("calendar-o"),
                  color = "green")),
              
              ## DOSES
              fluidRow(
                valueBox(
                  value = comma(df$doses_1st_24hr[df$date == date_latest_doses]),
                  "First Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                
                valueBox(
                  value = comma(df$doses_2nd_24hr[df$date == date_latest_doses]),
                  "Second Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green")),
              fluidRow(
                valueBox(
                  value = comma(df$doses_1st_cum[df$date == date_latest_doses]),
                  "First Dose administered",
                  icon = icon("medkit"),
                  color = "green"),
                
                valueBox(
                  value = comma(df$doses_2nd_cum[df$date == date_latest_doses]),
                  "Second Dose administered",
                  icon = icon("medkit"),
                  color = "green"))
      ),
      
      #-----------------------------------------------------------------
      # TAB Last: Information
      tabItem(tabName = "information",
              h2("Information"),
              p("Made by Phillip Hungerford"),
              br(),
              p("For more details visit my:", a("website", href="https://philliphungerford.github.io")),
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
           title=" Trends Over Time") + 
      scale_color_brewer(palette = "Set1") +
      theme_light() +
      theme(legend.position = "bottom",
            legend.title=element_blank())
    
    
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