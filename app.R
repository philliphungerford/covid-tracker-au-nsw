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
library(dplyr)
library(haven)
library(shiny)
library(shinydashboard) # for tabs
library(DT) # for displaying tables
library(ggplot2)
library(data.table)
library(reshape2)
library(tools)
library(tidyverse)
library(scales) # add comma to output
##############################################################################
# load data
df <- read.csv("data/covid_cases_nsw - Sheet1.csv")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
date_latest <- max(df$date)
date_earliest <- max(df$date)-13

# rename variables for nicer look
df_plot <- data.frame(df)
new <- names(df_plot)
new <- toTitleCase(str_replace_all(new, "_", " "))
names(df_plot) <- new

## TIDY DATA
# date, variable, value
df_plot <- reshape2::melt(df_plot, id.var = "Date")

df_plot$Date <- as.Date(df_plot$Date, format = "%Y-%m-%d")
df_plot$variable <- as.factor(df_plot$variable)
df_plot$value <- as.integer(df_plot$value)

# configuration
df_vars <- colnames(df)
size_line=1
size_point=2
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
  dashboardHeader(title = "COVID19 Tracker NSW"),
  #=========================================================================
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # icons from (https://fontawesome.com/v4.7.0/icons/)
      menuItem("Overview", tabName = "overview", icon = icon("columns")),#
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

              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("COVID-19 Tracker NSW"),
              p("This dashboard shows current statistics for COVID-19 cases in NSW. Data is sourced from", a("NSW Health.", href="https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-nsw.aspx")),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
              h1("Overview"),
              # CASE NUMBERS (RED)
              fluidRow(                
                column(12),
                
                # Total Cases
                valueBox(
                  value = comma(sum(df$total_cases[which(df$date == date_latest)])),
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
              
              # VACCINATIONS
              fluidRow(
                column(12),
                # TOTAL DEATHS
                valueBox(
                  value = comma(df$doses_total_NSW[df$date == date_latest]),
                  "Total Vaccinations in NSW",
                  icon = icon("male"),
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
                  icon = icon("bed"),
                  color = "orange"),
                
                # Tests
                valueBox(
                  value = comma(df$total_tests[df$date == date_latest]),
                  "Total tests",
                  icon = icon("thermometer"),
                  color = "orange")
              ),

              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              
              h1("Vaccinations"),
              p("These are vaccinations administered by NSW Health"),
              fluidRow(
                column(12),
                ## DOSES
                # Tests
                valueBox(
                  value = comma(df$doses_1st_24hr[df$date == date_latest]),
                  "First Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                # Tests
                valueBox(
                  value = comma(df$doses_2nd_24hr[df$date == date_latest]),
                  "Second Dose administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green"),
                
                valueBox(
                  value = comma(df$doses_total_24hr[df$date == date_latest]),
                  "Total Doses administered in 24hrs",
                  icon = icon("medkit"),
                  color = "green")
              ),
              # CUMULATIVE DOSES
              fluidRow(
                column(12),
                ## DOSES
                # Tests
                valueBox(
                  value = comma(df$doses_1st_cum[df$date == date_latest]),
                  "First Dose administered",
                  icon = icon("medkit"),
                  color = "green"),
                # Tests
                valueBox(
                  value = comma(df$doses_2nd_cum[df$date == date_latest]),
                  "Second Dose administered",
                  icon = icon("medkit"),
                  color = "green"),
                
                valueBox(
                  value = comma(df$doses_total_nswHealth_cum[df$date == date_latest]),
                  "Total Doses administered",
                  icon = icon("medkit"),
                  color = "green")
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
                
                # Infectious
                valueBox(
                  value = df$infectious_24hrs[df$date == date_latest],
                  "Infectious in the community",
                  icon = icon("map"),
                  color = "red")
              
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              fluidRow(
                column(12),
                # In hospital
                valueBox(
                  value = df$hospitalised[df$date == date_latest],
                  "Currently in hospital",
                  icon = icon("hospital-o"),
                  color = "orange"),
                
                # ICU
                valueBox(
                  value = df$icu[df$date == date_latest],
                  "Currently in ICU",
                  icon = icon("heartbeat"),
                  color = "orange")
              ),
            
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h2("Trends"),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # Input: Selector for variable to plot against mpg ----
              checkboxGroupInput(inputId = "variable",
                                 label = "Variable:", 
                                 choices = 
                                   list("New Cases" = "Num New Cases",
                                        "Overseas Acquired" = "Overseas Acquired",
                                        "Tests" = "Tests 24hrs",
                                        "Hospitalised" = "Hospitalised",
                                        "In ICU" = "Icu",
                                        "Deaths" = "Deaths",
                                        "Infectious in the community" = "Infectious 24hrs",
                                        "Linked to known cluster" = "Linked Cluster",
                                        "Household contacts" = "Contact Household",
                                        "Close contacts" = "Contact Close"),
                                 selected = "Num New Cases"),
              # Pass in Date objects
              dateRangeInput("dateRange", "Date range:",
                             start = date_latest-6,
                             end = date_latest,
                             min = "2021-08-01",
                             max = date_latest),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              # Show plot 
              fluidRow(
                column(12),
                plotOutput("graph_1")
              ),
              br(),
              p("Made by Phillip Hungerford"),
              p("For more details visit my:", a("website", href="https://philliphungerford.github.io")),
              p("If you would like to make a request, email me on phillip.hungerford@gmail.com")
              ),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          
      #-----------------------------------------------------------------
      # TAB 13: Acknowledgements
      tabItem(tabName = "information",
              h2("Information"),
              p("Made by Phillip Hungerford"),
              br(),
              p("For more details visit my:", a("website", href="https://philliphungerford.github.io")),
              p("If you would like to make a request, email me on phillip.hungerford@gmail.com")
      )
      #-----------------------------------------------------------------
    ) # tabItems
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
    
    tmp_vars <- input$variable
    
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
           title="") + 
      scale_color_brewer(palette = "BuPu") +
      theme_dark() +
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