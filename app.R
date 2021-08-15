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
size_line=2
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
              h1("Today"),
              fluidRow(
              
                valueBox(
                value = date(),
                "Current",
                icon = icon("calendar-o"),
                color = "blue"),
              
                ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h1("Overview"),
              # Info boxes for Overview
              fluidRow(                
                
                valueBox(
                  value =  format(date_latest, "%a %b %d"),
                  "Date updated",
                  icon = icon("calendar-o"),
                  color = "blue"),
                
                # Participants = 1514
                valueBox(
                  value = sum(df$num_new_cases[which(df$date >= date_earliest)]),
                  "Total cases in past 14 days",
                  icon = icon("male"),
                  color = "red"),
                
                valueBox(
                  value = df$doses_cum[df$date == date_latest],
                  "Cumulative Doses",
                  icon = icon("male"),
                  color = "green"),
              
              # Tests
              valueBox(
                value = df$total_tests[df$date == date_latest],
                "Total tests",
                icon = icon("male"),
                color = "green")),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              
                h1("Past 24hrs from 8pm last night:"),
              fluidRow(
                
                # New Cases
                valueBox(
                  value = df$num_new_cases[df$date == date_latest],
                  "New Cases",
                  icon = icon("line-chart"),
                  color = "orange"),
                
                # Infectious
                valueBox(
                  value = df$infectious_24hrs[df$date == date_latest],
                  "Infectious in the community",
                  icon = icon("line-chart"),
                  color = "red"),
                
                # In hospital
                valueBox(
                  value = df$hospitalised[df$date == date_latest],
                  "Currently in hospital",
                  icon = icon("line-chart"),
                  color = "orange"),
                
                # ICU
                valueBox(
                  value = df$deaths[df$date == date_latest],
                  "Total deaths",
                  icon = icon("line-chart"),
                  color = "red"),
                
                # ICU
                valueBox(
                  value = df$icu[df$date == date_latest],
                  "Currently in ICU",
                  icon = icon("line-chart"),
                  color = "orange"),

                
                # Tests
                valueBox(
                  value = df$doses_24hr[df$date == date_latest],
                  "Doses administered",
                  icon = icon("male"),
                  color = "green")
              ),
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              h2("Graphs"),
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              
              fluidRow(
              column(12),
                plotOutput("graph_1")
              ),
              
              # Input: Selector for variable to plot against mpg ----
              selectInput(inputId = "variable",
                          label = "Variable:", 
                          choices = 
                            c("New Cases" = "Num New Cases",
                              "Overseas Acquired" = "Overseas Acquired",
                              "Tests" = "Tests 24hrs",
                              "Hospitalised" = "Hospitalised",
                              "ICU" = "Icu",
                              "Deaths" = "Deaths",
                              "Infectious" = "Infectious 24hrs",
                              "Linked to clust" = "Linked Cluster",
                              "Household contact" = "Contact Household",
                              "Close contact" = "Contact Close")),
              
              
              fluidRow(
                column(12),
                  plotOutput("graph_2"))
              
      ),
          
      #-----------------------------------------------------------------
      # TAB 13: Acknowledgements
      tabItem(tabName = "information",
              h2("Information"),
              p("Made by Phillip Hungerford"),
              br(),
              p("For more details visit my: ", a("github", href="https://github.com/philliphungerford/covid-tracker-au-nsw")),
              p("Checkout my ", a("wesbite", href="https://philliphungerford.github.io"))
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
    
    tmp_vars <- as.character(unique(df_plot$variable)[c(1,2,7,8,9,10)])
    tmp <- df_plot[df_plot$variable %in% tmp_vars,]
      ggplot(data = tmp, aes(x = Date, y = value, color = variable)) + 
      geom_line(size=size_line) +
      geom_point(size=size_point) +
      scale_x_date(date_labels="%d %b",date_breaks  ="1 day") + 
      labs(x = "Date",
           y = "Number of Cases") + 
      theme(legend.position = "right") +
    theme_bw() 
    
  })
  
  output$graph_2 <- renderPlot({
    tmp <- df_plot[which(df_plot$variable == input$variable), ]
    
    ggplot(data = tmp, aes(x = Date, y = value, color=variable)) + 
      geom_line(size=size_line) + 
      geom_point(size=size_point) +
      scale_x_date(date_labels="%d %b",date_breaks  ="1 day") + 
      labs(x = "Date",
           y = input$variable) +
      theme(legend.position = "right") +
      theme_bw()
    
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