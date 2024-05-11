#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

require(shiny)
require(shinythemes)
require(tidyverse)
require(DT)
require(shinyalert)
require(shinyjs)

# Define UI for application that draws a histogram
fluidPage(
  theme = shinytheme("paper"),
  useShinyjs(), 
  navbarPage(
    "Rating fruits and their spread of quality",
    tabPanel("Input data",
             sidebarLayout(
               sidebarPanel(width = 5,
                            h4("Your fruit rating"), # Section heading
                            textInput("fruit", "Fruit to be judged", ""),
                            sliderInput("rating", "Rating out of 10", min = 1, max = 10, value = 5),
                            actionButton("submit", "Input results")
               ),
               mainPanel(width = 5, 
                         h4("Data inputs"), 
                         DT::dataTableOutput("datatable") # Outputs current table
               )
             ) 
    ), # finish first tab page
    tabPanel("Fruit Rating Overview", 
             mainPanel(width = 10, 
                       plotOutput(outputId = "overview_rating"), # Plots overview boxplot
                       DT::dataTableOutput("plotdata") # Outputs current table without date
             )
    ), # finish second tab page
    tabPanel("Fruit Rating Filtered", 
             mainPanel(width = 10, 
                       DT::dataTableOutput(outputId = "datatable_filt"), # Plots filtered dataset
                       plotOutput("filtered_rating") # Outputs filtered table
             ) 
    ) # finish third tab page
  )
) # finish UI
