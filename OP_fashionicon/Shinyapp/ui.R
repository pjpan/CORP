library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("网红和用户之间的匹配关系"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,
             selectInput("uid",
                         "用户的UID:",
                         c(uidslist))
      )),
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("uideid")
    )
  )
)


# # Define UI
# shinyUI(navbarPage("Facebook V: Predicting Check Ins",
#                    theme = shinytheme("cerulean"), #united
#                    tabPanel("Visual inspection",
#                             # Visual inspection sidebar panel
#                             sidebarPanel(
#                               selectInput("placeIdSel", "Place id selection",
#                                           selected = placeIds[1], multiple = FALSE,
#                                           choices = c(placeIds)),
#                               fluidRow(column(3,actionButton("prevPlace", "Previous",
#                                                              icon = icon("arrow-left"))),
#                                        column(3,actionButton("nextPlace", "Next",
#                                                              icon = icon("arrow-right")),
#                                               offset=1)
#                               ),
#                               br(),
#                               selectInput("analysisVar", "Analysis variable",
#                                           choices = c(analysisVarsComp, "accuracyGroup"),
#                                           selected = "x"
#                               ),
#                             # Visual inspection main panel
#                             mainPanel(
#                               tabsetPanel(
#                                 tabPanel("X-Y analysis",
#                                          br(),
#                                          radioButtons("xyType", "Plot type",
#                                                       choices = c("Density", "Observations"),
#                                                       selected = "Observations",
#                                                       inline = TRUE),
#                                          plotlyOutput("xyPlotly"),
#                                          br(),
#                                          fluidRow(column(6,
#                                                          sliderInput("nbXYBins", "Number of x and y density bins",
#                                                                      min = 5, max = 100, value = 50)
#                                                          , offset=3
#                                          ))
#                                 )
#                               )
#                             )
#                    )
# ))
