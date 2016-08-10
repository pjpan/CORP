library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("More Widgets"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("rock", "pressure", "cars"))
        
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Observations"),
      tableOutput("view")
    )
  )
))



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
