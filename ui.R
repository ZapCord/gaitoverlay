## Tony Johnson
## Last Modified: 6/9/2021
## UCHealth Motion Capture Lab

library(shiny)
library(shinyjs)
jsCode <- "shinyjs.refresh = function() { location.reload(); }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions=c("refresh")),
  titlePanel("Gait Video Overlay"),
  fluidRow(
    column(2,id="fileinputpanel",
           fileInput("existing_video_input", 
                      label=h5("Step 1: Patient video")),
           helpText("Choose video file from the
                    to analyze (max size 3000 MB)"),
           fileInput("existing_data_input", 
                     label=h5("Step 2: Patient Kinematics/Kinetics")),
           helpText("Choose the kinematics or kinetics .csv file from the
                    that corresponds with the input video"),
           selectInput("graph_name", "Choose a graph",list('placeholder'= "plac")),
           numericInput("ratio",label = h5("Model/Camera Ratio"), value = 2)
           
    ),
    column(4, h3("Video"),
           imageOutput("image")
    ),
    column(4, h3("Visualization"),
           tabsetPanel(
             tabPanel("Plot",plotOutput("plot")),
             tabPanel("Long Format CSV",dataTableOutput("kinematics_csv"))
           )
    )
    
  ),
  fluidRow(
    column(2,helpText("File Upload Status:"),
           verbatimTextOutput("fileinputpanel"),
           hr(),
           actionButton("refresh",label = "Refresh Page"),
    ),
    hr(),
    column(8,
           sliderInput("slider_time", label = h3("Time Index"), min = 1, 
                       max = 100, value = 1, width='100%',
                       animate = animationOptions(interval=16.67))
           
    )
  )
  
)