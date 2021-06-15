## Tony Johnson
## Last Modified: 6/11/2021
## UCHealth Motion Capture Lab

library(shiny)
library(shinyjs)
jsCode <- "shinyjs.refresh = function() { location.reload(); }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions=c("refresh")),
  # Finding the window size, code from 
  # https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  titlePanel("Gait Video Overlay"),
  fluidRow(
    column(1,id="fileinputpanel",
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
    column(6, h3("Video"),
           imageOutput("image")
           
    ),
    column(5, h3("Visualization"),
           tabsetPanel(
             tabPanel("Plot",plotOutput("plot",
                                        click = "plot_click",
                                        hover = "plot_hover",
                                        brush = brushOpts("plot_brush",
                                                          fill="#ccc")
                                                          
                                        ),
                      hr(),
                      helpText("Plot Information"),
                      verbatimTextOutput("info",placeholder = TRUE)
                      ),
             tabPanel("Long Format CSV",dataTableOutput("kinematics_csv"))
           )
    )

  ),
  fluidRow(
    column(1,helpText("File Upload Status:"),
           verbatimTextOutput("fileinputpanel"),
           hr(),
           actionButton("refresh",label = "Refresh Page"),
    ),
    hr(),
    column(11,
           sliderInput("slider_time", label = h3("Time Index"), min = 1,
                       max = 100, value = 1, width='100%',
                       animate = animationOptions(interval=16.67))

    )
  )

)
