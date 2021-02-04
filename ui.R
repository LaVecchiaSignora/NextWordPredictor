
suppressWarnings(library(shiny))
shinyUI(navbarPage("Coursera Data Science Capstone: Course Project",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Rasul Balayev</strong>"),
                            br(),
                            HTML("<strong>Date:04.02.2021</strong>"),
                            br(),
                           # Sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Enter a partially complete sentence to begin the next word prediction"),
                                    textInput("inputString", "Enter a partial sentence here",value = ""),
                                    hr(),
                                    hr(),
                                    hr(),
                                    hr()
                                ),
                                mainPanel(
                                    h2("Predicted Next Word"),
                                    verbatimTextOutput("prediction"),
                                    strong("Sentence Input:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                    textOutput('text1'),
                                    br(),
                                    strong("Note:"),
                                    tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                    textOutput('text2')
                                )
                            )
                            
                   )
)
)
