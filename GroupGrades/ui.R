#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#Create data

#Loading data
rendimiento<-read.csv("Notas - 100-rendimiento_1.csv",sep = ";")
actFeatures<-read.csv("ponderacion.csv",sep = ";")


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "style.css",
                  
  # Application title                       
  titlePanel("Programming group. Faculty of Engineering"),
  mainPanel(width=100,
      fluidRow(
        #Gruop Features
        titlePanel("Group Features"),
        column(5,
               class = "panels",
               span(
                   class="subtitle",
                   "Students Amount:"
               ),
               span(
                   textOutput("estCount")
               ),
               span(
                   class="subtitle",
                   "Score evaluated so far (points):"
               ),
               span(
                   textOutput("escEval")
               ),
               span(
                   class="subtitle",
                   "Students Average Performance (%):"
               ),
               span(
                   textOutput("estPerfMean")
               )
             
        ),
        column(5,
               class = "panels",
               span(
                   class="subtitle",
                   "Students Amount by Career:"
               ),
               tableOutput("stByCareer")
        )
      ),
      fluidRow(
    
          column(5,
                 class="panels",
                 selectInput('ActivitySel', 'Activity:', as.character(actFeatures$Activity)),
                 selectInput('CareerSel', 'Career:', as.character(unique(rendimiento$CARRERA)))
            ),
          column(5,
                 class="panels",
                 
                 span(class="subtitle",
                      "Selected Activity Grade Mean: "
                 ),
                 span(
                     textOutput("actMean")
                 ),
                 span(class="subtitle",
                      "Selected Career Grade Mean in selected Activity: "
                 ),
                 span(
                     textOutput("meanCareer")
                 )
              
          )
      ),
      fluidRow(
          titlePanel("Plots"),
          #plots Area
          column(4,
                 class = "panels",
                 radioButtons(
                     inputId="graphic_type",
                     label="Choose Graphic",
                     choices = c("Average scores for the activities by date",
                                 "histogram of students performance",
                                 "Scores by career")
                    )
          ),
          column(6,
            class = "panels",
            #Plot options  
            plotOutput(outputId = "first_plot",width = 500, height = 300),
            conditionalPanel(
                condition = "input.graphic_type == 'Average scores for the activities by date'",
                checkboxInput(
                              inputId = "trendChkBox",
                              label = strong("Show trend"),
                              value = FALSE
                              )
            )
      
      )
     
    
    )
)
)
)
