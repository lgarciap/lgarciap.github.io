#Loading packages

library(shiny)
library(ggplot2)

#Loading data
rendimiento<-read.csv("Notas - 100-rendimiento_1.csv",sep = ";")
actFeatures<-read.csv("ponderacion.csv",sep = ";")

#Preparing data
actFeatures$Date<- as.Date(actFeatures$Date, "%d/%m/%Y")

#Getting means of activities
activitiesMean<-data.frame(colnames(rendimiento[3:13]), colMeans(rendimiento[3:13]))
names(activitiesMean)<-c("Activity", "Mean")
activitiesMean<-merge(activitiesMean,actFeatures)
activitiesMean<-activitiesMean[complete.cases(activitiesMean),]

#Getting students performance by career
perfbyCar<-aggregate(rendimiento$Rendimiento~rendimiento$CARRERA,FUN = mean)
names(perfbyCar)<-c("career","performance")
perfbyCar$performance<-perfbyCar$performance*100
x<-perfbyCar$performance
names(x)<-perfbyCar$career

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #outputs
    #Texts outputs
    output$estCount<-renderText({nrow(rendimiento)})
    output$estPerfMean<-renderText({mean(rendimiento$Rendimiento)*100})
    output$escEval<-renderText({sum(actFeatures[c(1:9,11,14),]$Value)})
    #Table output
    output$stByCareer<-renderTable({
            table<-as.data.frame(table(rendimiento$CARRERA))
            names(table)<-c("Career","Students_Amount")
            table
        })
    #Text ouputs
    output$actMean<-renderText({mean(rendimiento[[input$ActivitySel]])})
    output$meanCareer<-renderText({mean(rendimiento[rendimiento$CARRERA==input$CareerSel,][[input$ActivitySel]])})
    #Plot
    output$first_plot<-renderPlot({
        #If Average scores for the activities by date radiobutton is selected 
        if (input$graphic_type == "Average scores for the activities by date"){
            g <- ggplot(activitiesMean,aes(Date,Mean))+ geom_line( colour="#000099") + scale_x_date()
            g <- g + xlab("Activities Date")
            g <- g + ylab("Activities Mean")
            g <- g + ggtitle("Average scores for the activities by date")
           
            #If checkbox is selected
             if (input$trendChkBox){
                 model<-lm(Mean~Date,activitiesMean)
                 g <- g + geom_abline(slope = model$coefficients[2], intercept = model$coefficients[1], colour="red")
             }
            print(g) #prints line plot
        }
        #If histogram of students performance radiobutton is selected 
        if (input$graphic_type == "histogram of students performance"){
            g <- ggplot(data=rendimiento, aes(rendimiento$Rendimiento)) 
            g <- g + geom_histogram(breaks=seq(0, 1, by =.2), col="red",aes(fill=..count..))
            g <- g + scale_fill_gradient("Count", low = "yellow", high = "orange")
            g <- g + ggtitle("Histogram of students performance") 
            g <- g + xlab("Performance")
            g <- g + ylab("Frequecy")
            print(g) #prints histogram plot

        }
        #If Scores by career radiobutton is selected 
        if (input$graphic_type == "Scores by career"){
            g <- ggplot(perfbyCar, aes(x=as.factor(career),y=performance, fill=career))
            g <- g + geom_bar(stat = "identity")
            g <- g + theme(axis.text.x=element_text(angle=25, hjust = 1))
            g <- g + ggtitle("Students performance by Career") 
            g <- g + xlab("Career")
            g <- g + ylab("Performance")
            print(g) #prints barplot
        }
     })
     
})
