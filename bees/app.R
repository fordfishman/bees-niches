#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bees"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Fig. 1: Niche Complementarity"),
            sliderInput("beeNum1",
                        "Number of bee species:",
                        min = 1,
                        max = 5,
                        value = 3),
            
            sliderInput("plantSpBee",
                        "Percentage of plant species used by each bee species:",
                        min = 1,
                        max = 100,
                        value = 3),
            
            h4("Fig. 2: Sampling Effect"),
            
            sliderInput("beeNum2",
                        "Number of bee species:",
                        min = 1,
                        max = 5,
                        value = 3),
            checkboxInput("superSpecies",
                          "Include superspecies",
                          value = FALSE)
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("fig1"),
           plotOutput("fig2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$fig1 <- renderPlot({
        
        beeNum <- input$beeNum1 # number of bee species
        perSpeciesBee <- 100/beeNum # number of bees per bee species
        beeCom <- rep(perSpeciesBee,beeNum) # vector of bee com. structure
       
        
        numPlants <- 100 # number of plant species
        plantSpBee <- input$plantSpBee # number of plant species each bee species uses
        beePerPlant <- perSpeciesBee/plantSpBee # number of bees going to each plant
        
        overlapAll <- rep(0,100)
        reps <- rep(0,100)
        for(ind in 1:100){
            plantVisits <- rep(0, numPlants)
            overlap <- c()
            for (i in 1:beeNum){
                a <- sample(1:numPlants, size = plantSpBee)
                overlap <- union(overlap, a)
                for (j in a){
                    plantVisits[j] <-  plantVisits[j] + beePerPlant
                }
                
            }
            
            
            
            y = 1*(plantVisits)/(0.1+(plantVisits))
            a = sum(y)
            reps[ind] <- a
            overlap.perc <- length(overlap)/100
            overlapAll[ind] <- overlap.perc
        }
        # ggplot(NULL, aes(x=overlapAll,y=reps))+
        #     geom_point(size = 2) +
        #     scale_y_continuous("Seed Production", limits = c(0,100)) +
        #     scale_x_continuous("Proportion of Plants Visited",limits = c(0,1))+
        #     ggtitle("Fig. 1") +
        #     theme(panel.grid.major = element_blank(), 
        #           panel.grid.minor = element_blank(),
        #           panel.background = element_blank(), 
        #           axis.line = element_line(colour = "black"),
        #           axis.text = element_text(size = 16),
        #           axis.title = element_text(size=16),
        #           axis.ticks = element_line(size = 1),
        #           axis.ticks.length = unit(5,"pt"),
        #           plot.title = element_text(size=18, face = "bold"))
        ov <- round(overlapAll,1)
        ov <- ifelse(ov==0,0.1,ov)
        ov <- as.factor(ov)
        
        ggplot(NULL, aes(x=reps,fill=ov))+
            geom_histogram(binwidth=1) +
            scale_y_continuous("Frequency", limits = c(0,100)) +
            scale_x_continuous("Seed Production",limits = c(0,100))+
            ggtitle("Fig. 1") +
            scale_fill_grey("Proportion of\nPlants Reached",
                            limits=as.character((1:10)/10),
                            end=0,start=0.8) +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text = element_text(size = 16),
                  axis.title = element_text(size=16),
                  axis.ticks = element_line(size = 1),
                  axis.ticks.length = unit(5,"pt"),
                  plot.title = element_text(size=18, face = "bold")) 
            
        # x <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = beeNum + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$fig2 <- renderPlot({
        
        beeNum <- input$beeNum2 # number of bee species
        supSpPresent <- input$superSpecies
        
        perSpeciesBee <- 100/beeNum # number of bees per bee species
        beeCom <- rep(perSpeciesBee,beeNum) # vector of bee com. structure
        
        
        numPlants <- 100 # number of plant species
        plantSpBee <- 10 # number of plant species each bee species uses
        beePerPlant <- perSpeciesBee/plantSpBee # number of bees going to each plant
        
        reps <- rep(0,100)
        b <- rep(0,100)
        if (supSpPresent){
            
            b <- sample(1:5, size =100, replace = TRUE) # the superspecies
            
        } 
        supSp <- c()
        for(ind in 1:100){
            plantVisits <- rep(0, numPlants)
            supSpecies <- "Absent"
            for (i in 1:beeNum){
                mult <- 1.0
                
                if (i==b[ind]){
                    mult <- 8
                    supSpecies <- "Present"
                    
                }
                a <- sample(1:numPlants, size = plantSpBee*mult)
                
                for (j in a){
                    plantVisits[j] <-  plantVisits[j] + beePerPlant*mult
                }
                
            }
            supSp <- c(supSp, supSpecies)
            y = 1*(plantVisits)/(0.1+(plantVisits))
            a = sum(y)
            reps[ind] <- a
        }
        ggplot(NULL, aes(x=reps))+
            geom_histogram(aes(fill = supSp), binwidth = 1)+
            scale_y_continuous("Frequency", limits = c(0,100)) +
            scale_x_continuous("Seed Production",limits = c(0,100))+
            scale_fill_manual("Superspecies",
                              limits=c("Absent","Present"),
                              values = c("black","grey")) +
            ggtitle("Fig. 2") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text = element_text(size = 16),
                  axis.title = element_text(size=16),
                  axis.ticks = element_line(size = 1),
                  axis.ticks.length = unit(5,"pt"),
                  plot.title = element_text(size=18, face = "bold")) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
