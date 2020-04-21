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
        plantVisits <- rep(0, numPlants)
        overlapAll <- c()
        reps <- rep(0,10)
        for(ind in 1:10){
            overlap <- c()
            for (i in 1:beeNum){
                # interactionList[[i]] <- sample(1:numPlants, size = plantSpBee)
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
            overlapAll <- c(overlapAll,overlap.perc)
        }
        # barplot(reps,names.arg = overlapAll, col = 'darkgray', ylab="Reproductive Output", ylim=c(0,100), main = "Fig 1", xlab="Replicates")
        # plot(overlapAll, reps, col = 'darkgray', ylab="Reproductive Output", ylim=c(0,100), xlim=c(0,1), main = "Fig 1", xlab="Niche Overlap")
        ggplot(NULL, aes(x=overlapAll,y=reps))+
            geom_point(size = 2) +
            scale_y_continuous("Reproductive Output", limits = c(0,100)) +
            scale_x_continuous("Proportion of Plants Visited",limits = c(0,1))+
            ggtitle("Fig. 1") +
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
        plantVisits <- rep(0, numPlants)
        # interactionList <- list()
        reps <- rep(0,10)
        b <- rep(0,10)
        if (supSpPresent){
            
            b <- sample(1:5, size =10, replace = TRUE) # the superspecies
            
        } 
        for(ind in 1:10){
            for (i in 1:beeNum){
                mult <- 1.0
                if (i==b[ind]){
                    mult <- 8
                }
                # interactionList[[i]] <- sample(1:numPlants, size = plantSpBee)
                a <- sample(1:numPlants, size = plantSpBee*mult)
                
                for (j in a){
                    plantVisits[j] <-  plantVisits[j] + beePerPlant*mult
                }
                
            }
            y = 1*(plantVisits)/(0.1+(plantVisits))
            a = sum(y)
            reps[ind] <- a
        }
        barplot(reps,col = 'darkgray', ylab="Reproductive Output", ylim=c(0,100), main = "Fig 2", xlab="Replicates")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
