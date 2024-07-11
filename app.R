# --------------------------------------#
max      <- 100
nGames   <- 1000 # results converge before this.
result   <- data.frame(matrix(NA, nrow = nGames, ncol = 3))
colnames(result) <- c("Ngos", "animalsSaved", "win")


#------------------------------------------
# Libraries
#------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(reshape2)
#GF Colors 
GFgreen      = "#009a4e"
colorVec2    = c("#eed1e5", "#006032", "#1AB068")##DA39AF")
#greenvec    = c("#006032", "#007D41", "#1AB068", "#3EBA7E")
HTMcolor    = c("#EE0C3DFF", "#2E4DF9FF", "#F6DE00FF")
#GFlightBlue = "#6E2C6BFF"
#GForange    = "#FF7F45"
#GFgold      = "#C6AC00"
#GFTBvec     = c("#0066b3", "#447fc1", "#80a0d3", "#bbc9e7")
#malvec      = c("#fbab18", "#fdbe59", "#ffd28e", "#ffe7c2")

#------------------------------------------
# UI 
#------------------------------------------
ui <- fluidPage(
    h2("Lisa's animal game [get actual name]"), 
    h4("CONCEPT: There are animals which need to move from one ice patch, to a bridge that is melting, to a second ice patch."),
    h4("TO WIN: Get all the animals to the second ice patch, via the bridge, before it melts."),
    h5("TO PLAY: Roll a six sided dice which may equally land on one of three options: BRIDGE to move an animal from the first ice patch (if there are any on the first ice patch); 
    SAVE to move an animal from the bridge to the second ice patch (if there are any on the bridge); 
    MELT to remove one of the bridge pillars."),
    fixedRow(
      column(3,
             wellPanel(
               fixedRow(
                 column(12,
                        h3(selectInput(inputId  = "N_animals",
                                       label    = "Number of animals to save to win the game", 
                                       choices  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                       multiple = FALSE,
                                       selected = "4")),
                        h3(selectInput(inputId  = "N_pillars",
                                       label    = "Number of bridge pillars that can melt before the game is lost", 
                                       choices  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                       multiple = FALSE,
                                       selected = "5")),
                 ),),
               ), # close well panel
               ), #close column
      column(9,
             # Output: Tabset w/ plot, summary, and table ----
             tabsetPanel(type = "tabs",
                         tabPanel("Result", h1(textOutput("text"), plotOutput("plot"))),
                         tabPanel("Geek info", plotOutput("plotDist"))),
      ), #close column
    )
) # close fluidPage

#------------------------------------------
# Server
#------------------------------------------
server <- function(input, output, session) {
  
    get.data1 = reactive({
    
    N_animals  <- input$N_animals
    N_pillars  <- input$N_pillars
    
    for (kg in 1:nGames){
      nM            <- 0
      savedAnimals  <- 0
      onBridge      <- 0
      for (k1 in 1:max){
        # randomly sample (dice throw) either 
        ## 1 (move animal to bridge)
        ## 2 (save animal from melting bridge)
        ## 3 (melt bridge)
        thisGo  <- sample(c(1,2,3),1) 
        # If BRIDGE is thrown
        if (thisGo == 1){
          onBridge <- onBridge + 1 # Counter for number of animals on bridge
        }
        # If SAVE is thrown
        if (thisGo == 2){
          if(onBridge > 1){
            onBridge     <- onBridge - 1 # if animal on bridge - save it
            savedAnimals <- savedAnimals + 1} # number of saved animals
        }
        # If MELT is thrown
        if (thisGo == 3){
          nM <- nM + 1 # Counter for number of bridge pillars removed
        }
        # To win
        if (savedAnimals == N_animals){
          result[kg,1]  <- k1
          result[kg,2]  <- savedAnimals
          result[kg,3]  <- 1
          break
        }
        # To lose
        if (nM > N_pillars){
          result[kg,1]  <- k1
          result[kg, 2] <- savedAnimals
          result[kg, 3] <- 0
          break}  
      }
    }
    
    
    data = result
  })
  
  
   output$plot = renderPlot({
    result = get.data1()
    DF <- result %>% dplyr::group_by(animalsSaved) %>% dplyr::summarise(n = n())
    DF$perc <- DF$n / sum(DF$n)
    DF$animalsSaved <- factor(DF$animalsSaved, levels = rev(DF$animalsSaved))
    DF$fill <- "lose"
    DF$fill[which(DF$animalsSaved == input$N_animals)] <- "win"
    g2 <- ggplot(DF, aes(x = animalsSaved, y = perc, fill = as.factor(fill))) + 
      geom_bar(stat = "identity", alpha = 0.8)  + 
      scale_fill_manual(values = c("#EE0C3DFF", "#009a4e"), name = "")+ 
      scale_x_discrete(name = "Number of animals saved") + 
      scale_y_continuous(name = "Probability") + 
      theme_bw(base_size = 24) + theme(legend.position = c(0.8, 0.8)) 
    g2
  })
  
  output$text = renderText({
    result = get.data1()
    text <-  paste0("        The probability of winning is ",sum(result$win) / nrow(result))
  })
  
  output$plotDist = renderPlot({
    result = get.data1()
    
    result$win[which(result$win == 1)] = "win"
    result$win[which(result$win == 0)] = "lose"
    gg <- ggplot(result, aes(Ngos, win, fill = win)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1) + 
      scale_fill_manual(values = c("#EE0C3DFF", "#009a4e"), name = "")+ 
      scale_x_continuous(name = "Number of dice throws to complete the game") + 
      scale_y_discrete(name = "") + 
      theme_bw(base_size= 24)
    gg
  })
  
  
} # close server

#------------------------------------------
# Launch
#------------------------------------------
shinyApp(ui, server, options = list(launch.browser = TRUE))
