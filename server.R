#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny);library(purrr); library(dplyr); library(infer); library(progress)
# Function Declarations
CreateDeck <- function(JokerSwitch, MetricSwitch){
  # Generates a standard card deck with or without a joker based on user input.
  # Deck will be a data frame with suit, face, and value in a (column by row)
  # 3 x 52 data frame.
  # Basic setup of a deck without Joker
  suits <- c('spades', 'clubs', 'diamonds', 'hearts')
  suit <- unlist(map(suits, rep, 13))
  faces <- c('king', 'queen', 'jack', 'ten', 'nine', 'eight', 'seven', 'six', 'five', 'four', 'three', 'two', 'ace')
  face <- rep(faces, 4)
  value <- rep(13:1, 4)
  player <- rep(0, 52) # indicates which card belongs to which player deck when combined
  
  if(MetricSwitch == 1){
    warz <- rep(0, 52) # war counter
    p1roundswon <- rep(0, 52) # couter for rounds won by Player 1
    p2roundswon <- rep(0, 52) # counter for rounds won by Player 2
    p1warswon <- rep(0, 52) # counter for wars won by Player 1
    p2warswon <- rep(0, 52) # counter for wars won by Player 2
    deck <- data.frame(face, suit, value, player, warz, p1roundswon, p2roundswon, p1warswon, p2warswon)
  } else {
    deck <- data.frame(face, suit, value, player)
  }
  # if Joker is included, Joker = 1
  if(JokerSwitch == 1){
    face <- c('joker', 'joker')
    suit <- c('none', 'none')
    value <- rep(14, 2)
    player <- c(0,0)
    warz <- c(0,0)
    p1roundswon <- warz; p2roundswon <- warz
    p1warswon <- warz; p2warswon <- warz
    if( MetricSwitch == 1){
      deck2 <- data.frame(face, suit, value, player, warz, p1roundswon, p2roundswon, p1warswon, p2warswon)
    } else {
      deck2 <- data.frame(face, suit, value, player)
    }
    
    deck <- rbind(deck, deck2)
  } 
  
  return(deck)
}

CardDeck <- function(Jkr){
  suits <- c('spades', 'clubs', 'diamonds', 'hearts')
  suit <- unlist(map(suits, rep, 13))
  faces <- c('king', 'queen', 'jack', 'ten', 'nine', 'eight', 'seven', 'six', 'five', 'four', 'three', 'two', 'ace')
  colors <- c('black','red')
  face <- rep(faces, 4)
  value <- rep(13:1, 4)
  color <- unlist(map(colors, rep, 26))
  
  duckies <- data.frame(face, suit, color, value)
  
  if(Jkr == 1){
    face <- c('joker', 'joker')
    suit <- c('none', 'none')
    value <- rep(14, 2)
    color <- c('black','red')
    ducky <- data.frame(face, suit, color, value)
    duckies <- rbind(duckies, ducky)
  } 
  
  return(duckies)
}

Shuffle <- function(ndeck){
  # Shuffle the deck via random resampling N times.
  return(sample_n(ndeck, dim(ndeck)[1], replace=F))
}

MultiShuffle <- function(ndeck, N){
  Tdeck <- ndeck
  for(i in 1:N){
    Tdeck <- Shuffle(Tdeck)
  }
  return(Tdeck)
}

Deal <- function(Ndeck){
  # Deals the cards evenly into two decks.
  odds <- seq_len(nrow(Ndeck)) %% 2
  Tdeck1 <- Ndeck[odds == 1,]; Tdeck1$player <- 1 # deal to player 1
  Tdeck2 <- Ndeck[odds == 0,]; Tdeck2$player <- 2 # deal to player 2
  return( rbind(Tdeck1, Tdeck2) )
}

PlayGame3 <- function(Game, GameData, Joker, Metric){
  # Setup Decks & Deal
  Deck <- CreateDeck(Joker,Metric); Deck <- MultiShuffle(Deck, 2); Deck <- Deal(Deck)
  D1 <- Deck[Deck$player == 1,]; D2 <- Deck[Deck$player == 2,]
  r <- 1
  Winner <- -1
  
  # Initial Joker counting.
  GameData[Game, 6] <- sum(D1$face == 'joker', na.rm = TRUE) # Increase P1 jokers
  GameData[Game, 7] <- sum(D2$face == 'joker', na.rm = TRUE) # Increase P2 jokers
  
  # while or first for loop would start here
  while(Winner != 1 | Winner != 2){
    if(is.null(D1) == TRUE){
      Winner <- 2
      break
    }
    if(is.null(D2) == TRUE){
      Winner <- 1
      break
    }
    
    # Determine everywhere D1 > D2, D1 < D2, and D1 = D2.
    D1greaterD2 <- as.numeric(D1$value > D2$value)
    D1lesserD2 <- as.numeric(D1$value < D2$value)
    D1equalsD2 <- as.numeric(D1$value == D2$value)
    #Comparison <- data.frame(P1equalsP2 = as.numeric(D1$value == D2$values), P1greaterP2 = as.numeric(D1$value > D2$vaue), P1lesserP2 = as.numeric(D1$value < D2$value))
    
    # Scenarios:
    # A)   D1 > D2: D1 wins. -> D1 takes both cards.
    # B).  D1 < D2: D2 wins. -> D2 takes both cards.
    # C).  D1 = D2: War.     -> Next six cards (3 from D1 and 3 from D2) are up for graps. 
    #.                       -> Next two cards (1 from D1 and 1 from D2) are drawn to repeat comparison.
    #                       -> It repeats until Scenario A or B occurs.
    
    # Estimate where P1 wins and P2 loses:
    # for loop to create a sequence of indices for the next for loop sorting through 
    # cards to drive scenarios. Initiate Bag of Holding to hold indices
    WarIDs <- which(D1$value == D2$value) # gives indices where War will occur
    Bag0Holding <- data.frame(A = as.numeric(D1greaterD2), B = as.numeric(D1lesserD2), War = as.numeric(D1equalsD2))
    Bag0Holding0 <- Bag0Holding
    # Designate cards to be pulled for War
    if(length(WarIDs) >= 1){
      WarSeq <- c(-1,-1,-1)
      for(i in 1:length(WarIDs)){
        
        if(WarIDs[i] %in% WarSeq){
          next
        } else {
          Bag0Holding[WarIDs[i]:WarIDs[i]+3,3] <- 1 # Set cards for War to 1
          Bag0Holding[WarIDs[i]:WarIDs[i]+3,1:2] <- 0 # Set cards for A, B to 0
          #Bag0Holding[WarIDs[i]:WarIDs[i]+3,4:5] <- 0 # Set cards for D1, D2 to 0
          WarSeq <- c(WarIDs[i]:WarIDs[i] + 3)
        }
      }
    } 
    
    
    # Conduct the game.
    for(i in 1:dim(Bag0Holding)[1]){
      #GameData[Game, 12] <- GameData[Game, 12] + 1 # Update Total Rounds
      GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker', na.rm = TRUE) # Increase P1 jokers
      GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker', na.rm = TRUE) # Increase P2 jokers
      
      if(is.na(any(Bag0Holding[i,])) == TRUE){
        next
      }
      #print(paste("Round ",i))
      Cards <- rbind(D1[1,], D2[1,]) # Extract cards to be dealt
      #print(dim(D1)); print(dim(D2))
      #print(Bag0Holding[i,])
      if(dim(D1)[1] < 1 | dim(D2)[1] < 4 | is.null(D1) == TRUE | is.null(D2) == TRUE){ # Check for Scenarios D1 & D2
        # This ends the game. 
        if(dim(D2)[1] < 1 | is.null(D2) == TRUE){
          # Scenario A: P1 wins and takes cards because P2 didn't have enough cards.
          D1 <- rbind(D1, Cards, D2); D2 <- NULL; Cards <- NULL
          Winner <- 1
          D1$player <- 1
          GameData[Game, 4] <- GameData[Game,4] + 1 # Increase P1 wins by 1
          GameData[Game, 8] <- GameData[Game, 8] + 1 # Increase P1 rounds won by 1
          GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker', na.rm = TRUE) # Increase P1 jokers 
          break
        }  else if(dim(D1)[1] < 1 | is.null(D1) == TRUE){
          # Scenario B: P2 wins and takes cards because P1 didn't have enough cards.
          D2 <- rbind(D2, Cards, D1); D1 <- NULL; Cards <- NULL
          D2$player <- 2
          Winner <- 2
          GameData[Game, 5] <- GameData[Game, 5] + 1 # Increase P2 wins by 1
          GameData[Game, 9] <- GameData[Game, 9] + 1 # Increase P2 rounds won by 1
          GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker', na.rm = TRUE) # Increase P2 jokers
          break
        }
      } else if(Bag0Holding[i,1] == 1){
        #print("P1 won this round.")
        
        # Scenario A: P1 wins and takes cards for this round.
        D1 <- rbind(D1, Cards); D1 <- D1[-1,]; D2 <- D2[-1,]
        D1$player <- 1
        #GameData[Game, 4] <- GameData[Game,4] + 1 # Increase P1 wins by 1
        GameData[Game, 8] <- GameData[Game, 8] + 1 # Increase P1 rounds won by 1
        #GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker') # Increase P1 jokers 
        #GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker') # Increase P2 jokers
      } else if(Bag0Holding[i,2] == 1){
        #print("P2 won this round.")
        # Scenario B: P2 wins and takes cards for this round.
        D2 <- rbind(D2, Cards); D1 <- D1[-1,]; D2 <- D2[-1,]
        D2$player <- 2
        #GameData[Game, 5] <- GameData[Game, 5] + 1 # Increase P2 wins by 1
        GameData[Game, 9] <- GameData[Game, 9] + 1 # Increase P2 rounds won by 1
        #GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker') # Increase P2 jokers
        #GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker')
      } else if(Bag0Holding[i,3] == 1){
        #print("P1 & P2 went to War.")
        # Scenario C: War. 
        #War <- -1
        #GameData[Game, 2] <- GameData[Game, 2] + 1 # Increase War Counter by 1
        #Cards <- rbind(D1[1:4,], D2[1:4,]) # Redo card extraction from D1 & D2.
        if(dim(D2)[1] < 4 | is.null(D2) == TRUE){
          # Scenario A: P1 wins and takes cards because P2 didn't have enough cards.
          D1 <- rbind(D1, Cards, D2); D2 <- NULL; Cards <- NULL
          Winner <- 1
          D1$player <- 1
          GameData[Game, 4] <- GameData[Game,4] + 1 # Increase P1 wins by 1
          GameData[Game, 8] <- GameData[Game, 8] + 1 # Increase P1 rounds won by 1
          GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker', na.rm = TRUE) # Increase P1 jokers 
          break
        }  else if(dim(D1)[1] < 4 | is.null(D1) == TRUE){
          # Scenario B: P2 wins and takes cards because P1 didn't have enough cards.
          D2 <- rbind(D2, Cards, D1); D1 <- NULL; Cards <- NULL
          D2$player <- 2
          Winner <- 2
          GameData[Game, 5] <- GameData[Game, 5] + 1 # Increase P2 wins by 1
          GameData[Game, 9] <- GameData[Game, 9] + 1 # Increase P2 rounds won by 1
          GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker', na.rm = TRUE) # Increase P2 jokers
          break
        }
        Cards <- rbind(D1[1:4,], D2[1:4,]) # Redo card extraction from D1 & D2.
        War <- -1
        GameData[Game, 2] <- GameData[Game, 2] + 1 # Increase War Counter by 1
        if(Cards[4,3] > Cards[8,3]){
          #print("P1 won the War.")
          # Scenario A: P1 wins and takes cards.
          D1 <- rbind(D1, Cards); D1 <- D1[-c(1:4),]; D2 <- D2[-c(1:4),]
          D1$player <- 1
          #GameData[Game, 4] <- GameData[Game,4] + 1 # Increase P1 wins by 1
          GameData[Game, 8] <- GameData[Game, 8] + 1 # Increase P1 rounds won by 1
          #GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker') # Increase P1 jokers
          GameData[Game, 10] <- GameData[Game, 10] + 1
        } else if(Cards[4,3] < Cards[8,3]){
          #print("P2 won the War.")
          #Scenario B: P2 wins and takes cards.
          D2 <- rbind(D2, Cards); D2 <- D2[-c(1:4),]; D1 <- D1[-c(1:4),]
          D2$player <- 2
          #GameData[Game, 5] <- GameData[Game, 5] + 1 # Increase P2 wins by 1
          GameData[Game, 9] <- GameData[Game, 9] + 1 # Increase P2 rounds won by 1
          #GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker') # Increase P2 jokers
          GameData[Game, 11] <- GameData[Game, 11] + 1
        } else {
          # In case of a second war! 
          
        }
      } else {#if(any(Bag0Holding[i,]) %in% c(-1,0) | is.na(any(Bag0Holding[i,])) == TRUE) {
        next
      }
      
    }
    
    if(is.null(D1) == TRUE){
      Winner <- 2
      break
    }
    if(is.null(D2) == TRUE){
      Winner <- 1
      break
    }
    
    # Update round counter
    r <- r + 1
    #print(dim(D1)); print(dim(D2))
    if(dim(D1)[1] < 4 | dim(D2)[1] < 4 | is.null(D1) == TRUE | is.null(D2) == TRUE){
      if(dim(D2)[1] < 4 | is.null(D2) == TRUE ){
        # Scenario A: P1 wins and takes cards because P2 didn't have enough cards.
        D1 <- rbind(D1, Cards, D2); D2 <- NULL; Cards <- NULL
        Winner <- 1
        D1$player <- 1
        GameData[Game, 4] <- GameData[Game,4] + 1 # Increase P1 wins by 1
        GameData[Game, 8] <- GameData[Game, 8] + 1 # Increase P1 rounds won by 1
        GameData[Game, 6] <- GameData[Game, 6] + sum(D1$face == 'joker', na.rm = TRUE) # Increase P1 jokers 
        break
      }  else if(dim(D1)[1] < 4 | is.null(D1) == TRUE){
        # Scenario B: P2 wins and takes cards because P1 didn't have enough cards.
        D2 <- rbind(D2, Cards, D1); D1 <- NULL; Cards <- NULL
        D2$player <- 2
        Winner <- 2
        GameData[Game, 5] <- GameData[Game, 5] + 1 # Increase P2 wins by 1
        GameData[Game, 9] <- GameData[Game, 9] + 1 # Increase P2 rounds won by 1
        GameData[Game, 7] <- GameData[Game, 7] + sum(D2$face == 'joker', na.rm = TRUE) # Increase P2 jokers
        break
      }
    }
    
    if(is.null(D1) == TRUE){
      Winner <- 2
      break
    }
    if(is.null(D2) == TRUE){
      Winner <- 1
      break
    }
    
  }
  GameData[Game,12] <- GameData[Game,8] + GameData[Game,9]
  return(GameData)
}

SimulateWar2 <- function(TotalGames, JokerProbability, MetricCheck){
  withProgress( 
    message = 'Simulation in progress', 
    detail = 'Please wait. This may take a while...', 
    value = 0, 
    {
      # Initiate variables
      if(MetricCheck == 1){
        GameData <- data.frame(Game = seq(1:TotalGames), WarCount = 0, JokersOrNot = 0, P1Wins = 0, P2Wins = 0, P1Jokers = 0, P2Jokers = 0, P1RoundsWon = 0, P2RoundsWon = 0, P1WarsWon = 0, P2WarsWon = 0, TotalRounds = 0)
        JokerGames <- sample(c(0,1), size=TotalGames, replace = TRUE, prob = c(1 - JokerProbability, JokerProbability))
      } else {
        GameData <- data.frame(Game = seq(1:TotalGames), WarCount = 0, JokersOrNot = 0 ,P1Wins = 0, P1Jokers = 0, P2Wins = 0, P2Jokers = 0)
        JokerGames <- sample(c(0,1), size=TotalGames, replace = TRUE, prob = c(1 - JokerProbability, JokerProbability))
      }  
      
      GameData$JokersOrNot <- JokerGames
      
      #pb <- txtProgressBar(min = 0, max = TotalGames, style = 3, width = 50, char = "=")
      
      for(n in 1:TotalGames){
        #print("Game"); print(n)
        GameData <- PlayGame3(n, GameData, JokerGames[n], MetricCheck)
        incProgress(1/TotalGames) 
        #print(GameData[n,])
        # Sets the progress bar to the current state
        #setTxtProgressBar(pb,n)
      }
      text("Done simulating!")
      #close(pb)
      return(GameData) 
    })
}


# Define server logic required to simulate Game of War and create a plot
#function(input, output, session) {
    #text <- reactiveVal()
    #output$txt <- renderText({ text() })
    #output$WarPlot <- renderPlot({
      # Generate Data to plot
    
   #   observe(
  #      suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1)),
 #       plot(GameCube$WarCount, GameCube$P1WarsWon)
      
#      ) |> bindEvent(input$run)
      
      #plot(GameCube$WarCount, GameCube$P1WarsWon)
      
#    })
#}
    
    #output$distPlot <- renderPlot({
    #  
    #    plot(GameCube$WarCount, GameCube$P1WarsWon)
    #  
    #  })

    
    
#    
#}
function(input, output, session) {
  # Variable to track the process status
  process_running <- reactiveVal(FALSE) 
  
  output$table <- renderTable(CardDeck(1))
  
  # Stop button functionality
  observeEvent(input$stopButton, {
    # Code to stop your process
    process_running(FALSE) 
    output$status <- renderText("Process Stopped!")
  })
  
  # Start button functionality
  observeEvent(input$startButton, {
    # Code to start your process
    process_running(TRUE) 
    output$status <- renderText("Process Started!")
    
    output$WarPlot <- renderPlot({
      
      # generate data
      suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1))
      
      # Segment
      WoJokers <- GameCube[GameCube$JokersOrNot == 0,]
      WJokers <- GameCube[GameCube$JokersOrNot == 1,]
      
      # run calculations
      reg.fitA <- lm(P1RoundsWon ~ TotalRounds, data = WoJokers)
      text_Ax <- min(WoJokers$TotalRounds) + (max(WoJokers) - min(WoJokers)) * 0.05
      text_Ay <- max(WoJokers$TotalRounds) + (max(WoJokers) - min(WoJokers)) * 0.15
      
      reg.fitB <- lm(P1RoundsWon ~ TotalRounds, data =  WJokers)
      text_Bx <- min(WJokers$TotalRounds) + (max(WJokers) - min(WoJokers)) * 0.05
      text_By <- max(WJokers$TotalRounds) + (max(WJokers) - min(WJokers)) * 0.15
      
      
      # draw plots
      # test plot & placeholder
      par(mfrow=c(2,2), fig=c(0.0, 0.8, 0.0, 0.8), new = TRUE)
      plot(WoJokers$TotalRounds, WoJokers$P1RoundsWon, xlab = "Total Rounds per Game", ylab = "# Rounds P1 Won per Game", col = "orange", pch = 19, lwd = 3, cex=1.5)
      abline(reg.fitA, col="orange", lwd = 3)
      text(text_Ax, text_Ay, labels = paste("Equation: y = ", round(coef(reg.fitA)[2], 2), "x + ", round(coef(reg.fitA)[1],2), "\nR-squared = ", round(summary(reg.fitA)$r.squared, 2)), pos = 4)
      points(WJokers$TotalRounds, WJokers$P1RoundsWon, col="blue", pch = 5, cex = 1.5)
      abline(reg.fitB, col="blue", lwd=2)
      par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
      boxplot(WoJokers$TotalRounds, col="NA", border ="orange", horizontal = TRUE, axes = FALSE, lwd = 3)
      boxplot(WJokers$TotalRounds, col = "NA", border ="blue", horizontal = TRUE, axes = FALSE, add = TRUE, yaxt = "n", lwd = 2)
      par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
      boxplot(WoJokers$P1RoundsWon, col="NA", border = "orange", axes = FALSE, lwd = 3)
      boxplot(WJokers$P1RoundsWon, col = "NA", border = "blue", axes = FALSE, add = TRUE, yaxt = "n", lwd = 2)
      mtext("Games w/o Jokers (left) & Games w/ Jokers (right)", side = 3, outer=TRUE, line = -3, cex=1.5)
      legend("topright", legend = c("Exclude Jokers", "Include Jokers"), col=c("orange", "blue"), pch=c(1, 5))
      
      #par(fig = c(2, 1.8, 2, 1.8), new = TRUE)
      #plot(WJokers$TotalRounds, WJokers$P1ROundsWon, xlab = "Total Rounds per Game", ylab = "# Rounds P1 Won per Game")
      #abline(reg.fitB, col = "red", lwd = 2)
      #text(text_Bx, text_By, labels = paste("Equation: y = ", round(coef(reg.fitB)[2], 2), "x + ", round(coef(reg.fitB)[1],2), "\nR-squared = ", round(summary(reg.fitB)$r.squared, 2)), pos = 4)
      #par(fig=c(1, 1.8, 2.55, 2), new=TRUE)
      #boxplot(WJokers$TotalRounds, horizontal = TRUE, axes = FALSE)
      #par(fig = c(2.65, 1, 2, 1.8), new = TRUE)
      #boxplot(WJokers$P1RoundsWon, axes = FALSE)
      #mtext("Games w/o Jokers (left) & Games w/ Jokers (right)", side = 3, outer=TRUE, line = -3)
           
      
    })
    
  })
  
  # Stop button functionality
  observeEvent(input$stopButton, {
    # Code to stop your process
    process_running(FALSE) 
    output$status <- renderText("Process Stopped!")
  })
}
  # Restart button functionality (example: stop then start again)
  #observeEvent(input$restartButton, {
    # Code to stop your process
  #  process_running(FALSE) 
    # Code to start your process
   # process_running(TRUE)
  #  output$status <- renderText("Process Restarted!") 
  #})
  
  #output$WarPlot <- renderPlot({
    
    # generate data
  #  suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1))
    
    # draw plots
    # test plot & placeholder
   # plot(GameCube$WarCount, GameCube$P1WarsWon)
    
  #})
  
#}

#function(input, output, session){
  
  # Reactive value to control stopping
#  text <- reactiveVal(FALSE)
  
  # Observer for the start button
 # observeEvent(input$startButton, {
    # Reset sto flag
  #  text(FALSE)
    
    # Generate Data
   # GameCube <- SimulateWar2(input$games, input$Pjoker, 1)
    
    # 
    #output$status <- renderText("Simulation complete.")
    
    # Generate plot
    #output$WarPlot <- plot(GameCube$WarCount, GameCube$P1WarsWon)
    
#})


  # Observer for stop button
  #observeEvent(input$stopButton, {
  #  text(TRUE)
  #  output$status <- renderText("Stopping simulation...")
  #})
  
  #output$WarPlot <- renderPlot({
    
   # plot(GameCube$WarCount, GameCube$P1WarsWon)
    
  #})
  
  #output$WarPlot <- plot(GameCube$WarCount, GameCube$P1WarsWon)
#}
  
#

#function(input, output, session) {
##    text <- reactiveVal()
#    output$txt <- renderText({ text() })
#    output$distPlot <- renderPlot({
#      # Generate Data to plot
#    
#    observe(
#      suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1))
#      ) |>  bindEvent(input$run)
#      
#      #plot(GameCube$WarsWon, GameCube$P1WarsWon)
#      
#    })
#    
#}




#function(input, output, session) {
  
#  observe(
#    output$WarPlot <- renderPlot({
#      # generate game data
#      suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1))
#      
#      # plot diagrams
#      plot(GameCube$WarCount, GameCube$P1RoundsWon)
#    })
#    
#  ) |> bindEvent(input$run)
#  
#}

#function(input, output, session) {
#  r <- reactiveValues(
#    simulating = FALSE,
#    data = NULL
#  )
#  
#  observe({
#    if (isTRUE(r$simulating)) {
#      # Your simulation code goes here
#      # ...
#      # Update the reactive value with new data
#      # r$data <- simulated_data
#      # generate game data
#      observe(
#        suppressWarnings(GameCube <- SimulateWar2(input$games, input$Pjoker, 1))
#      ) |> bindEvent(input$run)
#      # Render the plot
#      output$WarPlot <- renderPlot({
#        # Plot the data stored in r$data
#        # ...
#        # plot diagrams
#        plot(GameCube$WarCount, GameCube$P1RoundsWon)
#      })
#    }
#  })
#  
#  observeEvent(input$start_sim, {
#    r$simulating <- TRUE
#  })
#  
#  observeEvent(input$stop_sim, {
#    r$simulating <- FALSE
#  })
#  
#  #observeEvent(input$restart_sim, {
#  #  r$simulating <- FALSE
#  #  # Reset simulation parameters/data here
#    # ...
#  #})
#  
#  
#  
#}
