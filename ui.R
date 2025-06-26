#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny); library(purrr); library(dplyr); library(infer); library(progress)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Game of War: Investigating Inclusion of Jokers on Results"),
  navbarPage(  
      title = "Welcome!",
      # NavBar Top
      #page_navbar(
      #  nav_panel("App","")
      #)
      # Application title
      tabPanel("Introduction",
               mainPanel(tags$h2("Introduction", style = "font-size: 120%"), br(),
                          tags$p("War is a a classic card game between two players and is infamous for its potential to run for prolonged time periods. As a result, it delights youth and card game enthusiasts with its chaotic fun as a result of different rule systems that can be implemented to play the game.", style = "font-size: 115%"),

                          tags$p("Different rule sets include how many cards to include in war, whether to include jokers, which scoring schema to use, how many times to shuffle, whether to have a keep pile or add takes to the bottom of a draw pile, and more. Game theory classes and academia enjoy examination of this game due to how inclusion of the varied rules affect which player is more likely to win.", style = "font-size: 115%"),

                          tags$p("The simulator presented in this app models and simulates the War card game in order to build datasets to investigate how exclusion or inclusion of jokers in the deck affects game outcomes. In general, exclusion of jokers means that the four suits of cards with uniform value ranges are more uniformly distributed between the two players and, thus, provide each player with equal probabilities of winning. In the traditional game of War, the scoring schema from Ace to King is a numeric range of 1 to 13. The joker value is 14 and only two are included in a standard card deck. As a result, each player has has a probability of 1/27 of receiving one joker and an even slimmer chance of being dealt both.", style = "font-size: 115%"), 

                          tags$p("This app examines the hypothesis of how exclusion or inclusion of the jokers in the deck affects the game outcomes. In accordance with game theory, the working hypothesis is that there is a linear relationship between the rounds and games that a player wins and the number of rounds per game. When jokers are included, this evolves into data points clustering closer to a regression line and a greater fit represented by R^2 in linear regression. When jokers are excluded, data points should cluster farther away from the regression line and have a lower R^2 value.", style = "font-size: 115%")
               )),
      tabPanel("Game Overview",
               mainPanel(tags$h2("Game of War Objective", style = "font-size: 130%"),
                          tags$p("One player wins the whole deck of cards through successive rounds in a game. They may play successive games to determine an overall winner of the most games based on total games won per player.", style = "font-size: 115%"),
                          tags$p("There are two ways for an individual player to win a game:", style = "font-size: 115%"),
                          tags$ol(
                            tags$li("Win all of the cards in the deck through successive rounds and war", style = "font-size: 115%"),
                            tags$li("Win by default when the other player has too few cards to complete a round of war", style = "font-size: 115%")
                          ),
                          tags$h2("Game of War Walkthrough", style = "font-size: 130%"),
                          tags$h5("A) Setup:", style = "font-size: 110%"),
                          
                          tags$ol(tags$li("Shuffle and deal the deck evenly to the players:", style = "font-size: 115%"), 
                            tags$ul(tags$li("If jokers excluded from the deck:", style = "font-size: 115%"),
                              tags$ul(tags$li("Total Cards in Deck: 52", style = "font-size: 115%"),
                              tags$li("Total Cards per Player: 26", style = "font-size: 115%")
                              )),
                            tags$ul(tags$li("If jokers included in the deck:", style = "font-size: 115%"),
                              tags$ul(tags$li("Total cards in Deck: 54", style = "font-size: 115%"),
                              tags$li("Total cards per player: 27", style = "font-size: 115%"), br()
                              ))
                            ),
                          tags$h5("B) Play:", style = "font-size: 110%"),
                          tags$ol(tags$li("Each player draws top card from the deck and plays it facing up.", style = "font-size: 115%"),
                            tags$li("They compare the card values and the player with the largest value card wins both cards and the round.", style = "font-size: 115%"),
                            tags$li("If the cards have equal value, the players play a war round where they play three cards from the top of the deck facing down and a fourth card facing up. The value of the fourth card determines a winner or if they play another round of war. This step repeats as long as the players’ fourth cards continue to equal each other.", style = "font-size: 115%"),
                            tags$li("The winner takes all cards and adds them to the bottom of their deck without shuffling.", style = "font-size: 115%")
                          ), br(),
                          tags$h4("Winning Scenarios by Round:", style = "font-size: 120%"),
                          tags$ol(type="A",
                            tags$li("Card 1 > Card 2: Player 1 wins round and takes both cards.", style = "font-size: 115%"),
                            tags$li("Card 1 < Card 2: Player 2 wins round and takes both cards.", style = "font-size: 115%"),
                            tags$li("Card 1 == Card 2: Neither player wins round and now plays round of War.", style = "font-size: 115%"),
                            tags$ol(tags$li("Scenario A", style = "font-size: 115%"),
                                tags$li("Scenario B", style = "font-size: 115%"),
                                tags$li("Scenario C -> Repeat until either Scenarios A, B, or D occurs.", style = "font-size: 115%")
                              ),
                            tags$li("D) Either player runs out of cards and is unable to continue playing War.", style = "font-size: 115%"),
                          ),
                          tags$h4("Winning Scenarios by Game:", style = "font-size: 120%"),
                          tags$ol(type = "A", tags$li("Player 1 wins all of the cards.", style = "font-size: 115%"),
                            tags$li(" Player 2 wins all of the cards.", style = "font-size: 115%"),
                            tags$li("Player 1 has most of the cards and Player 2 didn’t have enough card to complete the last war round.", style = "font-size: 115%"),
                            tags$li("Player 2 has most of the cards and Player 1 didn’t have enough cards to complete the last war round.", style = "font-size: 115%")
                            ),
                          tags$h4("Deck Structure", style = "font-size: 120%"),
                          tags$p("The standard deck consists of 52 cards; each has the following different characteristics: face, suit, and number. The face characteristic includes different numbers of symbols consistent with the number in the upper left and lower right corners of the card or the picture of a person (i.e., Jack or Knight, Queen, and King). The numbers on the cards range from two to ten and the other four have: A for Ace, J for Jack, Q for Queen, and K for King. There are four possible suits: Diamonds, Hearts, Clubs, and Spades. On a traditional deck of cards, the suits diamonds and hearts are red and the suits spades and clubs are black.", style = "font-size: 115%"),
                          tags$p("The nontraditional deck that includes the jokers consists of 54 cards. It includes all of the cards in the traditional deck and two joker cards. The joker cards are distinguished from the traditional deck by the lack of numbers and suits. There are only two jokers and they feature the picture of a joker as a person and the letter J in the upper left and lower right corner of each card. Additionally, one of the jokers is red while the other is black.", style = "font-size: 115%"),
                          tags$h4("Card Values regardless of suit:", style = "font-size: 120%"),
                          tableOutput("table")
               )),
      tabPanel("Model Overview",
               mainPanel(
                  tags$h2("Model Overview", style = "font-size: 120%"),
                  tags$p("The game of war simulates a card deck with joker card inclusion based on the probability set by the user. The exclusion and inclusion of the jokers in the deck is determined by random sampling based on the probability of including jokers and the number of games. Thus, when the probability of joker inclusion is set to 0, most games will exclude jokers and, when the probability is set to 1, most of the games will include jokers.", style = "font-size: 115%"),
                  tags$p("The number of games as set by the user determines the total number of games to be simulated. Increasing the number of games directly increases the runtime of the simulation.", style = "font-size: 115%"),
                  tags$p("The simulation recognizes 4 scenarios that may end a game as follows:", style = "font-size: 115%"),
                  tags$ol(type="A",
                      tags$li("Player 1 wins all of the cards.", style="font-size: 115%"),
                      tags$li("Player 2 wins all of the cards.", style = "font-size: 115%"), 
                      tags$li("Player 1 has most of the cards and Player 2 didn’t have enough card to complete the last war round.", style = "font-size: 115%"), 
                      tags$li("Player 2 has most of the cards and Player 1 didn’t have enough cards to complete the last war round.", style = "font-size: 115%")
                  ),
                  tags$h4("Known Limitations:", style = "font-size: 115%"),
                  tags$ul(
                      tags$li("There is an unknown bug that can cause the simulation to stall. If the progress bar stalls in place, please refresh the page and rerun the simulation. If refresh does not work, please close the page, reopen it in a new page, and rerun the simulation.", style = "font-size: 115%"),
                      tags$li("Random sampling currently makes it unable to set equal data points for both cases of excluding and including jokers. This is being investigated to fix.", style = "font-size: 115%"),
                      tags$li("Only shows plot of the count of player 1 winning runs versus total rounds per game inclusive and exclusive of jokers. This will be addressed in later versions.", style = "font-size: 115%"),
                      tags$li("Successive war games are possible but may be the source of the bug causing runtime errors. Thus, the code attempts to limit wars to 1 round. This will be addressed in successive versions.", style = "font-size: 115%"),
                      tags$li("The simulation will run after the input parameters are set. The output is a single plot showing the relationship of player 1 rounds won to total rounds per game for both games where jokers were excluded and jokers were included.", style = "font-size: 115%"),
                  ),
                  tags$p("Please note as this app includes a simulation, it does require time to run and it has the potential to stall due to an unknown bug. If the simulation stalls, please close the current page and reopen it to reattempt the simulation.", style = "font-size: 115%")
                  )
               ),
      
      tabPanel("Instructions", 
               mainPanel(
                    tags$h3("Default simulation settings:", style = "font-size: 110%"),
                    tags$ul(tags$li("Probability of Joker Inclusion: 0.5", style = "font-size: 110%"),
                        tags$li("Number of Games: 80", style = "font-size: 110%")
                    ),
                    tags$h3("Instructions", style = "font-size: 110%"),
                    tags$ol(
                        tags$li("Set the probability of including the joker to on the first slider.", style = "font-size:110%"),
                        tags$li("Set the number of games on the second slider.", style = "font-size: 110%"),
                        tags$li("Press the “Start Simulation” button to run the simulation.", style = "font-size: 110%"),
                        tags$li("Wait while a progress bar displays in the bottom right of the screen as the simulation runs.", style = "font-size 110%"),
                        tags$li("Examine the plot of datasets including and excluding the joker from the deck.", style = "font-size: 110%")
                    ),
                        
              tags$p("I hope you enjoyed this app! :)", style = "font-size: 110%")
              )),
      
  # Question to answer: How does inclusion of jokers impact player wins?
  # Use a dataset where the jokers are included at a probability of 0.5 or 50%.
  # Explore graphs or regression lines of P1 and P2 wins to War counts for both
  # Jokers included in deck and Jokers not included in the deck. Should be 
  # an interactive plot 
  # Sidebar, letting people select number of runs between 50 to 500.
  # and checkboxes to include Jokers or Exclude...
      # Sidebar with a slider input for number of bins
      tabPanel("Simulator",
        sidebarLayout(
            sidebarPanel(
                #checkboxInput("checkbox", "Include Jokers?", FALSE),
                #verbatimTextOutput("value"),
                
                sliderInput("Pjoker",
                            "Probability of Jokers in Deck:",
                            min = 0,
                            max = 1,
                            value = 0.5,
                            step = 0.05),
                
                sliderInput("games",
                            "Number of Games:",
                            min = 20,
                            max = 150,
                            value = 70,
                            step = 5),
                
                actionButton("startButton", "Start Simulate War"),
                actionButton("stopButton", "Stop"),
                #actionButton("restartButton", "Restart"),
                textOutput("status")
                #textOutput("txt")
                
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("WarPlot")
            )
        )
    )
  )
)
