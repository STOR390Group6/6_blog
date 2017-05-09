library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)
library(readxl)
teams <- read_excel("~/6_analysis/data/teams.xlsx")
load("~/6_analysis/data/gamelogs.Rda")
load("~/6_analysis/data/twitter_sentiment.Rda")

gamelogs <- gamelogs %>%
  rename(player = name, team = current, minutes= mp, game_score = gsc, plus_minus = pm) %>%
  na.omit()

#normalizing game score
gamelogs$game_score <- as.numeric(gamelogs$game_score)
gamelogs <- gamelogs %>%
  mutate(normalized_gamescore = (game_score-mean(gamelogs$game_score))/sd(gamelogs$game_score))

#normalizing sentiment
twitter_sentiment <- twitter_sentiment %>%
  mutate(normalized_sent = (sent-mean(twitter_sentiment$sent))/sd(twitter_sentiment$sent))

#team win %
teams <- separate(teams, col = streak, into = c('streak_type', 'streak_amt'), sep = " ") 
teams <- teams %>%
  mutate(result_bin = if_else(result == 'W', 1, -1)) %>%
  mutate(win_pct = current_wins/(current_wins + current_losses))

#team average sentiment over time
team_sent <- twitter_sentiment %>% 
  separate(col = created_at, into = c('date', 'time'), sep = " ") %>%
  group_by(Tm, date) %>%
  summarise(avg= mean(sent))


shinyServer(function(input, output){   
  


  #output$choice = unique(filter(twitter_sentiment, input$input_team)$Player)    
  
  
  output$plot_1 <- renderPlot({ 
    
       ggplot()+
       geom_point(data=filter(twitter_sentiment, Player == input$input_player), aes(x=created_at, y = normalized_sent, color = "blue")) +
        labs(title="Player Sentiment vs. Game Score", x="Date", y= "Normalized Level") +
        geom_smooth(data=filter(twitter_sentiment, Player == input$input_player), aes(x=created_at, y = normalized_sent,color="blue"), method=loess, se = FALSE) +
        theme_bw()+
    #  geom_point(data=filter(gamelogs, player== input$input_player), aes(x=as.POSIXct(date), y=normalized_gamescore))+
     geom_smooth(data=filter(gamelogs, player== input$input_player), aes(x=as.POSIXct(date), y=normalized_gamescore, color="red"),method=loess, se=FALSE)+
    scale_color_manual(name=NULL, labels=c("Sentiment", "Game Score"), values=c("blue", "red"))+
      theme(plot.title = element_text(face="bold", size=24, hjust=0)) +
      theme(axis.title = element_text(size=16))
    
    })
  
    output$plot_2 <- renderPlot({ 
  
  # team sentiment and win % over time
   team_sent %>% filter(Tm==input$input_team) %>%
    ggplot()+
     geom_point(aes(x=as.POSIXct(date), y=avg, color='blue'))+
    geom_point(data=filter(teams,team==input$input_team), aes(x=as.POSIXct(date, format= "%a, %b %d, %Y"), y=win_pct, color='red'))+
    geom_smooth(data=team_sent, aes(x=as.POSIXct(date), y=avg,  color='blue'), method="loess") +
    scale_color_manual(name=NULL, labels = c("Team Sentiment", "Team Win %"), values = c("blue", "red"))+
    labs(title="Team Win % vs Team Sentiment", x="Date", y="Win % and Sentiment")+
       theme_bw()+
        theme(plot.title = element_text(face="bold", size=24, hjust=0)) +
        theme(axis.title = element_text(size=16))
        
    
   })
  
  
  
  output$ui <- renderUI({
    
    
    selectInput(inputId = "input_player",    
                
                label = "Player",  
                
                choices = unique(filter(twitter_sentiment, Tm == input$input_team)$Player))
    
   
  })
  
})

  





