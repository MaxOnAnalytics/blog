
# title: "Fantasy Football Salary Cap Lineup Optimizer"
# author: "Bryant Molloy"
# date: "September 5, 2020"


## Load Libraries and Source Code

library(shiny)
source("Optimal Salary Cap Lineup Functions.R")


# Create Shiny App with filters by Scoring, Cost Metric, Lineups, % of Budget to Spend, QBs, RBs, WRs, TEs, FLEXs, and Reduce Player Impact

# Shiny App

ui <- fluidPage(
    
    titlePanel(
        h1("Fantasy Football Salary Cap Lineup Optimizer", align = "center", 
           h4("Using Point Projections from FantasyPros and Projected Salary/AAV from Yahoo", align = "center", 
              h6("Set Reduce Player Impact to Yes to decrease player point projection by 5% 
                 after each inclusion (to show wider variety of lineups)", align = "center")))
    ),  
    
    sidebarLayout(
        
        
        sidebarPanel(
            
            radioButtons(inputId = 'scoring', 
                           label = 'Scoring System: ', 
                           choices = c("Standard", "Half PPR", "PPR")), 
            
            radioButtons(inputId = 'cost_metric', 
                           label = 'Player Cost Metric: ', 
                           choices = c("Projected Salary", "AAV")), 
            
            sliderInput(inputId = 'lineups',
                        label = 'Lineups to Create: ',
                        min = 1, max = 100, value = 10, step = 1, round = T), 
            
            sliderInput(inputId = 'budget_spent_pct',
                         label = '% of Budget to Spend: ',
                         min = 0, max = 100, value = 70, step = 1, round = T), 
            
            radioButtons(inputId = 'qb_slots', 
                         label = 'QBs: ', 
                         choices = c(1:2), 
                         selected = 1, 
                         inline = T), 
            
            radioButtons(inputId = 'rb_slots', 
                         label = 'RBs: ', 
                         choices = 1:3, 
                         selected = 2, 
                         inline = T), 
            
            radioButtons(inputId = 'wr_slots', 
                         label = 'WRs: ', 
                         choices = c(1:4), 
                         selected = 2, 
                         inline = T), 
            
            radioButtons(inputId = 'te_slots', 
                         label = 'TEs: ', 
                         choices = c(1:2), 
                         selected = 1, 
                         inline = T), 
            
            radioButtons(inputId = 'flex_slots', 
                         label = 'FLEXs: ', 
                         choices = c(1:2), 
                         selected = 1, 
                         inline = T), 
            
            radioButtons(inputId = 'reduce_player_impact', 
                         label = "Reduce Player Impact?", 
                         choices = c("Yes", "No"), 
                         selected = "No")
            
        ), #sidebarPanel 
        
        mainPanel(
            
            tabsetPanel(
                
                tabPanel('All Lineups', reactableOutput('lineup_wide')), 
                tabPanel('Lineup Breakdown', reactableOutput('lineup_long'))
                
            ) #tabsetPanel
            
            
        ) #mainPanel
        
    ) #sidebarLayout
    
)



server <- function(input, output){
    
    output$lineup_wide <- renderReactable({
        
        reactable(
            get_optimal_lineup(scoring = input$scoring, 
                               cost_metric = input$cost_metric, 
                               lineups = input$lineups, 
                               budget_spent_pct = input$budget_spent_pct, 
                               qb_slots = as.numeric(input$qb_slots), 
                               rb_slots = as.numeric(input$rb_slots),
                               wr_slots = as.numeric(input$wr_slots), 
                               te_slots = as.numeric(input$te_slots), 
                               flex_slots = as.numeric(input$flex_slots),
                               reduce_player_impact = ifelse(input$reduce_player_impact == "Yes", T, F),
                               long = F), 
            defaultPageSize = 10)
        
    })
    
    output$lineup_long <- renderReactable({
        
        reactable(
            get_optimal_lineup(scoring = input$scoring, 
                               cost_metric = input$cost_metric, 
                               lineups = input$lineups, 
                               budget_spent_pct = input$budget_spent_pct, 
                               qb_slots = as.numeric(input$qb_slots), 
                               rb_slots = as.numeric(input$rb_slots),
                               wr_slots = as.numeric(input$wr_slots), 
                               te_slots = as.numeric(input$te_slots), 
                               flex_slots = as.numeric(input$flex_slots),
                               reduce_player_impact = ifelse(input$reduce_player_impact == "Yes", T, F),
                               long = T), 
            defaultPageSize = as.numeric(input$qb_slots) + as.numeric(input$rb_slots) + as.numeric(input$wr_slots) + 
                as.numeric(input$te_slots) + as.numeric(input$flex_slots))
        
    })
    
    
}

shinyApp(ui = ui, server = server)

