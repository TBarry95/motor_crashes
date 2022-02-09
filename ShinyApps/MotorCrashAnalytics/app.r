############################################################
# DES: Shiny dashboard
# BY: Tiernan Barry
############################################################

library("shinydashboard")
library("dashboardthemes")
library("utils")
library("plotly")
#library("DT")

############################################################
# Define UI
############################################################

DefineUserInterface <- function(){
  
  ui <- shiny::shinyUI(
    
    shinydashboard::dashboardPage(
      
      shinydashboard::dashboardHeader(
        title="Motor Crashes Analytics"
      ), 
      
      ##########################################
      # Side bar:
      ##########################################
      
      shinydashboard::dashboardSidebar(
        width="250",
        
        shinydashboard::sidebarMenu(
          
          shinydashboard::menuItem("Data Quality", tabName="dq1"),
          
          shinydashboard::menuItem("Explore Data", tabName="eda1"), 
          
          shinydashboard::menuItem("Crash Hypothesis: COVID", tabName="covid_hyp")
          
          #shinydashboard::menuSubItem("Results", tabName="explore1")
          
        )
        
      ),
      
      shinydashboard::dashboardBody(
        
        dashboardthemes::shinyDashboardThemes(
          theme="poor_mans_flatly"
        ),
        
        shinydashboard::tabItems(
          
          ##########################################
          # DQ :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="dq1",
            plotly::plotlyOutput("dq_fig1")
          ),
          
          
          ##########################################
          # Explore :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="eda1",
            plotly::plotlyOutput("ts_fig1")
            
            # shiny::mainPanel(
            #   shiny::tabsetPanel(
            #     shiny::tabPanel(
            #       tabName="Crash Time Series",
            #       plotly::plotlyOutput("ts_fig1")
            #     ),
            #     shiny::tabPanel(
            #       tabName="Crash Map",
            #       plotly::plotlyOutput("")
            #     )
            # 
            #   )
            # )
            
          ),
          
          ##########################################
          # Hyp :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="covid_hyp",
            
            tags$h2("Hypothesis: COVID-19 has caused the number of car crashes to fall in New York"),
            tags$h4("H0: There is no difference in crash volumes between before and after COVID-19"),
            tags$h4("H1: There is a difference in crash volumes between before and after COVID-19"),
            br(),
            
            column(
              width=6,
              plotly::plotlyOutput("hist_fig1")
            ), 
            column(
              width=6,
              plotly::plotlyOutput("hist_fig2")
            ),
            fluidRow(
              column(
                width=12,
                shiny::textOutput("shapiro_wilk"),
                shiny::textOutput("mean_test")
              )
            )
          )
        )
      )
    )
  )
  return(ui)
  
}


############################################################
# Define Server
############################################################

DefineServer <- function(input, output){
  
  #########################################
  # Get data
  #########################################
  
  ts_fig1 <- readRDS("/home/rstudio/motor_crashes/Output/ts_crashes.rds")
  dq_fig1 <- readRDS("/home/rstudio/motor_crashes/Output/dq_report_fig1.rds")
  
  hist_fig1 <- readRDS("/home/rstudio/motor_crashes/Output/hist_hyp_before.rds")
  hist_fig2 <- readRDS("/home/rstudio/motor_crashes/Output/hist_hyp_after.rds")
  
  aft_sample1 <- readRDS("/home/rstudio/motor_crashes/Output/hyp_sample_aft.rds")
  bf_sample1 <- readRDS("/home/rstudio/motor_crashes/Output/hyp_sample_bf.rds")
  

  #########################################
  # DQ tab
  #########################################
  
  output$dq_fig1 <- plotly::renderPlotly({
    print(dq_fig1)
  })
  
  #########################################
  # Explore tab
  #########################################
  
  output$ts_fig1 <- plotly::renderPlotly({
    print(ts_fig1)
  })
  
  #########################################
  # Hyp test tab
  #########################################
  
  output$hist_fig1 <- plotly::renderPlotly({
    print(hist_fig1)
  })
  
  output$hist_fig2 <- plotly::renderPlotly({
    print(hist_fig2)
  })
  
  output$shapiro_wilk <- shiny::renderText({
    "Both samples did not display a normal distribution, as per the Shapiro Wilk Test for normality. "
  })
  
  output$mean_test <- shiny::renderText({
    "Using the Wilcoxon Rank Sum test, the Null Hypothesis was Rejected (P-Value = 0)"
  })
  
}


############################################################
# Launch app
############################################################

ui <- DefineUserInterface()

shiny::shinyApp(ui, DefineServer)


