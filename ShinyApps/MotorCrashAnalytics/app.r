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
          # Explore :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="dq1",
            plotly::plotlyOutput("dq_fig1")
            
          ),
          
          shinydashboard::tabItem(
            tabName="eda1",
            plotly::plotlyOutput("ts_fig1")
            
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
  
  
  
  
  
}


############################################################
# Launch app
############################################################

ui <- DefineUserInterface()

shiny::shinyApp(ui, DefineServer)


