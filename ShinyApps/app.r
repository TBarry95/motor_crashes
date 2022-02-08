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
          
          shinydashboard::menuItem("Explore Data", tabName="eda1")
          
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
            tabName="explore1",
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


