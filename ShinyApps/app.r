############################################################
# DES: Shiny dashboard
# BY: Tiernan Barry
############################################################

library("shinydashboard")
library("dashboardthemes")
library("utils")
#library("plotly")
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
          
          shinydashboard::menuItem("Explore Data"),
          shinydashboard::menuSubItem("Results", tabName="explore1")
          
        )
        
      ),
      
      shinydashboard::dashboardBody(
        
        dashboardthemes::shinyDashboardThemes(
          theme="grey_light"
        ),
        
        shinydashboard::tabItems(
          
          ##########################################
          # Explore Individual crypto:
          ##########################################
          
          shinydashboard::tabItem(
            tabName="explore1"
            
           
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
  
}


############################################################
# Launch app
############################################################

ui <- DefineUserInterface()

shiny::shinyApp(ui, DefineServer)


