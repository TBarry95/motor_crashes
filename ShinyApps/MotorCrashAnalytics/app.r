############################################################
# DES: Shiny dashboard
# BY: Tiernan Barry
############################################################

library("shinydashboard")
library("dashboardthemes")
library("utils")
library("plotly")
library("shiny")
library("stats")
library("tseries")
library("forecast")

############################################################
# Define UI
############################################################

DefineUserInterface <- function(){
  
  ui <- shiny::shinyUI(
    
    shinydashboard::dashboardPage(
      
      shinydashboard::dashboardHeader(
        titleWidth = "300",
        title="NY Motor Crash Analytics"
      ), 
      
      ##########################################
      # Side bar:
      ##########################################
      
      shinydashboard::dashboardSidebar(
        width="300",
        
        shinydashboard::sidebarMenu(
          
          shinydashboard::menuItem("Data Quality", tabName="dq1"),
          
          shinydashboard::menuItem("Explore Data", tabName="eda1"), 
          
          shinydashboard::menuItem("Time Series Forecast", tabName="tspred"), 
          
          shinydashboard::menuItem("Crash Hypothesis: COVID", tabName="covid_hyp")
          
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
            
            tabsetPanel(
              tabPanel(
                "Map - Today's crashes",
                fluid = TRUE,
                plotly::plotlyOutput("map1", width = "100%", height = "400px")
              ),
            
              tabPanel(
                "Time Series",
                fluid = TRUE,
                plotly::plotlyOutput("ts_fig1"),
                br(),
                plotly::plotlyOutput("ts_fig2")
              ),
              
              tabPanel(
                "Pivots",
                fluid = TRUE,
                
                column(
                  width=6,
                  rpivotTable::rpivotTableOutput("pivot1", width = "80%")
                )
                
              )
            )
          ),
          
          ##########################################
          # Hyp :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="covid_hyp",
            
            tags$h2("Hypothesis: COVID-19 has caused the number of car crashes to change in New York"),
            tags$h4("H0: There is no difference in crash volumes between before and after COVID-19 (2020-01-10)"),
            tags$h4("H1: There is a difference in crash volumes between before and after COVID-19 (2020-01-10)"),
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
          ),
          
          ##########################################
          # TS Pred :
          ##########################################
          
          shinydashboard::tabItem(
            tabName="tspred",
            
            tabsetPanel(
              tabPanel(
                "Decomposition",
                tags$h2("Time Series Decomposition: Crashes per Day"),
                shiny::plotOutput("ts_decom", width = "100%", height = "600px"),
                shiny::textOutput("adf_test")
              ),
              
              tabPanel(
                 "ARIMA",
                 shiny::plotOutput("ts_arima", width = "100%", height = "600px"),
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
  ts_fig2 <- readRDS("/home/rstudio/motor_crashes/Output/ts_crashes_ratio.rds")
  dq_fig1 <- readRDS("/home/rstudio/motor_crashes/Output/dq_report_fig1.rds")
  map1 <- readRDS("/home/rstudio/motor_crashes/Output/map.rds")
  # hist_fig1 <- readRDS("/home/rstudio/motor_crashes/Output/hist_hyp_before.rds")
  # hist_fig2 <- readRDS("/home/rstudio/motor_crashes/Output/hist_hyp_after.rds")
  
  aft_sample1 <- readRDS("/home/rstudio/motor_crashes/Output/hyp_sample_aft.rds")
  bf_sample1 <- readRDS("/home/rstudio/motor_crashes/Output/hyp_sample_bf.rds")
  
  pivot1 <- readRDS("/home/rstudio/motor_crashes/Output/pivot1.rds")

  ts_data_sum <- readRDS("/home/rstudio/motor_crashes/Output/ts_data_sum.rds")
  
  covid_start <- as.Date("01/10/2020", format = "%m/%d/%Y")
  alpha_stat <- 0.05
  
  #########################################
  # DQ tab
  #########################################
  
  output$dq_fig1 <- plotly::renderPlotly({
    print(dq_fig1)
  })
  
  #########################################
  # Explore tab
  #########################################
  
  output$map1 <- plotly::renderPlotly({
    map1
  })
  
  output$ts_fig1 <- plotly::renderPlotly({
    print(ts_fig1)
  })
  
  output$ts_fig2 <- plotly::renderPlotly({
    print(ts_fig2)
  })
  
  output$pivot1 <- rpivotTable::renderRpivotTable({
    pivot1
  })

  #########################################
  # Hyp test tab
  #########################################
  
  output$hist_fig1 <- plotly::renderPlotly({
    
    # populations:
    before_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") < covid_start, ]

    # samples:
    bf_sample1 <<- sample(before_covid$COUNT, size = 200)

    hist_fig1 <- plot_ly(x = bf_sample1, type = "histogram") %>%
      layout(
        title="Histogram: Motor crash sample before COVID 19",
        yaxis = list(title = "FREQUENCY"),
        xaxis = list(title = "CRASHES")
      )
    print(hist_fig1)
    
  })
  
  output$hist_fig2 <- plotly::renderPlotly({
    
    # populations:
    after_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") >= covid_start, ]
    
    # samples:
    aft_sample1 <<- sample(after_covid$COUNT, size = 200)
    
    hist_fig2 <- plot_ly(x = aft_sample1, type = "histogram")%>%
      layout(
        title="Histogram: Motor crash sample after COVID 19",
        yaxis = list(title = "FREQUENCY"),
        xaxis = list(title = "CRASHES")
      )
    
    print(hist_fig2)
  })
  
  output$shapiro_wilk <- shiny::renderText({
      
    norm_bf <- shapiro.test(bf_sample1)
    norm_aft <- shapiro.test(aft_sample1)
    
    if (norm_bf$p.value < alpha_stat & norm_aft$p.value < alpha_stat){
      paste0("Both samples are from a normal distribution, as per the Shapiro Wilk Test for normality.")
    } else {
      "Both samples did not display a normal distribution, as per the Shapiro Wilk Test for normality. "
    }
    
  })
  
  output$mean_test <- shiny::renderText({
    
    norm_bf <- shapiro.test(bf_sample1)
    norm_aft <- shapiro.test(aft_sample1)
    
    if (norm_bf$p.value < alpha_stat & norm_aft$p.value < alpha_stat){
      res <- t.test(bf_sample1, aft_sample1)
    } else {
      res <- wilcox.test(bf_sample1, aft_sample1)
    }
    
    if (res$p.value < alpha_stat){
      paste0("Reject Null Hypothesis. P Value = ", round(res$p.value, 5))
    } else {
      paste0("Fail to reject Null Hypothesis. P Value = ",  round(res$p.value, 5))
    }
    
  })
  
  #########################################
  # TS Pred:
  #########################################
  
  output$ts_decom <- shiny::renderPlot({
    
    ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")
    ts_data_sum <- ts_data_sum[order(ts_data_sum$DATE, decreasing = F), ]
    ts_data_sum_ts <- ts(ts_data_sum$COUNT, frequency = 365.25)
    dec_ts_data_sum = decompose(ts_data_sum_ts)
    plot(dec_ts_data_sum)
    
  })
  
  output$adf_test <- shiny::renderText({
    
    ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")
    ts_data_sum <- ts_data_sum[order(ts_data_sum$DATE, decreasing = F), ]
    ts_data_sum_ts <- ts(ts_data_sum$COUNT, frequency = 365.25)
    adf_res <- tseries::adf.test(ts_data_sum_ts)
    
    if (adf_res$p.value < alpha_stat){
      paste0("ADF test: Reject Null Hypothesis (data is stationary). P Value = ", round(adf_res$p.value, 5))
    } else {
      paste0("ADF test: Fail to reject Null Hypothesis (data is not stationary). P Value = ",  round(adf_res$p.value, 5))
    }
    
  })
  
  output$ts_arima <- shiny::renderPlot({
    
    ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")
    ts_data_sum <- ts_data_sum[order(ts_data_sum$DATE, decreasing = F), ]
    ts_data_sum_ts <- ts(ts_data_sum$COUNT, frequency = 365.25)
    
    # run auto arima:
    forecast::auto.arima(ts_data_sum_ts, trace=TRUE)
    
    arima_model <- forecast::Arima(ts_data_sum_ts,
                                   order=c(5, 1, 2),
                                   include.drift = F)
    
    preds <- forecast(arima_model, h = 300, level=c(99.6))
    plot(preds)
    
  })
  
  
}


############################################################
# Launch app
############################################################

ui <- DefineUserInterface()

shiny::shinyApp(ui, DefineServer)


