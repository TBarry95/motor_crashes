############################################################
# DES: Motor crashes analysis
# BY: Tiernan Barry
############################################################

library("aws.s3")
library("utils")
library("plotly")
#library("naniar")

############################################################
# Get data
############################################################

source("./motor_crashes/aws_access_keys.r")

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
  "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY
)

# S3 bucket
motor_data_obj <- aws.s3::get_object("s3://motor-crashes/Motor_Vehicle_Collisions_-_Crashes.csv") 
motor_data <- rawToChar(motor_data_obj)
motor_data <- textConnection(motor_data)
motor_data <- utils::read.delim(motor_data, sep = ";", stringsAsFactors = FALSE) 
print(head(motor_data))

############################################################
# Clean data
############################################################

motor_data[motor_data == "" | is.null(motor_data)] <- NA
print(head(motor_data))

nrow(motor_data)
length(unique(motor_data$COLLISION_ID))

############################################################
# Explore data:
# - DQ report: plot missing vals
# - Time series of crashes by date
# - Pivot table based on car type
############################################################

# - DQ report: plot missing vals
#naniar::vis_miss(motor_data, warn_large_data=FALSE)
total_rows <- nrow(motor_data)
total_fields_pct <- c()
cols <- c()

for (i in 1:length(names(motor_data))){
  col_df <- motor_data[, names(motor_data)[i]]
  na_col_df <- length(col_df[is.na(col_df)])
  na_col_df2 <- length(col_df[!is.na(col_df)])
  
  total_fields_pct[i] <- (1-round(na_col_df/total_rows,5))*100
  cols[i] <- names(motor_data)[i]
}

dq_report <- data.frame(COLUMNS=cols, POPULATED=total_fields_pct)

dq_report_fig <- plotly::plot_ly(
  
  x = dq_report$COLUMNS,
  y = dq_report$POPULATED,
  name = "Data Quality Report - % Populated",
  type = "bar"
) %>%
  layout(
    title="Data Quality Report - % Populated",
    yaxis = list(title = "PERCENT"),
    xaxis = list(title = "COLUMNS")
  )

saveRDS(dq_report_fig, "/home/rstudio/motor_crashes/Output/dq_report_fig1.rds")


# - Time series of crashes by date
ts_data <- unique(motor_data[, c("CRASH.DATE", "COLLISION_ID")])
ts_data_sum <- data.frame(table(ts_data$CRASH.DATE))
names(ts_data_sum) <- c("DATE", "CRASHES")
ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")

ts_plot_title1 <- paste0("Motor Crashes: ", min(ts_data_sum$DATE), " to ", max(ts_data_sum$DATE))

fig <- plotly::plot_ly(
  data=ts_data_sum,
  x=~DATE,
  y=~CRASHES) %>%
  plotly::add_lines() %>%
  layout(
    title=ts_plot_title1
  ) %>% 
  layout(legend = list(x=.5, y=.5, bgcolor = 'rgba(0,0,0,0)'))

print(fig)

saveRDS(fig, "/home/rstudio/motor_crashes/Output/ts_crashes.rds")

############################################################
# tests:
# - hypt tests: before Pandemic, after pandemic - mean test for crash differences
############################################################

covid_start <- as.Date("03/12/2020", format = "%m/%d/%Y")

# populations:
before_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") < covid_start, ]
after_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") >= covid_start, ]

# samples:
bf_sample1 <- sample(before_covid$CRASHES, size = 50)
aft_sample1 <- sample(after_covid$CRASHES, size = 50)

t.test(bf_sample1, aft_sample1) 





