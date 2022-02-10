############################################################
# DES: Motor crashes analysis
# BY: Tiernan Barry
############################################################

library("aws.s3")
library("utils")
library("plotly")
library("rpivotTable")

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

motor_data$TOTAL_KILLED <- rowSums(motor_data[, c("NUMBER.OF.PERSONS.KILLED", "NUMBER.OF.PEDESTRIANS.KILLED", "NUMBER.OF.CYCLIST.KILLED", "NUMBER.OF.MOTORIST.KILLED" )])
motor_data$TOTAL_INJURED <- rowSums(motor_data[, c("NUMBER.OF.PERSONS.INJURED", "NUMBER.OF.PEDESTRIANS.INJURED", "NUMBER.OF.CYCLIST.INJURED", "NUMBER.OF.MOTORIST.INJURED" )])

ts_data <- unique(motor_data[, c("CRASH.DATE", "COLLISION_ID")])
ts_data_sum <- data.frame(table(ts_data$CRASH.DATE))

names(ts_data_sum) <- c("DATE", "COUNT")
saveRDS(ts_data_sum, "/home/rstudio/motor_crashes/Output/ts_data_sum.rds")

############################################################
# tests:
# - hypt tests: before Pandemic, after pandemic - mean test for crash differences
############################################################

covid_start <- as.Date("01/10/2020", format = "%m/%d/%Y")

# populations:
before_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") < covid_start, ]
after_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") >= covid_start, ]

# samples:
bf_sample1 <- sample(before_covid$COUNT, size = 200)
aft_sample1 <- sample(after_covid$COUNT, size = 200)

hist_fig1 <- plot_ly(x = bf_sample1, type = "histogram") %>%
  layout(
    title="Histogram: Motor crash sample before COVID 19",
    yaxis = list(title = "FREQUENCY"),
    xaxis = list(title = "CRASHES")
  )

hist_fig2 <- plot_ly(x = aft_sample1, type = "histogram")%>%
  layout(
    title="Histogram: Motor crash sample after COVID 19",
    yaxis = list(title = "FREQUENCY"),
    xaxis = list(title = "CRASHES")
  )


norm2 <- shapiro.test(bf_sample1)
norm1 <- shapiro.test(aft_sample1)


alpha <- 0.05

hyp_res <- t.test(bf_sample1, aft_sample1) 

res <- wilcox.test(aft_sample1, bf_sample1)


saveRDS(hist_fig1, "/home/rstudio/motor_crashes/Output/hist_hyp_before.rds")
saveRDS(hist_fig2, "/home/rstudio/motor_crashes/Output/hist_hyp_after.rds")
saveRDS(aft_sample1, "/home/rstudio/motor_crashes/Output/hyp_sample_aft.rds")
saveRDS(bf_sample1, "/home/rstudio/motor_crashes/Output/hyp_sample_bf.rds")








