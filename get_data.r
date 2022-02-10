############################################################
# DES: Motor crashes analysis
# BY: Tiernan Barry
############################################################

library("aws.s3")
library("utils")
library("plotly")
#library("naniar")
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

############################################################
# Explore data:
# - DQ report: plot missing vals
# - Time series of crashes by date
# - Pivot table based on car type
# - Correlation: numrics:
############################################################

#######################################
# - DQ report: plot missing vals
#######################################

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

#######################################
# - Time series of crashes by date
#######################################

ts_data <- unique(motor_data[, c("CRASH.DATE", "COLLISION_ID")])
ts_data_sum <- data.frame(table(ts_data$CRASH.DATE))
names(ts_data_sum) <- c("DATE", "COUNT")
ts_data_sum$CLASS <- "CRASHES"
saveRDS(ts_data_sum, "/home/rstudio/motor_crashes/Output/ts_data_sum.rds")

ts_data2 <- unique(motor_data[, c("CRASH.DATE", "TOTAL_KILLED")])
ts_data_sum2 <- data.frame(table(ts_data2$CRASH.DATE))
names(ts_data_sum2) <- c("DATE", "COUNT")
ts_data_sum2$CLASS <- "KILLED"
saveRDS(ts_data_sum2, "/home/rstudio/motor_crashes/Output/ts_data_sum2.rds")

ts_data3 <- unique(motor_data[, c("CRASH.DATE", "TOTAL_INJURED")])
ts_data_sum3 <- data.frame(table(ts_data3$CRASH.DATE))
names(ts_data_sum3) <- c("DATE", "COUNT")
ts_data_sum3$CLASS <- "INJURED"
saveRDS(ts_data_sum3, "/home/rstudio/motor_crashes/Output/ts_data_sum3.rds")


names(ts_data_sum) <- c("DATE", "COUNT", "CLASS")
names(ts_data_sum2) <- c("DATE", "COUNT", "CLASS")
names(ts_data_sum3) <- c("DATE", "COUNT", "CLASS")

ts_data_sum <- rbind(ts_data_sum,ts_data_sum2,ts_data_sum3)

ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")

ts_plot_title1 <- paste0("Motor Crashes, Deaths, Injuries: ", min(ts_data_sum$DATE), " to ", max(ts_data_sum$DATE))

fig <- plotly::plot_ly(
  data=ts_data_sum,
  x=~DATE,
  y=~COUNT,
  color=~CLASS) %>%
  plotly::add_lines() %>%
  layout(
    title=ts_plot_title1
  ) %>% 
  layout(legend = list(x=.9, y=.9, bgcolor = 'rgba(0,0,0,0)'))

print(fig)

saveRDS(fig, "/home/rstudio/motor_crashes/Output/ts_crashes.rds")

#######################################
# - Time series LOG transformed of crashes by date
#######################################

ts_data_sumdiff <- ts_data_sum
ts_data_sumdiff$DATE <- as.Date(ts_data_sumdiff$DATE, format = "%m/%d/%Y")
ts_data_sumdiff <- ts_data_sumdiff[order(as.Date(ts_data_sumdiff$DATE), decreasing = TRUE), ]
ts_data_sumdiff$COUNTLAG <- c(ts_data_sumdiff$COUNT[2:nrow(ts_data_sumdiff)], 0)
ts_data_sumdiff$PCT_CHG <- ((ts_data_sumdiff$COUNT-ts_data_sumdiff$COUNTLAG)/ts_data_sumdiff$COUNTLAG)*100

ts_data_sum2diff <- ts_data_sum2 
ts_data_sum2diff$DATE <- as.Date(ts_data_sum2diff$DATE, format = "%m/%d/%Y")
ts_data_sum2diff <- ts_data_sum2diff[order(as.Date(ts_data_sum2diff$DATE), decreasing = TRUE), ]
ts_data_sum2diff$COUNTLAG <- c(ts_data_sum2diff$COUNT[2:nrow(ts_data_sum2diff)], 0)
ts_data_sum2diff$PCT_CHG <- ((ts_data_sum2diff$COUNT-ts_data_sum2diff$COUNTLAG)/ts_data_sum2diff$COUNTLAG)*100

ts_data_sum3diff <- ts_data_sum3
ts_data_sum3diff$DATE <- as.Date(ts_data_sum3diff$DATE, format = "%m/%d/%Y")
ts_data_sum3diff <- ts_data_sum3diff[order(as.Date(ts_data_sum3diff$DATE), decreasing = TRUE), ]
ts_data_sum3diff$COUNTLAG <- c(ts_data_sum3diff$COUNT[2:nrow(ts_data_sum3diff)], 0)
ts_data_sum3diff$PCT_CHG <- ((ts_data_sum3diff$COUNT-ts_data_sum3diff$COUNTLAG)/ts_data_sum3diff$COUNTLAG)*100

ts_data_sumdiff <- rbind(ts_data_sumdiff,ts_data_sum2diff,ts_data_sum3diff)

ts_data_sumdiff$DATE <- as.Date(ts_data_sumdiff$DATE, format = "%m/%d/%Y")

ts_plot_title1 <- paste0("Motor Crashes, Deaths, Injuries (% Change): ", min(ts_data_sumdiff$DATE), " to ", max(ts_data_sumdiff$DATE))

fig <- plotly::plot_ly(
  data=ts_data_sumdiff,
  x=~DATE,
  y=~PCT_CHG,
  color=~CLASS) %>%
  plotly::add_lines() %>%
  layout(
    title=ts_plot_title1
  ) %>% 
  layout(legend = list(x=.9, y=.9, bgcolor = 'rgba(0,0,0,0)'))

print(fig)

saveRDS(fig, "/home/rstudio/motor_crashes/Output/ts_crashes_diff.rds")

#######################################
# - time series: Injury rate / Death rate over time:
#######################################

ts_data <- unique(motor_data[, c("CRASH.DATE", "COLLISION_ID")])
ts_data_sum <- data.frame(table(ts_data$CRASH.DATE))
names(ts_data_sum) <- c("CRASH.DATE", "CRASHES")

ts_data2 <- unique(motor_data[, c("CRASH.DATE", "TOTAL_KILLED")])
ts_data_sum2 <- data.frame(table(ts_data2$CRASH.DATE))
names(ts_data_sum2) <- c("CRASH.DATE", "KILLED")

ts_data3 <- unique(motor_data[, c("CRASH.DATE", "TOTAL_INJURED")])
ts_data_sum3 <- data.frame(table(ts_data3$CRASH.DATE))
names(ts_data_sum3) <- c("CRASH.DATE", "INJURED")

data_ts <- merge(ts_data_sum, ts_data_sum2, by="CRASH.DATE")
data_ts <- merge(data_ts, ts_data_sum3, by="CRASH.DATE")

data_ts$PCT_INJURED <- (data_ts$INJURED/data_ts$CRASHES)*100
data_ts$PCT_KILLED <- (data_ts$KILLED/data_ts$CRASHES)*100

data_ts_pctkill <- unique(data_ts[, c("CRASH.DATE", "PCT_KILLED")])
data_ts_pctkill$CLASS <- "PCT_KILLED"

data_ts_pctinj <- unique(data_ts[, c("CRASH.DATE", "PCT_INJURED")])
data_ts_pctinj$CLASS <- "PCT_INJURED"

names(data_ts_pctinj) <- c("CRASH.DATE", "COUNT", "CLASS")
names(data_ts_pctkill) <- c("CRASH.DATE", "COUNT", "CLASS")

ts_data_pct <- rbind(data_ts_pctinj, data_ts_pctkill)

ts_data_pct$CRASH.DATE <- as.Date(ts_data_pct$CRASH.DATE, format = "%m/%d/%Y")

ts_plot_title1 <- paste0("Motor Crashes, Deaths, Injuries (Ratio): ", min(ts_data_pct$CRASH.DATE), " to ", max(ts_data_pct$CRASH.DATE))

fig <- plotly::plot_ly(
  data=ts_data_pct,
  x=~CRASH.DATE,
  y=~COUNT,
  color=~CLASS) %>%
  plotly::add_lines() %>%
  layout(
    title=ts_plot_title1
  ) %>% 
  layout(legend = list(x=.9, y=.9, bgcolor = 'rgba(0,0,0,0)'))

print(fig)

saveRDS(fig, "/home/rstudio/motor_crashes/Output/ts_crashes_ratio.rds")


#######################################
# - Pivot table based on car type
#######################################

pivot_data1 <- motor_data[, c("VEHICLE.TYPE.CODE.1", "BOROUGH")]
pivot_data1_sum <- data.frame(table(pivot_data1$VEHICLE.TYPE.CODE.1))
pivot_data1_sum1 <- pivot_data1_sum[pivot_data1_sum$Freq < 200,]
pivot_data1$VEHICLE.TYPE.CODE.1[pivot_data1$VEHICLE.TYPE.CODE.1 %in% pivot_data1_sum1$Var1 | 
                                  is.null(pivot_data1$VEHICLE.TYPE.CODE.1)] <- "OTHER"

pivot1 <- rpivotTable(pivot_data1, 
            rows = c("VEHICLE.TYPE.CODE.1", "BOROUGH"))
saveRDS(pivot1, "/home/rstudio/motor_crashes/Output/pivot1.rds")

pivot2 <- rpivotTable(pivot_data1, 
                      rows = c("BOROUGH"))
saveRDS(pivot2, "/home/rstudio/motor_crashes/Output/pivot2.rds")

#######################################
# - Correlation: numrics:
#######################################




############################################################
# tests:
# - hypt tests: before Pandemic, after pandemic - mean test for crash differences
############################################################

covid_start <- as.Date("01/10/2020", format = "%m/%d/%Y")

# populations:
before_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") < covid_start, ]
after_covid <- ts_data_sum[as.Date(ts_data_sum$DATE, format = "%m/%d/%Y") >= covid_start, ]

# samples:
bf_sample1 <- sample(before_covid$CRASHES, size = 200)
aft_sample1 <- sample(after_covid$CRASHES, size = 200)

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

hyp_res <- t.test(bf_sample1, aft_sample1) 

res <- wilcox.test(aft_sample1, bf_sample1, alternative = "less")


saveRDS(hist_fig1, "/home/rstudio/motor_crashes/Output/hist_hyp_before.rds")
saveRDS(hist_fig2, "/home/rstudio/motor_crashes/Output/hist_hyp_after.rds")
saveRDS(aft_sample1, "/home/rstudio/motor_crashes/Output/hyp_sample_aft.rds")
saveRDS(bf_sample1, "/home/rstudio/motor_crashes/Output/hyp_sample_bf.rds")






