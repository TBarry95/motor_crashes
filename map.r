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
# Model: Death or no Death:
############################################################

date_map <- "02/01/2022"

df_map <- unique(motor_data[, c("CRASH.DATE", "LATITUDE","LONGITUDE", "BOROUGH")])
df_map <- df_map[!is.na(df_map$BOROUGH) &
                   !is.na(df_map$LATITUDE) &
                   !is.na(df_map$LONGITUDE) & 
                   df_map$CRASH.DATE == date_map,]

g <- list(
  scope = 'NY',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)


fig <- plot_geo(df_map, locationmode = 'USA-states', sizes = c(1, 250))
fig <- fig %>% add_markers(
  x = ~LONGITUDE, 
  y = ~LATITUDE, 
  #size = ~pop, 
  #color = ~q, 
  hoverinfo = "text",
  #text = ~paste(df_map$BOROUGH, "<br />", df$pop/1e6, " million")
)
fig <- fig %>% layout(title = paste0('NYPD Motor Crashes: ', date_map), geo = g)
fig

saveRDS(fig, "/home/rstudio/motor_crashes/Output/map.rds")
