############################################################
# DES: Motor crashes analysis
# BY: Tiernan Barry
############################################################

library("aws.s3")
library("utils")

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

motor_data[motor_data == ""] <- NA





