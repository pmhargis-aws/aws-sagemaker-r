# Import R packages
library(fpp)
library(lubridate)
library(RemixAutoML)
library(reticulate)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(grid)

# Bring in library for prediction server
library(plumber)

# Import Sagemaker SDK
sagemaker <- import('sagemaker')

# Setup parameters
# Container directories
prefix <- '/opt/ml'
input_path <- paste(prefix, 'input/data', sep='/')
output_path <- paste(prefix, 'output', sep='/')
model_path <- paste(prefix, 'model', sep='/')

# Channel holding training data
channel_name = 'train/train.csv'
training_path <- paste(input_path, channel_name, sep='/')

# Setup training function
train <- function() {
    # Load & Prep Data
    walmart_store_sales_data = data.table::fread(training_path, header = T, stringsAsFactors = FALSE)
    top_grossing_store = walmart_store_sales_data %>% dplyr::group_by(., Store) %>% dplyr::summarize(., Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))
    max_sales = max(top_grossing_store$Weekly_Sales)
    top_grossing_store = top_grossing_store %>% dplyr::filter(., Weekly_Sales == max_sales)
    top_grossing_store = top_grossing_store$Store %>% as.numeric(.)
    top_store_weekly_sales <- walmart_store_sales_data[Store == eval(top_grossing_store),
    .(Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE)),
    by = "Date"]
    # Forecast
    weekly_forecast = RemixAutoML::AutoTS(
    data = top_store_weekly_sales %>% mutate(Date = ymd(Date)),
    TargetName = "Weekly_Sales",
    DateName = "Date",
    FCPeriods = 16,
    HoldOutPeriods = 12,
    TimeUnit = "week"
    )
    print(paste(weekly_forecast))

    saveRDS(weekly_forecast, file=paste(model_path, 'model.rds', sep='/'))


    write('success', file=paste(output_path, 'success', sep='/'))
}

# Setup scoring function
serve <- function() {
    app <- plumb(paste(prefix, 'plumber.R', sep='/'))
    app$run(host='0.0.0.0', port=8080)}


# Run at start-up
args <- commandArgs()
if (any(grepl('train', args))) {
    train()}
if (any(grepl('serve', args))) {
    serve()}
    
 





