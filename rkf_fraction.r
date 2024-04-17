# # load("/Users/kang/intradayModel/data/volume_aapl.rda")
# library(intradayModel)
# data(volume_aapl)
# volume_aapl_training <- volume_aapl[, 1:10]
# # volume_aapl_testing <- volume_aapl[, 11:12]
# volume_aapl_testing1 <- as.matrix(volume_aapl[, 11])
# volume_aapl_testing2 <- as.matrix(volume_aapl[, 11, drop = FALSE])
# volume_aapl_testing3 <- volume_aapl[, 11:12]
# model_fit <- fit_volume(volume_aapl_training)
# # analysis_result <- decompose_volume(purpose = "analysis", model_fit, volume_aapl_training)
# forecast_result <- forecast_volume(model_fit, volume_aapl_testing2)


# load("/Users/kang/intradayModel/data/volume_aapl.rda")
# home_path <- path.expand("~")
home_path <- "/homes/80/kang"
dir_path <- paste0(home_path,'/cmem/data_fraction/02.2_data_r_input_kf_fraction/')
# dir_path <- paste0(home_path,'/cmem/data_fraction/02.2_data_r_input_kf/')
# dir_path <- paste0(home_path,'/cmem/data_minmax/02.2_data_r_input_kf_minmax/')
# dir_path <- paste0(home_path,'/cmem/data_notional/02.2_data_r_input_kf/')
# dir_path <- paste0(home_path,'/cmem/data_notional/02.2_data_r_input_k_remained/')
# dir_path <- paste0(home_path,'/cmem/data/02.2_data_r_input_k_remained/')
# dir_path <- '/home/kanli/cmem/data/02.2_data_r_input_kf/'
# dir_path <- '/Users/kang/CMEM/data/02.2_data_r_input_kf/'
# dir_path <- "/home/kanli/cmem/data/02_r_input/"
# dir_path <- "/Users/kang/CMEM/data/02_r_input/"
# dir_path <- "/Users/kang/CMEM/data/02_r_input_10/"
file_names <- list.files(dir_path)


source(paste0(home_path,"/intradayModel/R/use_model.R"))
source(paste0(home_path,"/intradayModel/R/fit.R"))
source(paste0(home_path,"/intradayModel/R/auxiliary_kalman.R"))
source(paste0(home_path,"/intradayModel/R/auxiliary_tools.R"))

# source("/Users/kang/intradayModel/R/use_model.R")
# source("/Users/kang/intradayModel/R/fit.R")
# source("/Users/kang/intradayModel/R/auxiliary_kalman.R")
# source("/Users/kang/intradayModel/R/auxiliary_tools.R")





# Install the 'xts' package if it's not already installed
.libPaths("/homes/80/kang/my_R_lib")
# library(zoo)
# install.packages('lattice', lib='/homes/80/kang/my_R_lib', repos='http://cran.rstudio.com/')
# library(lattice)
# install.packages('zoo', lib='/homes/80/kang/my_R_lib', repos='http://cran.rstudio.com/')
# library(zoo)
# install.packages('xts', lib='/homes/80/kang/my_R_lib', repos='http://cran.rstudio.com/')
# Now you can load the 'xts' package
library(xts)






process_and_write_data <- function(i)
{


  # Read the CSV file into a data frame in R
  cat("++++++++++++++++++++ i is :", i, "\n")
  # filein1 <- paste0(dir_path, "CNC.txt")

  # Assuming 'home_path' and 'file_names' are defined elsewhere in your script
  out_dir_path <- paste0(home_path, '/cmem/output/0400_r_kl_output_raw_data_fractional/')
  
  # Construct the full path to the file you're checking
  file_path <- paste0(out_dir_path, file_names[i])
  
  # Check if the file exists
  if (file.exists(file_path)) {
    cat("File exists, skipping: ", file_path, "\n")
    return() # Skip this iteration
  } 
  else{

    filein1 <- paste0(dir_path, file_names[i])
    data_for_r <- read.csv(filein1)

    # Set the first column as row names (index)
    row_names <- data_for_r[, 1]
    rownames(data_for_r) <- row_names

    # Convert the columns (except the first) to numeric
    data_for_r[, -1] <- sapply(data_for_r[, -1], as.numeric)

    # Remove the first column (index) from the data frame
    data_for_r <- data_for_r[, -1]

    # Convert the data frame to a matrix
    data_matrix <- as.matrix(data_for_r)

    # Print the matrix
    # print(data_matrix)


    volume_aapl <- data_matrix


    # traing_len <- 26
    # traing_len <- 10*26
    traing_len <- 10
    test_len_with_one_dummy <- 2



    # date_index <- 1

    lst <- list()
    max_len <- ncol(volume_aapl)+1-traing_len-test_len_with_one_dummy
    for (date_index in 1:max_len){
      print(date_index)
      volume_aapl_training <- volume_aapl[, date_index : (date_index+traing_len-1)]
      # volume_aapl_testing1 <- as.matrix(volume_aapl[, 11])
      # volume_aapl_testing2 <- as.matrix(volume_aapl[, 11, drop = FALSE])
      volume_aapl_testing3 <- volume_aapl[, (date_index+traing_len):(date_index+traing_len+test_len_with_one_dummy-1)]

      model_fit <- fit_volume(volume_aapl_training)
      forecast_result <- forecast_volume(model_fit, volume_aapl_testing3)
      # forecast_result
      # forecast_result[1:26]





      # Truncate the items with 52 elements to have 26 elements
      truncate_to_26 <- function(data) {
        if (length(data) == 52) {
          return(data[1:26])
        } else {
          return(data)
        }
      }

      # Truncate original_signal
      forecast_result$original_signal <- truncate_to_26(forecast_result$original_signal)

      # Truncate forecast_signal
      forecast_result$forecast_signal <- truncate_to_26(forecast_result$forecast_signal)

      # Truncate forecast_components$daily
      forecast_result$forecast_components$daily <- truncate_to_26(forecast_result$forecast_components$daily)

      # Truncate forecast_components$dynamic
      forecast_result$forecast_components$dynamic <- truncate_to_26(forecast_result$forecast_components$dynamic)

      # Truncate forecast_components$seasonal
      forecast_result$forecast_components$seasonal <- truncate_to_26(forecast_result$forecast_components$seasonal)

      # Truncate forecast_components$residual
      forecast_result$forecast_components$residual <- truncate_to_26(forecast_result$forecast_components$residual)

      # Make sure to update the error$mae and error$mape since they are related to the length of the signals
      forecast_result$error$mae <- forecast_result$error$mae / 2
      forecast_result$error$mape <- forecast_result$error$mape / 2
      forecast_result$error$rmse <- forecast_result$error$rmse / 2




      # # Calculate R-squared
      # r2 <- cor(forecast_result$original_signal, forecast_result$forecast_signal)^2
      # r2

      date <- colnames(volume_aapl_testing3)[1]
      original_signal <- forecast_result$original_signal
      forecast_signal <- forecast_result$forecast_signal
      daily <- forecast_result$forecast_components$daily
      seasonal<-forecast_result$forecast_components$seasonal
      dynamic<-forecast_result$forecast_components$dynamic
      # residual<-forecast_components$residual
      # Creating the data frame

      df <- data.frame(
        original = original_signal,
        forecast_signal = forecast_signal,
        daily = daily,
        seasonal = seasonal,
        dynamic = dynamic
      )
      # Adding a column for the bins from 1 to 26
      df$bins <- 1:26
      df$date <- date
      df$r2   <- cor(original_signal, forecast_signal)^2
      # Printing the data frame
      # print(df)
      lst[[date_index]] <- df

    }

    dff <- do.call(rbind, lst)
    # out_dir_path <- paste0(home_path,'/cmem/output/0400_r_kl_output_raw_data_minmax/')
    out_dir_path <- paste0(home_path,'/cmem/output/0400_r_kl_output_raw_data_fractional/')
    # out_dir_path <- paste0(home_path,'/cmem/output/0400_r_kl_output_raw_data_notional/')
    # out_dir_path <- '/Users/kang/CMEM/r_output/0400_r_kl_output_raw_data/'
    filename <- paste0(out_dir_path,file_names[i])
    write.csv(dff, file = filename, row.names = FALSE)
  }

}




# for (i in 1:25) {
#   process_and_write_data(i)
# } # fn01

# for (i in 26:50) {
#   process_and_write_data(i)
# } # fn02

# for (i in 51:75) {
#   process_and_write_data(i)
# } # fn03

# for (i in 76:100) {
#   process_and_write_data(i)
# } # fn04


# for (i in 101:125) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 106:110) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason

# for (i in 110:110) {
#   process_and_write_data(i)
# } # not finished yet # done


# for (i in 111:115) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 116:120) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 121:125) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# Vector of specific numbers to iterate over
# numbers <- c(174, 196, 199)
# for (i in numbers) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason

# for (i in 100:250) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason

# for (i in 250:375) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason

# for (i in 375:500) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# only finish the 375-384

# for (i in 385:385) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 386:400) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 400:425) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason
# for (i in 425:450) {
#   process_and_write_data(i)
# } # fn01 12feb for vwap testing reason

# for (i in 301:400) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 401:500) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 201:300) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 101:200) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 1:100) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction

# for (i in 351:400) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 451:500) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 251:300) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 151:200) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction
# for (i in 51:100) {
#   process_and_write_data(i)
# } # fn01 17feb for fraction

for (i in 1:500) {
  process_and_write_data(i)
} # fn01 17feb used for check if all is done


# library(parallel)
# # Define your function here
# process_and_write_data <- function(i) {
#   # Your function's code
# }
# # Run in parallel
# results <- mclapply(51:100, process_and_write_data, mc.cores = 10)

