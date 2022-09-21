# This section imports libraries which are useful for data and will be used in this analysis
library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(data.table)

# This pulls the .xlsx data into a data.frame, which is more friendly to R
raw_income_data_2000_2005 <- read_excel("US Bureau of Economic Analysis/cu-all-multi-year-2000-2005.xlsx", 
                                        range = "A3:G173")
raw_income_data_2006_2012 <- read_excel("US Bureau of Economic Analysis/cu-all-multi-year-2006-2012.xlsx", 
                                        range = "A3:H173")
raw_income_data_2013_2018 <- read_excel("US Bureau of Economic Analysis/cu-all-multi-year-2013-2020.xlsx", 
                                        range = "A3:I173")

# This section cleans the raw data frames so they can be merged
raw_2000_2005 <- raw_income_data_2000_2005[c(1:4, 6:10, 12:19, 25, 111, 113) , ]
raw_2006_2012 <- raw_income_data_2006_2012[c(1, 5, 6, 8, 11:15, 20:21, 24:27, 30:31, 45, 138, 140) , 2:8]
raw_2013_2018 <- raw_income_data_2013_2018[c(1, 5, 6, 8, 11:15, 20:21, 24:27, 30:31, 45, 138, 140) , 2:7]
colnames(raw_2013_2018) <- c('2013', '2014', '2015', '2016', '2017', '2018')
# This section merges the raw data frames into an intermediary data frame and 
# finally a complete aggregated data frame
aggregated_income_data_2000_2012 <- bind_cols(raw_2000_2005 , raw_2006_2012)
  # merge( x = raw_2000_2005 , y = raw_2006_2012 , all = TRUE)
aggregated_income_data_2000_2018 <- bind_cols(aggregated_income_data_2000_2012, raw_2013_2018)
fixed_income_data_2000_2018 <- mutate_at(aggregated_income_data_2000_2018, function(x) as.numeric(x), .vars = 2:20 )
# merge( x = aggregated_income_data_2000_2012 , y = raw_2013_2020 , all = TRUE)
# This imports some hard-to-find data about computational price performance
# Although CPUs are typically used as an example for this, 
# GPUs were decided to be representative of larger market potential,
# as their specialized architecture is more reflective of the type of
# performance other specialized processors might get, such as AI-tailored
# solutions.
median_group_raw_data <- read_csv("Kaggle Data/Median Group Raw Data.csv")

#This imports the GFLOPs (billions of floating point operations per second)
# as a unit rate of performance/dollar by dividing the GFLOPs by the
# listed price to get GFLOPS/$, and multiplying that by 1000 to get
# MFLOPs/$, which is millions of operations per dollar. 
adjusted_mflops_per_dollar <- (median_group_raw_data$GFLOPS / median_group_raw_data$`Release Price (USD)`)*1000
filtered_year_released <- lubridate::mdy(median_group_raw_data$Date)
afj_mflop_fltrd_yr_df <- data.frame( year = lubridate::year(filtered_year_released) , MFLOPSpUSD = adjusted_mflops_per_dollar)
averaging_dataframe <- bind_cols(median_group_raw_data, afj_mflop_fltrd_yr_df)

agg_avg_mflop_yr_qntzd <- aggregate(afj_mflop_fltrd_yr_df$MFLOPSpUSD , by=list(name=afj_mflop_fltrd_yr_df$year), data=afj_mflop_fltrd_yr_df , FUN = mean)

wide_agg_avg <- spread(
                      agg_avg_mflop_yr_qntzd , 
                      key = name , 
                      value = x
                      )
wide_agg_avg <- add_column( wide_agg_avg, '2002' = wide_agg_avg$'2001', .after = "2001" )
wide_agg_avg <- add_column( wide_agg_avg, '2005' = wide_agg_avg$'2004', .after = "2004" )
wide_agg_avg <- add_column( wide_agg_avg, 'Item' = 'MFLOPS/$' , .before = '2000' )
final_agg_data <- rbind( wide_agg_avg , fixed_income_data_2000_2018)
# Converting wide data to long data so that arithmetic can be performed on vectors
#long_pretty_data <- pivot_longer(
#                          final_agg_data,
#                          3:20,
#                          names_to = final_agg_data$Item,
#                          names_sep = '.value'
#                          )
long_pretty_data <- data.frame(
                               t(final_agg_data[-1])
                               )
colnames(long_pretty_data) <- final_agg_data[, 1]
reasonable_expenditure <- c(long_pretty_data$`Audio and visual equipment and services 2/` + long_pretty_data$`Other entertainment supplies, equipment, and services`)
MFLOPsPPP <- c(long_pretty_data$`MFLOPS/$` * reasonable_expenditure)
long_pretty_data$MFLOPsPPP <- MFLOPsPPP
long_pretty_data$Resnbl_Exptr <- reasonable_expenditure
long_pretty_data$Year <- c(2000:2018)

ggplot(long_pretty_data, aes(x=Year, y=MFLOPsPPP)) +
       geom_point(colour = 'red')+
       xlim(2000,2018)+
       geom_smooth(data = long_pretty_data, 
                 aes(y = MFLOPsPPP),
                 colour = 'blue',
                 size = 0.7,
                 method = 'loess'
                 )


