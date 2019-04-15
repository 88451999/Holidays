library(lubridate)
library(tidyverse)
library(httr)
library(jsonlite)

# Function to create a data frame of all weeks in the year
# Parameters :
#   Year          : Year to be generated
#   Start_of_week : which day of the week is to be the start of the week
#                   Day is 1-7 where 1 is Sunday to 7 is Saturday
#                   Default is ISO-8601 standard of 2 (Monday)
# Following ISO-8601 standards, the 1st week of the year will contain
# 4th Jan and then work back to the start of the year.  Then each week number
# is based off that.
Dates <- function (year_num = 2019, start_of_week = 1) {

  # Week 1 of the year will contain 4th Jan so work out the week that has this
  fourth_first  <- dmy(paste('04/01', year_num))
  # Week 1 of next year will contain 4th jan, so use this to work out the end of this year
  fourth_next_year <- dmy(paste('04/01/', year_num+1))
  Start_of_year <- fourth_first - (ifelse(wday(fourth_first) >= start_of_week, wday(fourth_first) - start_of_week, 
                               (wday(fourth_first)+1))) 
  End_of_year <- fourth_next_year - (ifelse(wday(fourth_next_year) >= start_of_week, wday(fourth_next_year) - start_of_week, 
                               (wday(fourth_next_year)+1))) - 1
  
  day_df <- data.frame(date_id = 1)
  year_df <- data.frame()
  day_date <- Start_of_year
  # For loop converts date to number so need to use While..
  # while we are not at the end of the year, set up a time observation
  while (day_date <= End_of_year) {

    # give each date a unique id (1980-01-01 becomes a numeric 19800101)
    day_df$date_id <- year(day_date)*10000 + month(day_date)*100 + day(day_date)
    day_df$calendar_date <- day_date
    day_df$year_num <- year(day_date)
    day_df$month_num <- month(day_date)
    day_df$day_num <- day(day_date)
    day_df$month_num <- quarter(day_date)
    # Week num is the integer division of the number of days this year and 7 and then add 1
    # remember that when the day date is the start of the year then the minus will be 0 so the first 
    # week contains 0 to 6..
    day_df$week_num <- as.integer(day_date - Start_of_year) %/% 7 + 1
    # The year for the week num may be different to the actual year the date is in
    # eg, week 1 may contain dates from the previous year..
    day_df$week_num_year <- year_num
    day_df$year_week_num <- paste(day_df$week_num_year, '-', str_pad(day_df$week_num, 2, pad = "0"), sep="")

    day_df$day_name <- weekdays(day_date)
    day_df$day_name_abbrev <- weekdays(day_date, TRUE)
    day_df$day_number <- wday(day_date)
    day_df$weekend_ind <- ifelse(wday(day_date) %in% c(1, 7), TRUE, FALSE)
    # Set Seasons based on month number
    day_df$season <- case_when(
      day_df$month_num %in% c(12, 1, 2) ~ "Winter",
      day_df$month_num %in% c(3, 4, 5) ~ "Spring",
      day_df$month_num %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    )
    
    year_df <- rbind(year_df, day_df)
    day_date <- day_date + 1
  }
  
  year_df
}
  
# Loop through from 1980 to 2019 building each year and adding to the full data frame
time_dimension <- data_frame()
for (i in 1980:2019) {
  time_dimension <- rbind(time_dimension, Dates(i, 6))
}

# read list of holidays for US for 1980 t0 2019
holidays <- read.csv("holidays_US.csv", stringsAsFactors=FALSE)
holidays$date <- ymd(holidays$date)

# Cut down holidays to just national holidays, Easter, Valentines and Halloween
Other_days <- c("Valentine's Day", "Good Friday", "Easter Sunday",
                "Easter Monday", "Halloween")
holidays <- holidays %>% filter(grepl('National holiday', type) | name %in% Other_days )

# rename columns for final data frame
names(holidays)[names(holidays) == "name"] <- "event_name"
names(holidays)[names(holidays) == "type"] <- "event_type"

# Get Oscar dates
oscars <- read_csv("Oscars.csv")
oscars$date <- as.Date(oscars$date ,format='%B %d, %Y')
oscars$event_oscars <- "Oscars"

# Combine all to work out 
time_dimension <- time_dimension %>% 
 left_join(holidays %>% select(date, event_name, event_type), c("calendar_date" = "date") )  %>%
  left_join(., oscars, c("calendar_date" = "date"))

# If it is an Oscars day, set event name to that otherwise set to event name from holidays
time_dimension$event_name <- ifelse(!is.na(time_dimension$event_oscars), time_dimension$event_oscars, time_dimension$event_name )
time_dimension$event_type <- ifelse(!is.na(time_dimension$event_oscars), time_dimension$event_oscars, time_dimension$event_type )
# If the event is a natinal holiday set the holiday indicator..
time_dimension$holiday_ind <- ifelse(grepl('National holiday', time_dimension$event_type) , TRUE, FALSE)

# Any missing values convert to FALSE
time_dimension$holiday_ind <- ifelse(is.na(time_dimension$holiday_ind), FALSE, time_dimension$holiday_ind )
time_dimension$event_oscars <- NULL

# Write data frame..
write.csv(time_dimension, "time_dimension.csv")

# Cleanup
rm(list = ls())
