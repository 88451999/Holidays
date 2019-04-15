library(lubridate)
library(tidyverse)
library(httr)
library(jsonlite)

setwd("/tmp/testg/Holidays/")

# Function to create a data frame of Holidays 
# Parameters:
#   from_year     starting year to get
#   to_year       ending year
#   country       2 character country code for get holidays for
#
# function returns data frame with:
#   date          holiday date (YYYY-MM-DD)
#   name          holiday name
#   description   description of holiday
#   type          comma separated types (eg, National holiday, Observance...)
#   locations     locations where holiday is observed
Holidays <- function(from_year = 2019, to_year = 2019, country = "US") {
  
  calendarific_url <- "https://calendarific.com/api/v2/holidays?"
  api_key <- "a24180e5070fcaa9029480b19f51cbe67b3699f7"
  
  hols_colNames <- c("name", "description", "date", "locations", "type")
  
  holidays <- data.frame()

  # Loop through each year calling API to get the full year's holidays
  for (yearnum in from_year:to_year) {
    
    r1 <- GET(url = paste(calendarific_url, "api_key=", api_key, "&country=", country, "&year=", yearnum, sep=""))
    
    r2 <- content(r1, as = "text", encoding = "UTF-8")
    temp_hols <- data.frame(fromJSON(r2) )

    # Limit datatrame to name, description, date and locations
    holidays_df <- data.frame(temp_hols$response.holidays.name, temp_hols$response.holidays.description, temp_hols$response.holidays.date$iso, 
                              temp_hols$response.holidays.locations, stringsAsFactors = FALSE)

    # type is a list, so for each row combine the list for that row into
    # comma separated string
    holidays_df$type <- NA
    for (i in 1:length(temp_hols$response.holidays.type) ) {
      holidays_df[i, 5] <- paste(temp_hols$response.holidays.type[[i]], collapse=", ")
    }
    
    # rename columns
    colnames(holidays_df) <- hols_colNames
    
    # Some dates have times added so just take date portion
    holidays_df$date <- substring(holidays_df$date, 1, 10)
    
    holidays <- rbind(holidays, holidays_df)
  }
  
  # reorder data frame to have date first
  holidays[,c(3, 1, 2, 5, 4)]
}

holidays <- Holidays(1980, 2019, "US")
write.csv(holidays, paste("holidays_US.csv", sep=""), row.names = FALSE)

holidays <- Holidays(1980, 2019, "AU")
write.csv(holidays, paste("holidays_AU.csv", sep=""), row.names = FALSE)

# Cleanup
rm(list = ls())
