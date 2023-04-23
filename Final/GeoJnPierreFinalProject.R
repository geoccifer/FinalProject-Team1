# Load required packages
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
# Read in the raw data
Raw_Data <- read_excel("RawData.xlsx", sheet = "Raw-Data")

# Read in the calendar data
calendar <- read_excel("RawData.xlsx", sheet = "Calendar")%>%
  mutate(Quarter = as.integer(Quarter))

# Add Quarter and Year columns to the raw_data sheet by matching the Receipt Date to the date ranges in the calendar
Raw_Data <- Raw_Data %>%
  mutate(Quarter = quarter(`Receipt Date`),
         Year = year(`Receipt Date`)) %>%
  left_join(calendar, by = c("Quarter", "Year"))

# Remove unnecessary columns from the final output
Raw_Data <- Raw_Data %>% select(-c("S.No", "Start_Date", "End_date"))

# Convert date columns to date format
Raw_Data <- Raw_Data %>%
  mutate(across(starts_with("Date"), as.Date))

# Calculate In-transit Lead Time and Manufacturing Lead Time
Raw_Data <- Raw_Data %>%
  mutate(InTransit_Lead_Time = as.numeric(difftime(`Receipt Date`, `Ship Date`, units = "days")),
         Manufacturing_Lead_Time = as.numeric(difftime(`Ship Date`, `PO Download Date`, units = "days")))

# Ship Date = Manufacturing_Lead_Time + `PO Download Date`

# Receipt Date = `Ship Date` + InTransit_Lead_Time

# Ship Date = Manufacturing_Lead_Time + `PO Download Date`
Raw_Data$`Ship Date`[is.na(Raw_Data$`Ship Date`)] <- as.Date(Raw_Data$`PO Download Date`[is.na(Raw_Data$`Ship Date`)]) + 
  Raw_Data$Manufacturing_Lead_Time[is.na(Raw_Data$`Ship Date`)]

# Receipt Date = `Ship Date` + InTransit_Lead_Time
Raw_Data$`Receipt Date`[is.na(Raw_Data$`Receipt Date`)] <- as.Date(Raw_Data$`Ship Date`[is.na(Raw_Data$`Receipt Date`)]) +
  Raw_Data$InTransit_Lead_Time[is.na(Raw_Data$`Receipt Date`)]

impute_median <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

# Impute missing values with median
Raw_Data <- Raw_Data %>%
  mutate(across(everything(), impute_median))
