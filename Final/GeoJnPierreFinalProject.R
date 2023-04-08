# Load required packages
library(lubridate)
library(readxl)
library(dplyr)
# Read in the raw data
raw_data <- read_excel("RawData.xlsx", sheet = "Raw-Data")

# Read in the calendar data
calendar <- read_excel("RawData.xlsx", sheet = "Calendar")

# Remove duplicate rows based on `Receipt Date`
raw_data <- raw_data %>% distinct(`Receipt Date`, .keep_all = TRUE)

# Add Quarter and Year columns to the calendar sheet
calendar <- calendar %>%
  mutate(Quarter = quarter(Start_Date),
         Year = year(Start_Date))

# Add Quarter and Year columns to the raw_data sheet by matching the Receipt Date to the date ranges in the calendar
raw_data <- raw_data %>%
  mutate(Quarter = quarter(`Receipt Date`),
         Year = year(`Receipt Date`)) %>%
  left_join(calendar, by = c("Quarter", "Year"))

# Remove unnecessary columns from the final output
raw_data <- raw_data %>% select(-c("S.No", "Start_Date", "End_date"))

# Convert date columns to date format
raw_data <- raw_data %>%
  mutate(across(starts_with("Date"), as.Date))

raw_data <- raw_data %>%
  # Convert date columns to date format
  mutate(across(starts_with("Date"), as.Date)) %>%
  # Calculate In-transit Lead Time and Manufacturing Lead Time
  mutate(InTransitLeadTime = as.numeric(`Ship Date` - `Receipt Date`) +
           as.numeric(`Receipt Date` - InventoryInStockDate),
         ManufacturingLeadTime = as.numeric(FinishedGoodsProductionDate - ProductionStartDate))

