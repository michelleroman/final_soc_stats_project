
# Load packages

library(here)
library(janitor)
library(readr)
library(dplyr)

# Establish working directory
input_path <- here::here("input")

setwd(input_path)

# Load General Social Survey data
GSS <-
  readr::read_fwf(
    "GSS.dat",
    col_positions = fwf_positions(
      start = c(1, 21, 41, 61, 81),
      end = c(20, 40, 60, 80, 100),
      col_names = c("YEAR", "ID_", "HEALTH", "CLASS", "HELPSICK")
  )) %>%
  janitor::clean_names()

## Review distribution of responses
table(GSS$class, exclude= NULL)
table(GSS$health, exclude= NULL)
table(GSS$helpsick, exclude= NULL)

## Set blank or outlier values to NULL/NA
## Drop answers with fewer than 1000 rows
## Drop 0 since it is equivalent to NA

#GSS$class[GSS$class>=5] <- NA
#GSS$class[GSS$class==0] <- NA

#GSS$health[GSS$health>=8] <- NA
#GSS$health[GSS$health==0] <- NA

GSS$helpsick[GSS$helpsick>=8|GSS$helpsick==0] <- NA

table(GSS$class, exclude= NULL)
table(GSS$health, exclude= NULL)
table(GSS$helpsick, exclude= NULL)

## Recode variables 

## ERROR: the below code didn't relabel anything 
## and there are too few labels for the values present. 
## We also only need the numeric values for analysis. 
#GSS$helpsick[GSS$helpsick=="Government help"]<-1


GSS$health <- factor(GSS$health, labels = c("Excellent", "Good", "Fair","Poor"))
GSS$class <- factor(GSS$class, labels = c("Lower", "Working", "Middle", "Upper"))

table(GSS$class, exclude= NULL)
table(GSS$health, exclude= NULL)
table(GSS$helpsick, exclude= NULL)

## Export final data file
output_path <- here::here("output") 
  
write.csv(GSS, output_path, "final.csv")


