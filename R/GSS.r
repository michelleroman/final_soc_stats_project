## Cleaning script to process General Social Survey data and prepare for analysis

# Load packages
library(here)
library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# Establish working directory
input_path <- here::here("input")
setwd(input_path)

# Identified metadata DCT file
metadata <- readLines("GSS.dct")
print(metadata)

# Load General Social Survey data
# WE are pulling the entire dataset going back to 1972
raw_df <-
  readr::read_fwf("GSS.dat",
                  col_positions = fwf_positions(
                    start = c(1, 21, 41, 61, 81),
                    end = c(20, 40, 60, 80, 100),
                    col_names = c("YEAR", "ID_", "HEALTH", "CLASS", "HELPSICK")
                  )) %>%
  janitor::clean_names()

## Review distribution of responses

#Subjective class identification
ggplot2::ggplot(data = raw_df) +
  ggplot2::geom_bar(mapping = aes(x = class))

#Condition of health
ggplot2::ggplot(data = raw_df) +
  ggplot2::geom_bar(mapping = aes(x = health))

#Should govt help pay for medical care
ggplot2::ggplot(data = raw_df) +
  ggplot2::geom_bar(mapping = aes(x = helpsick))


## We want to ensure that we have enough of a sample size to
## draw conclusions around the relationships between these
## variables.

# 0 values represent Not applicable and responses that are
# greater than 5 are equivalent to Don't know or No answer, we can safely recode
# these rows  to NULL since they are not relevant to our analysis.

# Recode outliers and null values
df <- raw_df %>%
  mutate(class = dplyr::if_else((class >= 5 |
                                   class == 0), NA_real_, class)) %>%
  mutate(health = dplyr::if_else((health >= 8 |
                                    health == 0), NA_real_, health)) %>%
  mutate(helpsick = dplyr::if_else((helpsick >= 8 |
                                      helpsick == 0), NA_real_, helpsick))

## Review distribution of cleaned responses
ggplot2::ggplot(data = df) +
  ggplot2::geom_bar(mapping = aes(x = class))

ggplot2::ggplot(data = df) +
  ggplot2::geom_bar(mapping = aes(x = health))

ggplot2::ggplot(data = df) +
  ggplot2::geom_bar(mapping = aes(x = helpsick))


## Recode variables with appropriate factor labels
df <- df %>%
  dplyr::mutate(
    class = class %>% forcats::as_factor() %>%
      forcats::fct_recode(
        "Lower" = "1" ,
        "Working" = "2",
        "Middle" = "3",
        "Upper" = "4"
      )
  ) %>%
  
  dplyr::mutate(
    health = health %>% forcats::as_factor() %>%
      forcats::fct_recode(
        "Excellent" = "1",
        "Good" = "2",
        "Fair" = "3",
        "Poor" = "4"
      )
  )                     
    
## ERROR: the below code didn't relabel anything 
## and there are too few labels for the values present. 
## We also only need the numeric values for analysis. 
#GSS$helpsick[GSS$helpsick=="Government help"]<-1


table(GSS$class, exclude= NULL)
table(GSS$health, exclude= NULL)
table(GSS$helpsick, exclude= NULL)

## Export final data file
output_path <- here::here("output") 
  
write.csv(GSS, output_path, "final.csv")


