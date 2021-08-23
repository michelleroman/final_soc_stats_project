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

# Load General Social Survey data and simplify column names
raw_df <-
  readr::read_fwf("GSS.dat",
                  col_positions = fwf_positions(
                    start = c(1, 21, 41, 61, 81),
                    end = c(20, 40, 60, 80, 100),
                    col_names = c("YEAR", "ID_", "HEALTH", "CLASS", "HELPSICK")
                  )) %>%
  janitor::clean_names()

# This question was asked in 1975, 1983, 1984, 1986-Present
# Limit the data to when the question was asked

table(raw_df$year, exclude = NULL)

raw_df <- raw_df %>%
  filter(year == 1975 |
           between(year, 1983, 1984) |
           between(year, 1986, 2016))

table(raw_df$year, exclude = NULL)

## Review distribution of responses
#Subjective class identification
ggplot2::ggplot(data = raw_df) +
  ggplot2::geom_bar(mapping = aes(x = class))

#Condition of health
ggplot2::ggplot(data = raw_df) +
  ggplot2::geom_bar(mapping = aes(x = health))

#Should govt help pay for medical care, or should people take care of themselves?
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
  mutate(class = dplyr::if_else((class >= 5 | class == 0), NA_real_, class)) %>%
  mutate(health = dplyr::if_else((health >= 8 | health == 0), NA_real_, health)) %>%
  mutate(helpsick = dplyr::if_else((helpsick >= 8 | helpsick == 0), NA_real_, helpsick))

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

table(df$class, exclude= NULL)
table(df$health, exclude= NULL)
table(df$helpsick, exclude= NULL)

## Export final data file
output_path <- here::here("output") 
  
write.csv(df, paste0(output_path, "/new_final.csv"))


