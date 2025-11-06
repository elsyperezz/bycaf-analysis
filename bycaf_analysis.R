#bycaf.analysis.R

## Attach packages -------------------
library("tidyverse")
library("readxl")

## Attach data -------------------
bycaf_data<- read_excel("bycaf_data.xlsx")

# Frequency of Creek Name --------------------------------

df_creek_name <- bycaf_data %>%
  mutate(Creek_Name_B1a = str_split(Creek_Name_B1a, ", ")) %>%
  unnest(Creek_Name_B1a) %>%
  group_by(Creek_Name_B1a) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_creek_name, 5)
