#bycaf.analysis.R

## Attach packages -------------------
library("tidyverse")
library("readxl")

## Attach data -------------------
bycaf_data<- read_excel("bycaf_data.xlsx")

file_path <- "C:/Users/elsy.perez/Documents/GitHub/bycaf-analysis/bycaf_data.xlsx"

## Read sheets -------------------
b5map_data <- read_excel(file_path, sheet = "B5Map")
c2map_data <- read_excel(file_path, sheet = "C2Map")


## -------- Calculate frequencies ------------------------
# Frequency of common previous area of living ------------------------
df_common_living <- bycaf_data %>%
  unnest(Previous_Place_Of_Residence_A6) %>%
  group_by(Previous_Place_Of_Residence_A6) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_common_living, 10)

# Frequency of common occupations ------------------------
df_common_occ <- bycaf_data %>%
  unnest(Occupation_Class) %>%
  group_by(Occupation_Class) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_common_occ, 10)

# Frequency of Creek Name --------------------------------
df_creek_name <- bycaf_data %>%
  mutate(Creek_Name_B1a = str_split(Creek_Name_B1a, ", ")) %>%
  unnest(Creek_Name_B1a) %>%
  group_by(Creek_Name_B1a) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_creek_name, 5)

#Frequency of common comments for use, value, appreciation, and association ---
#use
df_use <- b5map_data %>%
  unnest(Revised_Use) %>%
  group_by(Revised_Use) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_use, 5)

#value
df_value <- bycaf_data %>%
  mutate(Value_B4 = str_split(Value_B4, ", ")) %>%
  unnest(Value_B4) %>%
  group_by(Value_B4) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_value, 5)

#appreciation
df_appreciation <- bycaf_data %>%
  unnest(CICES_Code) %>%
  group_by(CICES_Code) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_appreciation, 5)

#association
df_association <- bycaf_data %>%
  mutate(General_Association_B3 = str_split(General_Association_B3, ", ")) %>%
  unnest(General_Association_B3) %>%
  group_by(General_Association_B3) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_association, 5)

#Frequency of common concerns
df_concerns <- c2map_data %>%
  unnest(Revised_Concern) %>%
  group_by(Revised_Concern) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_concerns, 5)

#Frequency of common changes
df_changes <- bycaf_data %>%
  mutate(Specific_Changes_C1a = str_split(Specific_Changes_C1a, ",")) %>%
  unnest(Specific_Changes_C1a) %>%
  group_by(Specific_Changes_C1a) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_changes, 5)

#Frequency of future changes
df_future_changes <- bycaf_data %>%
  mutate(Changes_In_Future_C3 = str_split(Changes_In_Future_C3, ",")) %>%
  unnest(Changes_In_Future_C3) %>%
  group_by(Changes_In_Future_C3) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_future_changes, 5)

#Frequency of common person/entity responsible
df_responsibility <- c2map_data %>%
  mutate(Revised_Responsibility = str_split(Revised_Responsibility, ",")) %>%
  unnest(Revised_Responsibility) %>%
  group_by(Revised_Responsibility) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_responsibility, 5)

#Frequency of common ecosystem services
df_ecosystem_services <- b5map_data %>%
  group_by(CICES_Use_Code) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_ecosystem_services, 5)
