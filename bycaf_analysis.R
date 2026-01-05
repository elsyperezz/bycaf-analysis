#bycaf.analysis.R
install.packages("ggmosaic", repos = c('https://haleyjeppson.r-universe.dev', 'https://cloud.r-project.org')) #use this since it has been archived from R 4.4

## Attach packages -------------------
library("tidyverse")
library("readxl")
library("ggpubr")
library("ggmosaic")

## Attach data -------------------
bycaf_data<- read_excel("bycaf_data.xlsx", na = c("", "NA"))
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

## -------- Calculate averages ------------------------
#average age of respondents
mean(bycaf_data$Age_A1, na.rm = TRUE)

#lowest
min(bycaf_data$Age_A1, na.rm = TRUE)

#highest
max(bycaf_data$Age_A1, na.rm = TRUE)

#average length of time living in the area
mean(bycaf_data$Years_Of_Living_In_Area_A5, na.rm=TRUE)

#shortest length of time living in the area
min(bycaf_data$Years_Of_Living_In_Area_A5, na.rm = TRUE)

#longest length of time living in the area
max(bycaf_data$Years_Of_Living_In_Area_A5, na.rm = TRUE)

## -------- Calculate ratios ------------------------
#gender ratio
#ratio of area of residence
#ratio of knowing the creek
#visit frequency ratio

## -------- Visualizations ------------------------
#Boxplot of age vs value
age_value_boxplot <- bycaf_data %>%
  mutate(Value_B4 = str_split(Value_B4, ",")) %>%
  unnest(Value_B4) %>%
  group_by(Value_B4)
ggplot(age_value_boxplot, aes(x = `Value_B4`, y = `Age_A1`)) +
  geom_boxplot(fill = "#457888", color = "black") +
  theme_pubclean() +
  labs(x = "Value of Creek", y = "Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#X2TOI use by gender
b5map_data
table(b5map_data$Gender, b5map_data$Revised_Use)
chisq.test(table(b5map_data$Gender, b5map_data$Revised_Use))

#Mosaic plot - use vs gender
ggplot(b5map_data) +
  geom_mosaic(color = "white", aes(x = product(Gender), fill = Revised_Use)) +
  theme_pubclean() + 
  labs(x = "Gender", y = "Use of Creek")

#Barplot - gender vs use
ggplot(b5map_data, aes(x = Gender, y = Revised_Use)) +
  geom_col(fill = "#457888", color = "white") +
  theme_pubclean()

#Mosaic Plot/Grouped Bar Plot/X2TOI - area of residence (explanatory), use (response) - Are there differences in creek use or perception in the different communities?
#Mosaic Plot/Grouped Bar Plot/X2TOI - occupation (explanatory), appreciation (response) *
  
