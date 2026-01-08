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
  filter(!is.na(Previous_Place_Of_Residence_A6)) %>%
  group_by(Previous_Place_Of_Residence_A6) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_common_living, 10)

# Frequency of common occupations ------------------------
df_common_occ <- bycaf_data %>%
  unnest(Occupation_Class) %>%
  filter(!is.na(Occupation_Class)) %>%
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
  mutate(CICES_Code = str_split(CICES_Code, ", ")) %>%
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
  mutate(Specific_Changes_C1a = str_split(Specific_Changes_C1a, ", ")) %>%
  unnest(Specific_Changes_C1a) %>%
  filter(!is.na(Specific_Changes_C1a)) %>%
  group_by(Specific_Changes_C1a) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_changes, 5)

#Frequency of future changes
df_future_changes <- bycaf_data %>%
  mutate(Changes_In_Future_C3 = str_split(Changes_In_Future_C3, ", ")) %>%
  unnest(Changes_In_Future_C3) %>%
  group_by(Changes_In_Future_C3) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

head(df_future_changes, 5)

#Frequency of common person/entity responsible
df_responsibility <- c2map_data %>%
  mutate(Revised_Responsibility = str_split(Revised_Responsibility, ", ")) %>%
  unnest(Revised_Responsibility) %>%
  filter(!is.na(Revised_Responsibility)) %>%
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
gender_ratio <- bycaf_data %>%
  summarize(
    males = length(Gender_A2[Gender_A2 == "Male"]),
    females = length(Gender_A2[Gender_A2 == "Female"])
  ) %>%
  mutate(ratio = males/females)

#proportion of area of residence
proportion_residence <- bycaf_data %>%
  group_by(Place_Of_Residence_A4) %>%
  summarize(Total = n()) %>%
  arrange(Total)

#ratio of knowing the creek
creek_knowledge_ratio <- bycaf_data %>%
  summarize(
    yes = length(Do_You_Know_The_Creek_B1[Do_You_Know_The_Creek_B1 == "Yes"]),
    no = length(Do_You_Know_The_Creek_B1[Do_You_Know_The_Creek_B1 == "No"])
  ) %>%
  mutate(ratio = yes/no)

#proportion of visit frequency
proportion_visit <- bycaf_data %>%
  group_by(Visit_Frequency_B2) %>%
  summarize(Total = n()) %>%
  arrange(-Total)

## -------- Visualizations ------------------------
#Boxplot of age vs value
age_value_boxplot <- bycaf_data %>%
  mutate(Value_B4 = str_split(Value_B4, ",")) %>%
  unnest(Value_B4) %>%
   filter(!is.na(Value_B4), !is.na(Age_A1)) %>%
  group_by(Value_B4)
ggplot(age_value_boxplot, aes(x = `Value_B4`, y = `Age_A1`)) +
  geom_boxplot(fill = "#457888", color = "black") +
  theme_pubclean() +
  labs(x = "Value of Creek", y = "Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#X2TOI use by gender
table(b5map_data$Gender, b5map_data$Revised_Use)
chisq.test(table(b5map_data$Gender, b5map_data$Revised_Use))

#Mosaic plot - use vs gender
ggplot(b5map_data) +
  geom_mosaic(color = "white", aes(x = product(Gender), fill = Revised_Use)) +
  theme_pubclean() + 
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  labs(x = "Gender", y = "Use of Creek") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#Barplot - gender vs use
ggplot(b5map_data, aes(x = Gender, y = Revised_Use, fill = Revised_Use)) +
  geom_bar(stat = "identity") +
  labs(title = "Use by Gender",
       x = "Gender", y = "Use of Creek") +
  theme_pubclean() +
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#Mosaic Plot/Grouped Bar Plot/X2TOI - area of residence (explanatory), use (response) - Are there differences in creek use or perception in the different communities?
ggplot(b5map_data) +
  geom_mosaic(color = "white", aes(x = product(Area_Residence), fill = Revised_Use)) +
  theme_pubclean() + 
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  labs(x = "Area of Residence", y = "Use of Creek") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#barplot^
ggplot(b5map_data, aes(x = Area_Residence, y = Revised_Use)) +
  geom_col(fill = "#457888", color = "white") +
  theme_pubclean()

ggplot(b5map_data, aes(x = Area_Residence, y = Revised_Use, fill = Revised_Use)) +
  geom_bar(stat = "identity") +
  labs(title = "Use by Area of Residence",
       x = "Area of Residence", y = "Use of Creek") +
  theme_pubclean() +
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#X2TOI^
table(b5map_data$Area_Residence, b5map_data$Revised_Use)
chisq.test(table(b5map_data$Area_Residence, b5map_data$Revised_Use))

#Mosaic Plot/Grouped Bar Plot/X2TOI - occupation (explanatory), appreciation (response) *
ggplot(bycaf_data) +
  geom_mosaic(color = "white", aes(x = product(Revised_Appreciation_CICES), fill = Occupation_Class)) +
  theme_pubclean() + 
  labs(x = "Appreciation of Creek", y = "Occupation of Residents") +
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#barplot^
ggplot(bycaf_data, aes(x = Revised_Appreciation_CICES, y = Occupation_Class)) +
  geom_col(fill = "#457888", color = "white") +
  theme_pubclean()

#grouped barplot^
appreciation_CICES <- bycaf_data %>%
  mutate(Revised_Appreciation_CICES = str_split(Revised_Appreciation_CICES, ", ")) %>%
  unnest(Revised_Appreciation_CICES) %>%
  group_by(Revised_Appreciation_CICES)
  
ggplot(appreciation_CICES, aes(x = Revised_Appreciation_CICES, y = Occupation_Class, fill = Occupation_Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Appreciation by Occupation",
       x = "Appreciation", y = "Occupation") +
  theme_pubclean() +
  scale_fill_manual(values = c("#d95e5e", "#457888", "#ecd294", "#2db7be", "#66C2A5","#5E4FA2", "#32888D", "#F46D43", "#9970AB", "#E78AC3")) +
  theme( 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#X2TOI^
table(bycaf_data$Occupation_Class, bycaf_data$Revised_Appreciation_CICES)
chisq.test(table(bycaf_data$Occupation_Class, bycaf_data$Revised_Appreciation_CICES))
