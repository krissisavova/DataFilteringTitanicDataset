# Data Filtering Project - Titanic Dataset 
# Author: Kristina Savova, F110572
# Course: MITM301: Data Mining
# This program has been created by Kristina Savova

# Step 1: Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 2: Load Dataset
# CSV файлът трябва да се вкара чрез File / Import Dataset / From text (base)

titanic_data <- titanic

# Step 3: Data filtering
# 3.1 Remove rows with missing ESSENTIAL values only
# For crew members: fare/sibsp/parch can be N/A, so we only filter critical columns
data_clean <- titanic_data %>%
  filter(!is.na(survived) & !is.na(class) & !is.na(name))

# 3.2 Remove duplicate rows
filtered_data <- data_clean %>% distinct()

# 3.3 Filter anomalies --> we should keep "fare" within reasonable bounds OR allow N/A for crew
filtered_data <- filtered_data %>%
  filter(is.na(fare) | (fare > 0 & fare <= 300))

# 3.4 Add a column --> Crew vs Passenger
filtered_data <- filtered_data %>%
  mutate(passenger_type = case_when(
    class %in% c("1st", "2nd", "3rd") ~ "Passenger",
    TRUE ~ "Crew"
  ))

# Checking Results
print("Class distribution:")
table(filtered_data$class)
print("Passenger type distribution:")
table(filtered_data$passenger_type)

# 3.5 Add derived columns for analysis
# Extract the title from the 'name' column
filtered_data <- filtered_data %>%
  mutate(Title = sub(".*, (.*?)\\..*", "\\1", name))

# Group embarked locations itno major ports (handle N/A values)
filtered_data <- filtered_data %>%
  mutate(EmbarkedGroup = case_when(
    embarked %in% c("S", "C", "Q") ~ embarked,
    is.na(embarked) ~ "Unknown",
    TRUE ~ "Other"
  ))

# View filtered dataset summary
summary(filtered_data)

# --- Step 4: Exploratory Data Analysis and Visualization ---
# 4.1 Survival Rate by Category (Crew vs Passenger)
plot_passenger_type <- ggplot(filtered_data, aes(x = passenger_type, fill = factor(survived))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  labs(title = "Survival: Passengers vs Crew",
       x = "Category",
       y = "Count",
       fill = "Survived (0 = No, 1 = Yes)") +
  theme_minimal()

print(plot_passenger_type)

# Survival statistics
survival_stats <- filtered_data %>%
  group_by(passenger_type) %>%
  summarise(
    total = n(),
    survived = sum(survived),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  )

print("Survival rates by category:")
print(survival_stats)

# H0: Women in 1st class had the highest survival rate, and men in 3rd class had the lowest

# 4.2 Survival Rate by Class and Gender (only for passengers with gender data)
passenger_data <- filtered_data %>% filter(passenger_type == "Passenger" & !is.na(gender))

if(nrow(passenger_data) > 0) {
  plot_class_gender <- ggplot(passenger_data, aes(x = factor(class), fill = factor(survived))) +
    geom_bar(position = "dodge") +
    facet_wrap(~gender) +
    labs(title = "Survival by Class and Gender (Passengers Only)",
         x = "Class",
         y = "Count",
         fill = "Survived (0 = No, 1 = Yes)") +
    theme_minimal()
  print(plot_class_gender)
}

#H0: There were mostly people between 25 and 40 on the ship, therefore, 
# there were more victims in this age range.

# 4.3 Age Distribution by Survival (only for those with age data)
age_data <- filtered_data %>% filter(!is.na(age))

if(nrow(age_data) > 0) {
  plot_age <- ggplot(age_data, aes(x = age, fill = factor(survived))) +
    geom_histogram(binwidth = 5, position = "dodge") +
    labs(title = "Age Distribution by Survival",
         x = "Age",
         y = "Count",
         fill = "Survived (0 = No, 1 = Yes)") +
    theme_minimal()
  print(plot_age)
}

# Let's create a boxplot, too
plot_age_boxplot <- ggplot(filtered_data, aes(x = factor(survived), y = age, fill = factor(survived))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Survival",
       x = "Survived (0 = No, 1 = Yes)",
       y = "Age",
       fill = "Survived") +
  theme_minimal()
print(plot_age_boxplot)


# 4.4 Survival Rate by Embarked Location
plot_embarked <- ggplot(filtered_data, aes(x = EmbarkedGroup, fill = factor(survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Rate by Embarked Location",
       x = "Embarked Location",
       y = "Count",
       fill = "Survived (0 = No, 1 = Yes)") +
  theme_minimal()

print(plot_embarked)

# --- Step 5: Conclusion ---
# This script demonstrates data filtering techniques applied to the Titanic dataset.
# Further exploration can involve predictive modeling, correlation analysis, or deeper insights into specific subgroups.
