# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
data <- read_csv("C:/Users/User/Downloads/Written test/Zimbabwe_children_under5_interview.csv")

# View the first few rows to understand the structure
head(data)

# Convert interview_date to a date format and extract the month
data$interview_date <- dmy(data$interview_date)
data$interview_month <- month(data$interview_date)

# Keep only relevant columns and filter for children aged 3 or 4
relevant_columns <- c("interview_date", "interview_month", "child_age_years", "EC6", "EC7", "EC8", "EC9", "EC10", "EC11", "EC12", "EC13", "EC14", "EC15")
data_filtered <- data %>% 
  select(all_of(relevant_columns)) %>% 
  filter(child_age_years == 3 | child_age_years == 4)

# Replace codes with meaningful labels
data_filtered <- data_filtered %>%
  mutate(across(starts_with("EC"), ~case_when(
    . == 1 ~ "Yes",
    . == 2 ~ "No",
    TRUE ~ "DK"
  )))

# Step 2: Analysis

# Group by interview month and calculate the percentage of "Yes" for each indicator
monthly_summary <- data_filtered %>%
  group_by(interview_month) %>%
  summarise(
    Literacy_Math = mean((EC6 == "Yes") & (EC7 == "Yes") & (EC8 == "Yes")) * 100,
    Physical = mean((EC9 == "Yes") & (EC10 == "Yes")) * 100,
    Learning = mean((EC11 == "Yes") & (EC12 == "Yes")) * 100,
    Socio_emotional = mean((EC13 == "Yes") & (EC14 == "Yes") & (EC15 == "No")) * 100
  )

# Step 3: Visualization

# Create a line plot for each educational area
ggplot(monthly_summary, aes(x = interview_month)) +
  geom_line(aes(y = Literacy_Math, color = "Literacy + Math"), size = 1) +
  geom_line(aes(y = Physical, color = "Physical"), size = 1) +
  geom_line(aes(y = Learning, color = "Learning"), size = 1) +
  geom_line(aes(y = Socio_emotional, color = "Socio-emotional"), size = 1) +
  labs(
    title = "Monthly Evolution of Educational Performance for 3- and 4-Year-Old Children",
    x = "Month",
    y = "Percentage of Children Meeting Criteria",
    color = "Educational Area"
  ) +
  theme_minimal()

# Export the plot
ggsave("education_performance_plot.png")
