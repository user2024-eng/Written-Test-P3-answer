# Load necessary libraries
library(readxl)
library(dplyr)

# Load datasets
unicef_data <- read_excel("C:/Users/mdahmed/Desktop/Written test/Written Test P3 answer/Task 1/GLOBAL_DATAFLOW_2018-2022.xlsx", sheet = "Unicef data")
population_data <- read_excel("C:/Users/mdahmed/Desktop/Written test/Written Test P3 answer/Task 1/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", sheet = 1, skip = 25)
mortality_status <- read_excel("C:/Users/mdahmed/Desktop/Written test/Written Test P3 answer/Task 1/On-track and off-track countries.xlsx", sheet = 1)

# Clean and filter UNICEF data for relevant indicators and years
relevant_indicators <- c(
  "Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider",
  "Skilled birth attendant - percentage of deliveries attended by skilled health personnel"
)

filtered_data <- unicef_data %>%
  filter(Indicator %in% relevant_indicators & TIME_PERIOD >= 2018 & TIME_PERIOD <= 2022) %>%
  group_by(`Geographic area`, Indicator) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  ungroup() %>%
  select(`Geographic area`, Indicator, OBS_VALUE)

# Filter population data for the year 2022 and select relevant columns
population_data_filtered <- population_data %>%
  filter(Year == 2022) %>%
  select(Country, Births)

# Merge datasets
merged_data <- filtered_data %>%
  left_join(population_data_filtered, by = c("Geographic area" = "Country")) %>%
  left_join(mortality_status, by = c("Geographic area" = "Country"))

# Preview the merged data
head(merged_data)





#=======================================================================
#  Calculate Weighted Averages for On-Track and Off-Track Countries   #
#======================================================================

# Define the function to calculate weighted coverage
calculate_weighted_coverage <- function(data, indicator) {
  data %>%
    filter(Indicator == indicator) %>%
    group_by(Status.U5MR) %>%
    summarise(Weighted_Coverage = sum(OBS_VALUE * Births, na.rm = TRUE) / sum(Births, na.rm = TRUE))
}

# Calculate weighted coverage for ANC4
weighted_coverage_anc <- calculate_weighted_coverage(merged_data, relevant_indicators[1])

# Calculate weighted coverage for SBA
weighted_coverage_sba <- calculate_weighted_coverage(merged_data, relevant_indicators[2])

# Combine the results
weighted_coverage <- weighted_coverage_anc %>%
  rename(ANC4 = Weighted_Coverage) %>%
  left_join(weighted_coverage_sba %>% rename(SBA = Weighted_Coverage), by = "Status.U5MR")

# Print the results
print(weighted_coverage)


#=====================================
# Reshape data for visualization     =
#=====================================

library(ggplot2)
coverage_long <- weighted_coverage %>%
  pivot_longer(cols = c("ANC4", "SBA"), names_to = "Indicator", values_to = "Coverage")

# Create the plot
ggplot(coverage_long, aes(x = Status.U5MR, y = Coverage, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population-Weighted Coverage of Health Services",
       x = "Under-5 Mortality Status",
       y = "Weighted Coverage (%)",
       fill = "Health Indicator") +
  theme_minimal()




