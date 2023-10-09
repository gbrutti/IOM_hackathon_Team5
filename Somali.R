library("readxl")
DTM_FMS_Hack2023 <- read_excel("DTM_FMS_Hack2023.xlsx")

somali <- subset(DTM_FMS_Hack2023, departed_state_region_admin1=="Somali")

#### Cleaning ####

install.packages("janitor")
library(janitor)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)

somali <- somali %>%
  mutate(is_idp = ifelse(country == "Ethiopia", TRUE, FALSE))

somali <-
  somali |> 
  dplyr::mutate(date_of_survey = excel_numeric_to_date(date_of_survey),
                year = lubridate::year(date_of_survey))

table(DTM_FMS_Hack2023$departed_state_region_admin1 == "Somali")
prop.table(table(somali$sex == "Female"))

ggplot(ethiopia, aes(x = Var2, y=Freq)) +
  geom_bar(
    aes(color = Var1, fill = Var1),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + facet_grid(.~Var3)

#### Women Reasons for Migration by Year ####

# Women: Reasons for Migration by Year 
# Men: Reasons for Migration by Year
# Create a new data frame that contains the number of females per category per year

somali$female_count <- ifelse(somali$sex == "Female", 1, 0)

# Create a new data frame that contains the number of females per category per year
female_counts <- data.frame(
  year = somali$year,
  category = case_when(
    as.logical(somali$reasons_for_migration_conflict_and_insecurity) ~ "Conflict and Insecurity",
    as.logical(somali$reasons_for_migration_economic_reasons) ~ "Economic Reasons",
    as.logical(somali$reasons_for_migration_access_to_services) ~ "Access to Services",
    as.logical(somali$reasons_for_migration_environmetal) ~ "Environmental Reasons",
    as.logical(somali$reasons_for_migration_covid19_related) ~ "COVID-19 Related",
    as.logical(somali$reasons_for_migration_na) ~ "NA",
    TRUE ~ "Others"
  ),
  count = somali$female_count
)

female_counts <- female_counts %>%
  group_by(year, category) %>%
  summarize(count = sum(count))

# Create a bar plot
ggplot(female_counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, ncol = 3) +
  labs(title = "Total Number of Females per Category per Year",
       x = "Category",
       y = "Total Count") +
  theme_classic()

#### Men Reasons for Migration by Year ####

somali$male_count <- ifelse(somali$sex == "Male", 1, 0)

# Create a new data frame that contains the number of females per category per year
male_counts <- data.frame(
  year = somali$year,
  category = case_when(
    as.logical(somali$reasons_for_migration_conflict_and_insecurity) ~ "Conflict and Insecurity",
    as.logical(somali$reasons_for_migration_economic_reasons) ~ "Economic Reasons",
    as.logical(somali$reasons_for_migration_access_to_services) ~ "Access to Services",
    as.logical(somali$reasons_for_migration_environmetal) ~ "Environmental Reasons",
    as.logical(somali$reasons_for_migration_covid19_related) ~ "COVID-19 Related",
    as.logical(somali$reasons_for_migration_na) ~ "NA",
    TRUE ~ "Others"
  ),
  count = somali$male_count
)

esex <- subset(somali, year==2018)
prop.table(table(esex$sex, esex$reasons_for_migration_environmetal), 1)

male_counts <- male_counts %>%
  group_by(year, category) %>%
  summarize(count = sum(count))

# Create a bar plot
ggplot(male_counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, ncol = 3) +
  labs(title = "Total Number of Males per Category per Year",
       x = "Category",
       y = "Total Count") +
  theme_classic()

# Combine female and male counts
combined_counts <- bind_rows(
  female = female_counts,
  male = male_counts,
  .id = "sex"
)

# Define custom colors
custom_colors <- c("#A30A8B", "grey")

# Create the grouped bar plot with custom colors
ggplot(combined_counts, aes(x = category, y = count, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, ncol = 3) +
  labs(title = "Number of Females and Males per Category per Year",
       x = "Category",
       y = "Total Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = custom_colors) 

# If you experienced -> cited environmental reasons

severity->frequency 










# Somali region: 

## 2023: Flood, 

## 2022: Drought, Epidemic, 

## 2020: Flood

## 2019: Epidemic, Locust, Flood, 

## 2018: Epidemic

#### % Gender Environment ####
results <- data.frame(year = unique(somali$year), percentage_women = numeric(length(unique(somali$year))), percentage_men = numeric(length(unique(somali$year))))

for (year in unique(somali$year)) {
  # Subset the data for the current year
  year_data <- somali[somali$year == year, ]
  
  # Tabulate the counts of "1" for each gender
  table_gender <- table(year_data$sex, year_data$reasons_for_migration_environmetal)
  
  # Calculate the percentage for women and men for the current year
  percentage_women <- (table_gender["Female", "1"] / sum(table_gender["Female", ])) * 100
  percentage_men <- (table_gender["Male", "1"] / sum(table_gender["Male", ])) * 100
  
  # Store the results in the results data frame
  results[results$year == year, "percentage_women"] <- percentage_women
  results[results$year == year, "percentage_men"] <- percentage_men
}

# Print the results
print(results)

ggplot(results, aes(x = factor(year), y = percentage_women, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men, fill = "Men"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite Environmental Reasons for \n Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "darkblue", "Men" = "white")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)),  # Adjust the title size
    axis.title.x = element_text(size = 14),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 14),  # Adjust the y-axis label size
    axis.text.x = element_text(size = 12),   # Adjust the x-axis tick label size
    axis.text.y = element_text(size = 12)    # Adjust the y-axis tick label size
  )

ggplot(results, aes(x = factor(year), y = percentage_women, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men, fill = "Men"), stat = "identity", 
           position = position_dodge(width = 0.4), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite \n Environmental Reasons for Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "#A30A8B", "Men" = "#0A3CA3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
    axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
    axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
    axis.text.y = element_text(size = 14),   # Adjust the y-axis tick label size
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank()        # Remove minor grid lines
  )
#### % Gender Economy ####
economy <- data.frame(year = unique(somali$year), percentage_women_econ = numeric(length(unique(somali$year))), percentage_men_econ = numeric(length(unique(somali$year))))

for (year in unique(somali$year)) {
  # Subset the data for the current year
  year_data_economy <- somali[somali$year == year, ]
  
  # Tabulate the counts of "1" for each gender
  table_gender_economy <- table(year_data_economy$sex, year_data_economy$reasons_for_migration_economic_reasons)
  
  # Calculate the percentage for women and men for the current year
  percentage_women_econ <- (table_gender_economy["Female", "1"] / sum(table_gender_economy["Female", ])) * 100
  percentage_men_econ <- (table_gender_economy["Male", "1"] / sum(table_gender_economy["Male", ])) * 100
  
  # Store the results in the results data frame
  economy[economy$year == year, "percentage_women_econ"] <- percentage_women_econ
  economy[economy$year == year, "percentage_men_econ"] <- percentage_men_econ
}

# Print the results
print(economy)

ggplot(economy, aes(x = factor(year), y = percentage_women_econ, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men_econ, fill = "Men"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite Economic Reasons for Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "#A30A8B", "Men" = "#0A3CA3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
    axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
    axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
    axis.text.y = element_text(size = 14),   # Adjust the y-axis tick label size
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank()        # Remove minor grid lines
  )


#### % Gender Service ####
service <- data.frame(year = unique(somali$year), percentage_women_s = numeric(length(unique(somali$year))), percentage_men_s = numeric(length(unique(somali$year))))

for (year in unique(somali$year)) {
  # Subset the data for the current year
  year_data_s <- somali[somali$year == year, ]
  
  # Tabulate the counts of "1" for each gender
  table_gender_s <- table(year_data_s$sex, year_data_s$reasons_for_migration_access_to_services)
  
  # Calculate the percentage for women and men for the current year
  percentage_women_s <- (table_gender_s["Female", "1"] / sum(table_gender_s["Female", ])) * 100
  percentage_men_s <- (table_gender_s["Male", "1"] / sum(table_gender_s["Male", ])) * 100
  
  # Store the results in the results data frame
  service[service$year == year, "percentage_women_s"] <- percentage_women_s
  service[service$year == year, "percentage_men_s"] <- percentage_men_s
}

# Print the results
print(service)

ggplot(service, aes(x = factor(year), y = percentage_women_s, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men_s, fill = "Men"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite Access to Service Reasons for Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "#A30A8B", "Men" = "#0A3CA3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
    axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
    axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
    axis.text.y = element_text(size = 14),   # Adjust the y-axis tick label size
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank()        # Remove minor grid lines
  )


#### % Gender Service ####
service <- data.frame(year = unique(somali$year), percentage_women_s = numeric(length(unique(somali$year))), percentage_men_s = numeric(length(unique(somali$year))))

for (year in unique(somali$year)) {
  # Subset the data for the current year
  year_data_s <- somali[somali$year == year, ]
  
  # Tabulate the counts of "1" for each gender
  table_gender_s <- table(year_data_s$sex, year_data_s$reasons_for_migration_access_to_services)
  
  # Calculate the percentage for women and men for the current year
  percentage_women_s <- (table_gender_s["Female", "1"] / sum(table_gender_s["Female", ])) * 100
  percentage_men_s <- (table_gender_s["Male", "1"] / sum(table_gender_s["Male", ])) * 100
  
  # Store the results in the results data frame
  service[service$year == year, "percentage_women_s"] <- percentage_women_s
  service[service$year == year, "percentage_men_s"] <- percentage_men_s
}

# Print the results
print(service)

ggplot(service, aes(x = factor(year), y = percentage_women_s, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men_s, fill = "Men"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite Access to Service Reasons for Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "blue", "Men" = "red")) +
  theme_minimal()

+
  theme(panel.background = element_rect(fill = "#E2E9F4")+

          scale_fill_manual(values = c("Women" = "#A30A8B", "Men" = "#0A3CA3")) +\
        

        
#### % Gender Conflict ####
conflict <- data.frame(year = unique(somali$year), percentage_women_c = numeric(length(unique(somali$year))), percentage_men_c = numeric(length(unique(somali$year))))

for (year in unique(somali$year)) {
  # Subset the data for the current year
  year_data_c <- somali[somali$year == year, ]
  
  # Tabulate the counts of "1" for each gender
  table_gender_c <- table(year_data_c$sex, year_data_c$reasons_for_migration_conflict_and_insecurity)
  
  # Calculate the percentage for women and men for the current year
  percentage_women_c <- (table_gender_c["Female", "1"] / sum(table_gender_c["Female", ])) * 100
  percentage_men_c <- (table_gender_c["Male", "1"] / sum(table_gender_c["Male", ])) * 100
  
  # Store the results in the results data frame
  conflict[conflict$year == year, "percentage_women_c"] <- percentage_women_c
  conflict[conflict$year == year, "percentage_men_c"] <- percentage_men_c
}

# Print the results
print(conflict)

ggplot(conflict, aes(x = factor(year), y = percentage_women_c, fill = "Women")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  geom_bar(aes(x = factor(year), y = percentage_men_c, fill = "Men"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, width = 0.4) +
  labs(
    title = "Percentage of Women and Men Who Cite Access to Conflict and Insecurity Reasons for Migration by Year",
    x = "Year",
    y = "Percentage",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Women" = "blue", "Men" = "red")) +
  theme_minimal()

############ 


# Calculate the frequencies of destinations for each gender
destination_counts_male <- table(somali_severity$country[somali_severity$sex == "Male"])
destination_counts_female <- table(somali_severity$country[somali_severity$sex == "Female"])


# Create a function to plot a pie chart with labels and custom colors
plot_pie_chart <- function(counts, labels, title, colors) {
  pie(counts, labels = labels, col = colors)
  title(main = title)
  legend("topright", legend = labels, cex = 0.8, fill = colors)
}

# Create labels for the destinations
labels_male <- names(destination_counts_male)
labels_female <- names(destination_counts_female)

# Define custom colors for the pie slices
colors_male <- c("darkblue", "#0A3CA3", "lightblue")
colors_female <- c("#5D0862", "#A30A8B", "lightpink")

# Create the first pie chart for men with labels and custom colors
par(mfrow = c(1, 2))  # Create a 1x2 grid for multiple plots
plot_pie_chart(destination_counts_male, labels_male, "Destinations for Men", colors_male)

# Create the second pie chart for women with labels and custom colors
plot_pie_chart(destination_counts_female, labels_female, "Destinations for Women", colors_female)










library(lubridate)
somali_new <-
  somali|> 
  mutate(days_when_did_you_leave = case_when(when_did_you_leave == "Today" ~ days(0),
                                             when_did_you_leave == "If not today, less than 2 weeks ago" ~ days(0),
                                             when_did_you_leave == "Dont know/ No answer" ~ NA,
                                             when_did_you_leave == "Between 2 weeks and 3 months ago" ~ weeks(2),
                                             when_did_you_leave == "Between 3 and 6 months ago" ~ days(30 * 3),
                                             when_did_you_leave == "Between 6 and 12 months ago" ~ days(30 * 6),
                                             when_did_you_leave == "12 months or more ago" ~ days(30 * 12)),
         
         est_min_date_of_departure = date_of_survey - days_when_did_you_leave
         
         
  ) |> 
  relocate(when_did_you_leave, days_when_did_you_leave, est_min_date_of_departure, .before = 1)









table(somali$is_idp)






barplot(prop.table(table(ethiopia$SEX)))

table(somali$country)
barplot(table(somali$country))

table(somali$country, somali$sex)







table(somali$reasons_for_migration_environmetal)
table(somali$reasons_for_migration_environmetal, somali$year)

table(somali$reasons_for_migration_environmetal, somali$reasons_for_migration_economic_reasons, somali$reasons_for_migration_access_to_services)

mea



library(ggplot2)
library(dplyr)


percentage_data <- somali %>%
  group_by(year, sex) %>%
  summarise(Percentage = mean(somali$reasons_for_migration_environmetal * 100))


# Reshape the data for a table
percentage_table <- pivot_wider(percentage_table, names_from = Gender, values_from = Percentage)

# Print the resulting table
percentage_table
            
            