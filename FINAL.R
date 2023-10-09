library("readxl")
DTM_FMS_Hack2023 <- read_excel("DTM_FMS_Hack2023.xlsx")

library(dplyr)
library(lubridate)
library(janitor)

# Create data set 
ethiopia <- subset(DTM_FMS_Hack2023, departed_from_which_country=="Ethiopia")
ethiopia <-
  ethiopia |> 
  dplyr::mutate(date_of_survey = excel_numeric_to_date(date_of_survey),
                year = lubridate::year(date_of_survey))

#### Number of Men and Women IDPs in Ethiopia #####

ethiopia <- ethiopia %>%
  mutate(is_idp = ifelse(country == "Ethiopia", TRUE, FALSE))

table(ethiopia$is_idp)

idp_counts <- ethiopia %>%
  group_by(year, sex) %>%
  summarize(idp_count = sum(is_idp))

library(ggplot2)

  ggplot(idp_counts, aes(x = year, y = idp_count, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of IDPs", fill = "Gender")


#### Number of IDPs per year in each region ####

# Calculate the number of IDPs in each region every year
idp_counts_by_region_and_year <- ethiopia %>%
  group_by(departed_state_region_admin1, year) %>%
  summarize(idp_count = sum(is_idp))

# Create a bar plot using the `ggplot2` package

  
  ggplot(idp_counts_by_region_and_year, aes(x = year, y = idp_count, fill = departed_state_region_admin1)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Number of Internally Situated Migrants", fill = "Region") +
    scale_fill_manual(values = c("Somali" = "red")) +
    theme(panel.background = element_rect(fill = "#E2E9F4"),
          axis.title.x = element_text(size = 14),  # Adjust the size as needed
          axis.title.y = element_text(size = 14))



#### Number EDPs in each region every year ####
  
edp_counts_by_region_and_year <- ethiopia %>%
    group_by(departed_state_region_admin1, year) %>%
    summarize(edp_count = sum(is_idp == FALSE))  # Corrected the condition
  
# Create a bar plot using the `ggplot2` package
ggplot(edp_counts_by_region_and_year, aes(x = year, y = edp_count, fill = departed_state_region_admin1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of Externally Situated Migrants", fill = "Region") +
  scale_fill_manual(values = c("Somali" = "red")) +
  theme(panel.background = element_rect(fill = "#E2E9F4"),
        axis.title.x = element_text(size = 14),  # Adjust the size as needed
        axis.title.y = element_text(size = 14))

#### Number of Men and Women IDPs in Somalia ####
# Calculate the number of men and women who are IDPs each year
idp_counts_by_sex_and_year_somali <- df %>%
  group_by(sex, year) %>%
  summarize(idp_count = sum(is_idp))

# Create a bar plot using the `ggplot2` package
library(ggplot2)
ggplot(idp_counts_by_sex_and_year_somali, aes(x = sex, y = idp_count, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sex", y = "Number of IDPs", fill = "Sex")

#### Where Men and Women EDPs from Somali Go ####

# Calculate the number of male and female migrants for each country and year
somalimigrant_counts_by_country_gender_and_year <- df %>%
  group_by(country, sex, year) %>%
  count()

# Create a bar plot using the `ggplot2` package
ggplot(somalimigrant_counts_by_country_gender_and_year, aes(x = country, y = n, fill = sex)) +
  geom_bar(stat = "sum", position = "dodge") +
  facet_wrap(~year)

library(lubridate)
ethiopia_new <-
  ethiopia |> 
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

library(lubridate)
ethiopia_new <-
  ethiopia |> 
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



ethiopia_new <-
  ethiopia_new |> 
  dplyr::mutate(year_depart = lubridate::year(est_min_date_of_departure))

ethiopia_new <-
  ethiopia_new |> 
  dplyr::mutate(month_depart = lubridate::month(est_min_date_of_departure))

write.csv(ethiopia_new, "cleaned2_FMSdata.csv", row.names = FALSE)


