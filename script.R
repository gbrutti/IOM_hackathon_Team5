df <- read.csv("mytable.csv")
library(janitor)
excel_numeric_to_date(df$DATE_OF_SURVEY)

df <-df |>dplyr::mutate(DATE_OF_SURVEY = excel_numeric_to_date(DATE_OF_SURVEY),
                        year = lubridate::year(DATE_OF_SURVEY))

df$year <- as.numeric(format(df$DATE_OF_SURVEY, "%Y"))

ethiopia <- df[df$DEPARTED_FROM_WHICH_COUNTRY=="Ethiopia",]
uganda <- df[df$DEPARTED_FROM_WHICH_COUNTRY=="Uganda",]

table(df$DEPARTED_FROM_WHICH_COUNTRY)
tb <- data.frame(table(ethiopia$WHAT_IS_THE_MAIN_REASON_FOR_YOUR_JOURNEY))
nrow(data.frame(ethiopia[ethiopia$REASONS_FOR_MIGRATION_ENVIRONMETAL == "1",]))

ethiopia_all <- ethiopia[ethiopia$REASONS_FOR_MIGRATION_ENVIRONMETAL == "1" |ethiopia$REASONS_FOR_MIGRATION_ACCESS_TO_SERVICES == "1",]
ethiopia_env <- ethiopia[ethiopia$REASONS_FOR_MIGRATION_ENVIRONMETAL == "1",]
nrow(all_sub_migrants) - nrow(env_migrants)
write.csv(tb, "motivations.csv")

#ethiopia <- ethiopia[ethiopia$SEX != "No Answer" | ethiopia$SEX != "dont want to answer",]
#ethiopia$SEX <- factor(ethiopia$SEX)

barplot(prop.table(table(ethiopia$SEX)))

barplot(prop.table(table(uganda$SEX)))
df$year <- as.numeric(format(df$DATE_OF_SURVEY, "%Y"))

library(ggplot2)
library(tidyverse)

# Histogram by group in ggplot2
ethiopia_df <- data.frame(table(ethiopia$SEX, ethiopia$year, ethiopia$DEPARTURE_ADMIN1_PCODE))

ethiopia_df$Var2 <- as.numeric(ethiopia_df$Var2)
ethiopia_df[ethiopia_df == 1] <- 18
ethiopia_df[ethiopia_df == 2] <- 19
ethiopia_df[ethiopia_df == 3] <- 20
ethiopia_df[ethiopia_df == 4] <- 21
ethiopia_df[ethiopia_df == 5] <- 22
ethiopia_df[ethiopia_df == 6] <- 23

ggplot(ethiopia_df, aes(x = Var2, y=Freq)) +
  geom_bar(
    aes(color = Var1, fill = Var1),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + facet_grid(.~Var3)

ethiopia$DEPARTED_STATE_REGION_ADMIN1[ethiopia$DEPARTURE_ADMIN1_PCODE == "ET12"]
d <- data.frame(table(ethiopia$DEPARTED_STATE_REGION_ADMIN1))
external <- read.csv("ExternalDataHack2023.csv")
admin <- read.csv("admin1_info.csv")
table(external$Data.Variable)
hazards <- external[(external$Data.Variable =="HazardRisk_cropfailure"|external$Data.Variable =="HazardRisk_drought"|
                       external$Data.Variable =="HazardRisk_heatwave"|external$Data.Variable =="HazardRisk_riverflood"|
                       external$Data.Variable =="HazardRisk_tropicalcyclone"|external$Data.Variable =="HazardRisk_wildfire")
                    & external$ADMIN_0_NAMES == "Ethiopia",]



#LandUse_pasture
#LandUse_cropland
#LandUse_builtup
#LandUse_rangeland

tb <- data.frame(table(ethiopia$ADMIN_1_NAMES))

clean <- read.csv("cleaned2_FMSdata.csv")
clean$year_depart <- as.numeric(clean$year_depart)
clean$month_depart <- as.numeric(clean$month_depart)
clean$hazard_subject <- 0
clean$hazard_type <- "no"
clean$hazard_subject[clean$year_depart == 2019 & !is.na(clean$year_depart) & clean$month_depart == 10 & !is.na(clean$month_depart) & clean$fmp_ADM1_Name == "Somali"] <- 1
clean$hazard_type[clean$year_depart == 2019 & !is.na(clean$year_depart) & clean$month_depart == 10 & !is.na(clean$month_depart) & clean$fmp_ADM1_Name == "Somali"] <- "flood"
clean$hazard_subject[clean$year_depart == 2020 & !is.na(clean$year_depart) & (clean$month_depart == 4|clean$month_depart == 5) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name == "Dire Dawa"|clean$fmp_ADM1_Name == "SNNP")] <- 1
clean$hazard_type[clean$year_depart == 2020 & !is.na(clean$year_depart) & (clean$month_depart == 4|clean$month_depart == 5) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name == "Dire Dawa"|clean$fmp_ADM1_Name == "SNNP")] <- "flood"
clean$hazard_subject[clean$year_depart == 2020 & !is.na(clean$year_depart) & (clean$month_depart >= 6 & clean$month_depart <= 9) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name == "Afar" | clean$fmp_ADM1_Name == "Oromia" | clean$fmp_ADM1_Name=="Somali" | clean$fmp_ADM1_Name=="Gambella"| clean$fmp_ADM1_Name=="SNNP")] <- 1
clean$hazard_type[clean$year_depart == 2020 & !is.na(clean$year_depart) & (clean$month_depart >= 6 & clean$month_depart <= 9) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name == "Afar" | clean$fmp_ADM1_Name == "Oromia" | clean$fmp_ADM1_Name=="Somali"| clean$fmp_ADM1_Name=="Gambella"| clean$fmp_ADM1_Name=="SNNP")] <- "flood"
clean$hazard_subject[clean$year_depart == 2021 & !is.na(clean$year_depart) & clean$month_depart == 5 & !is.na(clean$month_depart)] <- 1
clean$hazard_type[clean$year_depart == 2021 & !is.na(clean$year_depart) & clean$month_depart == 5 & !is.na(clean$month_depart)] <- "flood"
clean$hazard_subject[((clean$year_depart == 2021 & clean$month_depart >= 5) | (clean$year_depart == 2022 & clean$month_depart <= 2)) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Tigray" | clean$fmp_ADM1_Name=="Afar" |clean$fmp_ADM1_Name=="Amhara")] <- 1
clean$hazard_type[((clean$year_depart == 2021 & clean$month_depart >= 5) | (clean$year_depart == 2022 & clean$month_depart <= 2)) & !is.na(clean$year_depart) & !is.na(clean$month_depart)& (clean$fmp_ADM1_Name=="Tigray" | clean$fmp_ADM1_Name=="Afar" |clean$fmp_ADM1_Name=="Amhara")] <- "drought"
clean$hazard_subject[clean$year_depart == 2022 & (clean$month_depart >= 8 & clean$month_depart <= 10) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Gambella" | clean$fmp_ADM1_Name=="Amhara" |clean$fmp_ADM1_Name=="Afar" | clean$fmp_ADM1_Name=="Oromia")] <- 1
clean$hazard_type[clean$year_depart == 2022 & (clean$month_depart >= 8 & clean$month_depart <= 10) & !is.na(clean$year_depart) & !is.na(clean$month_depart)& (clean$fmp_ADM1_Name=="Gambella" | clean$fmp_ADM1_Name=="Amhara" |clean$fmp_ADM1_Name=="Afar" | clean$fmp_ADM1_Name=="Oromia")] <- "flood"
clean$hazard_subject[(clean$year_depart == 2022 | (clean$year_depart == 2023 & clean$month_depart <= 2)) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Somali" | clean$fmp_ADM1_Name=="South West Ethiopia" |clean$fmp_ADM1_Name=="SNNP" | clean$fmp_ADM1_Name=="Oromia")] <- 1
clean$hazard_type[(clean$year_depart == 2022 | (clean$year_depart == 2023 & clean$month_depart <= 2)) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Somali" | clean$fmp_ADM1_Name=="South West Ethiopia" |clean$fmp_ADM1_Name=="SNNP" | clean$fmp_ADM1_Name=="Oromia")] <- "drought"
clean$hazard_subject[clean$year_depart == 2023 & (clean$month_depart >= 3 & clean$month_depart <= 5) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Somali" |clean$fmp_ADM1_Name=="Afar" | clean$fmp_ADM1_Name=="Oromia")] <- 1
clean$hazard_type[clean$year_depart == 2023 & (clean$month_depart >= 3 & clean$month_depart <= 5) & !is.na(clean$year_depart) & !is.na(clean$month_depart) & (clean$fmp_ADM1_Name=="Somali" |clean$fmp_ADM1_Name=="Afar" | clean$fmp_ADM1_Name=="Oromia")] <- "flood"


library(ggplot2)
library(tidyverse)

# Histogram by group in ggplot2
clean <- clean[clean$sex != "No Answer",]
ethiopia_df <- data.frame(table(clean$sex, clean$hazard_subject, clean$departure_admin1_pcode))

ggplot(ethiopia_df, aes(x = Var2, y=Freq)) +
  geom_bar(
    aes(color = Var1, fill = Var1),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )

severity <- read.csv("severity_clean.csv")

clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Gambela"] <- "Gambella"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Harari"] <- "Hareri"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Beneshangul Gumuz"] <- "Beneshangul Gumu"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Benishangul Gumuz"] <- "Beneshangul Gumu"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Beneshangul Gumz"] <- "Beneshangul Gumu"
severity$ADM1_NAME[severity$ADM1_NAME == "SNNPR"] <- "SNNP"
severity$ADM1_NAME[severity$ADM1_NAME == "Gambela"] <- "Gambella"
names(clean)[22] <- "ADM1_NAME"
names(clean)[54] <- "year_survey"
names(clean)[56] <- "year"
names(clean)[57] <- "month"
clean$year <- as.numeric(clean$year)
severity$year <- as.numeric(severity$year)
clean$month <- as.numeric(clean$month)
severity$month <- as.numeric(severity$month)
severity <- severity[, c(5, 12, 13, 14)]
data <- merge(clean, severity, by = c("ADM1_NAME", "year", "month"))

data <- data[data$sex != "dont want to answer",]
#data <- data[data$ADM1_NAME == "Somali",]
data_table <- data.frame(table(data$sex, data$pdsi, data$reasons_for_migration_environmetal, data$ADM1_NAME, data$reasons_for_migration_access_to_services, data$year))
names(data_table)[2] <- "drought_severity"
names(data_table)[7] <- "number_of_migrants"
names(data_table)[1] <- "sex"
names(data_table)[3] <- "environmental_motive"
names(data_table)[5] <- "access_to_service_motive"
names(data_table)[4] <- "adm1_region"
names(data_table)[6] <- "year"
data_table$number_of_migrants <- as.numeric(data_table$number_of_migrants)
data_table <- data_table[data_table$number_of_migrants != 0 & data_table$environmental_motive == 1,]
ggplot(data_table, aes(x=drought_severity, y=number_of_migrants, color=sex)) + geom_point() + facet_grid(.~year)
data_table$drought_severity <- as.numeric(data_table$drought_severity) 

model <- lm(data_table$number_of_migrants ~ poly(data_table$drought_severity,2) + data_table$sex)
summary(model)
model1 <- lm(data_table$number_of_migrants ~ poly(data_table$drought_severity,2)*data_table$sex)
summary(model1)
model2 <- lm(data_table$number_of_migrants ~ data_table$sex)
summary(model2)

write.csv(data, "data.csv")
