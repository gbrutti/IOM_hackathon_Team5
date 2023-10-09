library(janitor)
df <- read.csv("mytable.csv")
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
write.csv(tb, "motivations.csv")

#ethiopia <- ethiopia[ethiopia$SEX != "No Answer" | ethiopia$SEX != "dont want to answer",]
#ethiopia$SEX <- factor(ethiopia$SEX)

barplot(prop.table(table(ethiopia$SEX)))

barplot(prop.table(table(uganda$SEX)))
df$year <- as.numeric(format(df$DATE_OF_SURVEY, "%Y"))

library(ggplot2)
library(tidyverse)

# Histogram by group in ggplot2
ethiopia <- ethiopia[ethiopia$REASONS_FOR_MIGRATION_ENVIRONMETAL == "1",]
ethiopia_df <- data.frame(table(ethiopia$SEX, ethiopia$year, ethiopia$DEPARTURE_ADMIN1_PCODE))

ethiopia_df$Var2 <- as.numeric(ethiopia_df$Var2)
ethiopia_df[ethiopia_df == 1] <- 18
ethiopia_df[ethiopia_df == 2] <- 19
ethiopia_df[ethiopia_df == 3] <- 20
ethiopia_df[ethiopia_df == 4] <- 21
ethiopia_df[ethiopia_df == 5] <- 22
ethiopia_df[ethiopia_df == 6] <- 23
names(ethiopia_df)[1] <- "sex"
names(ethiopia_df)[2] <- "year"
names(ethiopia_df)[3] <- "adm1"
names(ethiopia_df)[4] <- "number_of_migrants"

ggplot(ethiopia_df, aes(x = year , y=number_of_migrants)) +
  geom_bar(
    aes(color = sex, fill = sex),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + facet_grid(.~adm1) + theme(panel.background = element_rect(fill = "#E2E9F4"))+scale_fill_manual(values = c("Female" = "#A30A8B", "Male" = "#0A3CA3")) + theme(
    plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
    axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
    axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
    axis.text.y = element_text(size = 14))

