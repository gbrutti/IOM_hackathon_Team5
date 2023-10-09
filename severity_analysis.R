library(ggplot2)

data <- read.csv("data.csv")
severity <- read.csv("clean_severity_14_11.csv")
data_table <- data.frame(table(data$sex, data$pdsi_max, data$reasons_for_migration_environmetal, data$ADM1_NAME, 
                               data$reasons_for_migration_access_to_services, data$year, 
                               data$drought, data$month))
names(data_table)[2] <- "drought_severity"
names(data_table)[7] <- "month"
names(data_table)[8] <- "drought"
names(data_table)[9] <- "number_of_migrants"
names(data_table)[1] <- "sex"
names(data_table)[3] <- "environmental_motive"
names(data_table)[5] <- "access_to_service_motive"
names(data_table)[4] <- "adm1_region"
names(data_table)[6] <- "year"
data_table$number_of_migrants <- as.numeric(data_table$number_of_migrants)
data_table$drought_severity <- as.numeric(as.character(data_table$drought_severity))
data_table <- data_table[data_table$number_of_migrants != 0 & data_table$environmental_motive == 1,]

ggplot(data_table, aes(x=drought_severity, y=number_of_migrants, color=sex)) + geom_point() + theme(panel.background = element_rect(fill = "#E2E9F4"))+scale_fill_manual(values = c("Female" = "#A30A8B", "Male" = "#0A3CA3")) + theme(
  plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
  axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
  axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
  axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
  axis.text.y = element_text(size = 14)) + geom_vline(xintercept = 0, linetype = 2, size = 0.5, color = "red") + facet_grid(.~sex) 

data_table_somali <- data_table[data_table$adm1_region == "Somali",]
ggplot(data_table_somali, aes(x=drought_severity, y=number_of_migrants, color = sex)) + geom_point() + geom_vline(xintercept = 0, linetype = 2, size = 0.5, color = "red") 

data_table_no_somali <- data_table[data_table$adm1_region != "Somali",]
ggplot(data_table_no_somali, aes(x=drought_severity, y=number_of_migrants, color=sex)) + geom_point()  + geom_vline(xintercept = 0, linetype = 2, size = 0.5, color = "red")

data_table$somali <- "Other"
data_table$somali[data_table$adm1_region == "Somali"] <- "Somali"
ggplot(data_table, aes(x=drought_severity, y=number_of_migrants, color = sex)) + geom_point() + geom_vline(xintercept = 0, linetype = 2, size = 0.5, color = "red") + facet_grid(.~somali) + theme(
  plot.title = element_text(size = 16, hjust = 0.5),  # Center-align the title
  axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
  axis.title.y = element_text(size = 16),  # Adjust the y-axis label size
  axis.text.x = element_text(size = 14),   # Adjust the x-axis tick label size
  axis.text.y = element_text(size = 14)) 

data_table_no_somali <- data_table[data_table$adm1_region != "Somali",]
ggplot(data_table_no_somali, aes(x=drought_severity, y=number_of_migrants, color=sex)) + geom_point()  + geom_vline(xintercept = 0, linetype = 2, size = 0.5, color = "red")

model <- lm(data_table$number_of_migrants ~ poly(data_table$drought_severity,2) + data_table$sex)
summary(model)

model_s <- lm(data_table_somali$number_of_migrants ~ poly(data_table_somali$drought_severity,2) + data_table_somali$sex)
summary(model_s)

model_no_s <- lm(data_table_no_somali$number_of_migrants ~ poly(data_table_no_somali$drought_severity,2) + data_table_no_somali$sex)
summary(model_no_s)

model_no_s <- lm(data_table_no_somali$number_of_migrants ~ data_table_no_somali$drought_severity + data_table_no_somali$sex)
summary(model_no_s)

glm <- glm(data$reasons_for_migration_environmetal ~ data$pdsi_max + data$sex)
summary(glm)

data_somali <- data[data$ADM1_NAME == "Somali",]
glm_s <- glm(data_somali$reasons_for_migration_environmetal ~ data_somali$pdsi_max + data_somali$sex)
summary(glm_s)

data_no_somali <- data[data$ADM1_NAME != "Somali",]
glm_no_s <- glm(data_no_somali$reasons_for_migration_environmetal ~ data_no_somali$pdsi_max + data_no_somali$sex)
summary(glm_no_s)

agg_severity <- data.frame(aggregate(severity$pdsi_max, by=list(severity$year,severity$ADM1_NAME), FUN=mean))
names(agg_severity)[1] <- "year"
names(agg_severity)[2] <- "adm1"
names(agg_severity)[3] <- "pdsi"
agg_severity[agg_severity == "2018"] <- 18
agg_severity[agg_severity == "2019"] <- 19
agg_severity[agg_severity == "2020"] <- 20
agg_severity[agg_severity == "2021"] <- 21
agg_severity[agg_severity == "2022"] <- 22
agg_severity[agg_severity == "2023"] <- 23

ggplot(agg_severity, aes(x=year, y=pdsi)) + geom_point() + geom_hline(yintercept = 0, linetype = 2, size = 0.5, color = "red") + facet_grid(.~adm1) 


severity_somali <- agg_severity[agg_severity$adm1 == "Somali",]
ggplot(severity_somali, aes(x=year, y=pdsi)) + geom_point() + geom_hline(yintercept = 0, linetype = 2, size = 0.5, color = "red") 

severity_no_somali <- agg_severity[agg_severity$adm1 != "Somali",]
ggplot(severity_no_somali, aes(x=year, y=pdsi)) + geom_point() + geom_hline(yintercept = 0, linetype = 2, size = 0.5, color = "red") + facet_grid(.~adm1)
