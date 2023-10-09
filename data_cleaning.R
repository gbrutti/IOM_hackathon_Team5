# load data
clean <- read.csv("cleaned2_FMSdata.csv") #flow data 
severity <- read.csv("clean_severity_14_11.csv") #drought severity data 

# correct inconsistencies in the data
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Gambela"] <- "Gambella"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Harari"] <- "Hareri"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Beneshangul Gumuz"] <- "Beneshangul Gumu"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Benishangul Gumuz"] <- "Beneshangul Gumu"
clean$departed_state_region_admin1[clean$departed_state_region_admin1 == "Beneshangul Gumz"] <- "Beneshangul Gumu"
severity$ADM1_NAME[severity$ADM1_NAME == "SNNPR"] <- "SNNP"
severity$ADM1_NAME[severity$ADM1_NAME == "Gambela"] <- "Gambella"

# rename column in data sets so that they are consistent
names(clean)[22] <- "ADM1_NAME"
names(clean)[54] <- "year_survey"
names(clean)[56] <- "year"
names(clean)[57] <- "month"
clean$year <- as.numeric(clean$year)
severity$year <- as.numeric(severity$year)
clean$month <- as.numeric(clean$month)
severity$month <- as.numeric(severity$month)
severity <- severity[, c(2, 6, 13, 14, 16)]
severity$drought <- 0 
severity$drought[severity$pdsi_max >= 0] <- 1

# merge the data sets by common administrative region name, year and month
data <- merge(clean, severity, by = c("ADM1_NAME", "year", "month"), all=TRUE)
data <- unique(data)

# omit respondents that did not answer or did not want to answer
data <- data[data$sex != "dont want to answer" & data$sex != "No Answer" ,]
write.csv(data, "data.csv")

