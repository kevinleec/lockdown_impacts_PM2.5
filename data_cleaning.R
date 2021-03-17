#######################################
## Define state regions
#######################################
NE.name <- c("connecticut","maine","massachusetts","new hampshire",
             "rhode island","vermont","new jersey","new york",
             "pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("indiana","illinois","michigan","ohio","wisconsin",
             "iowa","kansas","minnesota","missouri","nebraska",
             "north dakota","south dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("delaware","district of columbia", "florida","georgia",
            "maryland","north carolina","south carolina","virginia",
            "west virginia","alabama","kentucky","mississippi",
            "tennessee","arkansas","louisiana","oklahoma","texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("arizona","colorado","idaho","new mexico","montana",
            "utah","nevada","wyoming","alaska","california",
            "hawaii","oregon","washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

################################################################
## Data cleaning
################################################################

## 2010-2019 AQS Data
pm25 <- read.csv("pm25_daily_monitor_data.csv")
pm25$Date.Local <- as.Date(pm25$Date.Local, format = "%Y-%m-%d")
pm25 <- subset(pm25, Date.Local >= as.Date("2010-01-01") & Date.Local <= as.Date("2019-12-31"))
pm25clean <- aggregate(Arithmetic.Mean ~ State.Code + County.Code + Site.Num +
                         Parameter.Code + Latitude + Longitude + Date.Local +
                         Local.Site.Name + State.Name + County.Name + 
                         City.Name + CBSA.Name + Monitor,
                       data=pm25, FUN=mean)


#pm25clean$Site.Num <- formatC(pm25clean$Site.Num, width = 4, format = "d", flag = "0")
#pm25clean$full_aqs_id <- paste(pm25clean$State.Code, pm25clean$County.Code, 
#                               pm25clean$Site.Num, sep = "")

pm25clean$Monitor <- gsub('-', '', pm25clean$Monitor)
pm25clean$Monitor <- as.integer(pm25clean$Monitor)
pm25clean$full_aqs_id <- as.factor(sprintf("%09d", pm25clean$Monitor))


## Meteorological
weather <- read.csv("temp_daily_county.csv")
weather$COUNTYFP <- as.factor(weather$COUNTYFP)
weather$STATEFP <- as.factor(weather$STATEFP)
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")
pm25clean$County.Code <- as.factor(pm25clean$County.Code)

filenames_vs <- list.files(path = "wind_speed/", pattern = "*.csv")
fullpath_vs <- file.path("wind_speed", filenames_vs)
windspeed <- do.call("rbind", lapply(fullpath_vs, FUN = function(files) {read.csv(files)}))
windspeed$fips <- as.factor(sprintf("%05d", windspeed$fips))
windspeed$date <- as.Date(windspeed$date, format = "%Y-%m-%d")

filenames_th <- list.files(path = "wind_direction/", pattern = "*.csv")
fullpath_th <- file.path("wind_direction", filenames_th)
winddirection <- do.call("rbind", lapply(fullpath_th, FUN = function(files) {read.csv(files)}))
winddirection$fips <- as.factor(sprintf("%05d", winddirection$fips))
winddirection$date <- as.Date(winddirection$date, format = "%Y-%m-%d")


## 2020 AirNow Data
airnow <- read.csv("daily_pm_2020.csv")
colnames(airnow) <- c("full_aqs_id", "Date.Local", "Arithmetic.Mean", "State.ID", "fips", "zip", "Longitude", "Latitude")
airnow$Date.Local <- as.Date(airnow$Date.Local)
airnow <- subset(airnow, Date.Local >= as.Date("2020-01-01"))
airnow$full_aqs_id <- str_sub(airnow$full_aqs_id, start = -9)


## Merge data
full_data <- merge(pm25clean, airnow, by = c("full_aqs_id", "Date.Local", "Arithmetic.Mean"), all = TRUE)
drop <- c("State.ID", "zip", "Longitude.y", "Latitude.y")
full_data <- full_data[, !names(full_data) %in% drop]
full_data <- full_data %>% group_by(full_aqs_id)
full_data <- na.locf(full_data)
full_data$fips <- str_extract(full_data$full_aqs_id, "^.{5}")

## Merge w/ confounders
pm25_with_conf <- merge(full_data, weather, by.x = c("State.Code", "County.Code", "Date.Local"), by.y = c("STATEFP", "COUNTYFP", "date"), all.x = TRUE)

## Day of Week
pm25_with_conf$day <- weekdays(as.Date(pm25_with_conf$Date.Local))
pm25_with_conf$month <- month(as.Date(pm25_with_conf$Date.Local))
pm25_with_conf$week <- isoweek(ymd(pm25_with_conf$Date.Local))
pm25_with_conf$week <- formatC(as.numeric(as.character(pm25_with_conf$week)), width = 2, format = "d", flag = "0")
pm25_with_conf$fips <- formatC(as.numeric(as.character(pm25_with_conf$fips)), width = 5, format = "d", flag = "0")

pm25_with_conf <- merge(pm25_with_conf, windspeed, by.x = c("Date.Local", "fips"), by.y = c("date", "fips"), all.x = TRUE)
pm25_with_conf <- merge(pm25_with_conf, winddirection, by.x = c("Date.Local", "fips"), by.y = c("date", "fips"), all.x = TRUE)

pm25_with_conf <- pm25_with_conf %>% 
  filter(!(as.numeric(week) == 53 & as.numeric(month) == 1)) %>%
  filter(!(as.numeric(week) == 52 & as.numeric(month) == 1)) %>%
  filter(!(as.numeric(week) == 1 & as.numeric(month) == 12))
pm25_with_conf <- pm25_with_conf %>% 
  mutate(Day.ID = case_when(
    day == "Monday" ~ 1,
    day == "Tuesday" ~ 2,
    day == "Wednesday" ~ 3,
    day == "Thursday" ~ 4,
    day == "Friday" ~ 5,
    day == "Saturday" ~ 6,
    day == "Sunday" ~ 7,
    TRUE ~ 8
  )
  )
pm25_with_conf$index <- paste(pm25_with_conf$week, pm25_with_conf$Day.ID, sep = "")
pm25_with_conf$year <- year(pm25_with_conf$Date.Local)
year(pm25_with_conf$Date.Local) <- 2020

## Weekday/Month indicators
pm25_with_conf <- pm25_with_conf %>%
  mutate(weekDay = day, value = 1) %>%
  spread(key = weekDay, value = value, fill = 0) %>%
  mutate(monthID = lubridate::month(Date.Local, label = T), value = 1) %>%
  spread(key = monthID, value = value, fill = 0)
pmdata <- subset(pm25_with_conf, Date.Local >= as.Date("2020-01-05") & Date.Local < as.Date("2020-04-30"))

## SOM Dates
som <- read.csv("state_of_emergency.csv")
som$State.of.emergency <- as.Date(som$State.of.emergency, format = "%m/%d/%y")
som$Reopen.businesses <- as.Date(som$Reopen.businesses, format = "%m/%d/%y")
som[42, "Reopen.businesses"] <- as.Date("2020-04-28")



#################################################
## Associated Factors (formatting data)
#################################################

################## Coal Contributions to PM2.5 by County ##################

coal_2016 <- read_fst("zips_pm25_total_2016.fst")
coal_2017 <- read_fst("zips_pm25_total_2017.fst")
coal_2018 <- read_fst("zips_pm25_total_2018.fst")

coal_avg <- merge(coal_2016, coal_2017, by = "ZIP", all = T) %>%
  merge(coal_2018, by = "ZIP", all = T)
coal_avg$mean <- rowMeans(subset(coal_avg, select = c(V3.x, V3.y, V3)))
coal_avg <- coal_avg[,-c(2,3,4)]

zip_to_county <- read.csv("zips_with_county.csv")
zip_to_county$ZIP <- as.factor(formatC(as.numeric(as.character(zip_to_county$ZIP)), width = 5, format = "d", flag = "0"))

coal <- merge(coal_avg, zip_to_county[, c("ZIP", "STATE_NAME", "fips", "county_name")], by = "ZIP", all.x = T)
coal <- aggregate(mean ~ STATE_NAME + fips + county_name, 
                  data = coal, FUN = mean)
coal$fips <- formatC(as.numeric(as.character(coal$fips)), width = 5, format = "d", flag = "0")

#################### Socioeconomic Data ####################

socioecon <- read.csv("census_county_uninterpolated.csv")
socioecon <- filter(socioecon, year == 2018)
socioecon$fips <- formatC(as.numeric(as.character(socioecon$fips)), width = 5, format = "d", flag = "0")
socioecon <- socioecon %>% dplyr::select(fips, NAME, year, white_pct, age_pct_65_plus, poverty,
                                         population, population_density, no_grad)

################## Movement Data ##################

mobility <- read.csv("mobility.csv")
mobility$fips <- as.factor(sprintf("%05d", mobility$fips))
mobility$relative_mobility_change <- -mobility$relative_mobility_change

################ Residential Wood Burning ################

wood <- read.csv("s.csv")
wood <- subset(wood, STATE != "AK" & STATE != "PR" & STATE != "HI" & STATE != "VI")
wood$COUNTY_FIPS <- formatC(as.numeric(as.character(wood$COUNTY_FIPS)), width = 3, format = "d", flag = "0")
wood$STATE_FIPS <- formatC(as.numeric(as.character(wood$STATE_FIPS)), width = 2, format = "d", flag = "0")
wood$fips <- paste(wood$STATE_FIPS, wood$COUNTY_FIPS, sep = "")

################ Misc Sources ################

misc_sources <- read.csv("misc_sources.csv")
misc_sources$SECTOR <- as.character(misc_sources$SECTOR)
misc_sources <- misc_sources %>%
  mutate(SECTOR = case_when(
    str_detect(SECTOR, "Agriculture -") ~ "agriculture",
    str_detect(SECTOR, "Dust -")  ~ "dust",
    str_detect(SECTOR, "Industrial Boilers") ~ "industrial_boilers",
    str_detect(SECTOR, "Residential") ~ "residential",
    str_detect(SECTOR, "Industrial Processes") ~ "industrial_processes",
    TRUE ~ SECTOR
  )
  )
misc_sources <- aggregate(data=misc_sources, EMISSIONS ~ SECTOR + STATE + STATE_FIPS + COUNTY + COUNTY_FIPS, FUN = sum)
misc_sources <- subset(misc_sources, STATE != "TR", STATE != "DM" & STATE != "AK" & STATE != "PR" & STATE != "HI" & STATE != "VI")
misc_sources$COUNTY_FIPS <- formatC(as.numeric(as.character(misc_sources$COUNTY_FIPS)), width = 3, format = "d", flag = "0")
misc_sources$STATE_FIPS <- formatC(as.numeric(as.character(misc_sources$STATE_FIPS)), width = 2, format = "d", flag = "0")
misc_sources$fips <- paste(misc_sources$STATE_FIPS, misc_sources$COUNTY_FIPS, sep = "")
misc_sources <- misc_sources %>% spread(SECTOR, EMISSIONS)

############### Mobile Sources ###############

mobile <- read.csv("mobile.csv")
mobile$SECTOR <- as.character(mobile$SECTOR)
mobile <- mobile %>%
  mutate(SECTOR = case_when(
    str_detect(SECTOR, "Mobile") ~ "mobile",
    TRUE ~ SECTOR
  )
  )
mobile <- aggregate(data=mobile, EMISSIONS ~ SECTOR + STATE + STATE_FIPS + COUNTY + COUNTY_FIPS, FUN = sum)
mobile <- subset(mobile, STATE != "TR", STATE != "DM" & STATE != "AK" & STATE != "PR" & STATE != "HI" & STATE != "VI")
mobile$COUNTY_FIPS <- formatC(as.numeric(as.character(mobile$COUNTY_FIPS)), width = 3, format = "d", flag = "0")
mobile$STATE_FIPS <- formatC(as.numeric(as.character(mobile$STATE_FIPS)), width = 2, format = "d", flag = "0")
mobile$fips <- paste(mobile$STATE_FIPS, mobile$COUNTY_FIPS, sep = "")
mobile <- mobile %>% spread(SECTOR, EMISSIONS)
mobile <- subset(mobile, select=-c(STATE,STATE_FIPS,COUNTY,COUNTY_FIPS))

############### Urban/Rural ###############

ur_codes <- read.csv("urcodes.csv")
ur_codes$fips <- formatC(as.numeric(as.character(ur_codes$FIPS.code)), width = 5, format = "d", flag = "0")
