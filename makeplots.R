library(tidyverse)
library(ggplot2)
library(lubridate)

# Daily Summaries 
data_files <- list.files('data/daily_reports/', pattern="\\.csv", full.names= TRUE)
zoom_usage <- lapply(data_files, read.csv, header=TRUE)

# Modify date columns
combined_usage <- do.call(rbind, zoom_usage)
combined_usage$Date <- ymd(combined_usage$Date)
combined_usage$month <- month(combined_usage$Date)
combined_usage$day <- day(combined_usage$Date)
combined_usage$quarter <- quarter(combined_usage$Date)
combined_usage$year <- year(combined_usage$Date)

combined_usage_monthly <- combined_usage %>% 
  group_by(month, year) %>%
  summarise(total_participants = sum(Participants), total_minutes = sum(Meeting.Minutes))
  

combined_usage_monthly$Date <- as.Date(strptime(paste(combined_usage_monthly$year,'1',combined_usage_monthly$month, sep="-"), format="%Y-%d-%m"))

# Total endpoints 
ggplot(combined_usage_monthly, aes(x=Date, y=total_participants), color=as.factor(year) ) + geom_line()

# Person Hours
ggplot(combined_usage_monthly, aes(x=Date, y=total_minutes/60/24), color=as.factor(year) ) + geom_line()

# Monthly by Country

monthly_by_country_csv <- list.files('data/monthly_by_country/', pattern="\\.csv", full.names=TRUE)

data <- monthly_by_country_csv %>% data_frame(filename=.) %>%
  mutate(file_contents= map(filename, ~read_csv(.)))
data <- unnest(data)
data$Date <- parse_date(substr(data$filename,57,63), "%Y-%m")

a <- data %>% group_by(Date, Location) %>% summarise(n = n())  %>% mutate(pct = n / sum(n) ) 

monthly_by_country <- sapply(monthly_by_country_csv, read.csv, simplify=FALSE)
foo <- bind_rows(monthly_by_country, id="id")

all_countries <- a %>% group_by(Location) %>% summarise(sum= sum(`Meeting Participants`)) %>% arrange(desc(sum)) %>% filter(sum >= 10)
ggplot(all_countries, aes(y=sum, x=reorder(Location, sum) )) + geom_bar(stat="identity")  + coord_flip() + ggtitle("Video Call Participants by Country (Sep 2017-Sep 2018)")

