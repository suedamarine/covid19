library(tidyverse)

# read from file on internet

covid_mort_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid_morts <- read_csv(covid_mort_url)

# filter for South Korea, Italy, Spain, UK, Germany, Portugal, Sweden
covid_morts_filtered <- covid_morts %>%
  filter(`Country/Region` %in% c("Italy", "Spain", "United Kingdom", "Germany", "Portugal", "Sweden"))   
head(covid_morts_filtered)


# remove UK province/states in preparation for plot
covid_morts_filtered <- covid_morts_filtered %>%
  filter(is.na(`Province/State`)) %>%
  subset(select = -`Province/State`) %>%
  gather(date, morts, -`Country/Region`, -Lat,- Long) %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
head(covid_morts_filtered)

# plot 

covid_morts_filtered %>%
  ggplot(aes(date, morts, color = `Country/Region`)) + geom_point() + scale_y_log10(limits = c(100, NA)) + stat_smooth() + scale_x_date(breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B", limits = as.Date(c("0020-03-20", "0020-05-20")) )


# create morts with province na

covid_morts_na <- covid_morts %>%
  filter(`Country/Region` %in% c("Italy", "Spain", "United Kingdom", "Germany", "Portugal", "Sweden")) %>%
  filter(is.na(`Province/State`)) %>%
  subset(select = -`Province/State`)
head(covid_morts_na)

# get length of morts
morts_length <- length(covid_cases_na)

daily_morts <- cases_length - 3

# define country daily cases
Germany_morts<- as.numeric(as.vector(covid_morts_na[1,4:cases_length]))
Italy_morts <- as.numeric(as.vector(covid_morts_na[2,4:cases_length]))
Portugal_morts <- as.numeric(as.vector(covid_morts_na[3,4:cases_length]))
Spain_morts <- as.numeric(as.vector(covid_morts_na[4,4:cases_length]))
United_Kingdon_morts <- as.numeric(as.vector(covid_morts_na[6,4:cases_length]))
Sweden_morts <- as.numeric(as.vector(covid_morts_na[5,4:cases_length]))
series_date <- seq(as.Date("2020/1/22"), by = "day", length.out = daily_morts-1)

#create date frame for case rate
# calculate daily rates
Germany_mrate <- (Germany_morts[2:daily_morts]-Germany_morts[1:daily_morts-1])/Germany_morts[1:daily_morts-1]

Italy_mrate <- (Italy_morts[2:daily_morts]-Italy_morts[1:daily_morts-1])/Italy_morts[1:daily_morts-1]

Portugal_mrate <- (Portugal_morts[2:daily_morts]-Portugal_morts[1:daily_morts-1])/Portugal_morts[1:daily_morts-1]

Spain_mrate <- (Spain_morts[2:daily_morts]-Spain_morts[1:daily_morts-1])/Spain_morts[1:daily_morts-1]

UK_mrate <- (United_Kingdon_morts[2:daily_morts]-United_Kingdon_morts[1:daily_morts-1])/United_Kingdon_morts[1:daily_morts-1]

Sweden_mrate <- (Sweden_morts[2:daily_morts]-Sweden_morts[1:daily_morts-1])/Sweden_morts[1:daily_morts-1]

# a series of daily mortality numbers
Germany_daily <- (Germany_morts[2:daily_morts]-Germany_morts[1:daily_morts-1])
Italy_daily <- (Italy_morts[2:daily_morts]-Italy_morts[1:daily_morts-1])
Portugal_daily <- (Portugal_morts[2:daily_morts]-Portugal_morts[1:daily_morts-1])
Spain_daily <- (Spain_morts[2:daily_morts]-Spain_morts[1:daily_morts-1])
UK_daily <- (United_Kingdon_morts[2:daily_morts]-United_Kingdon_morts[1:daily_morts-1])
Sweden_daily <- (Sweden_morts[2:daily_morts]-Sweden_morts[1:daily_morts-1])
daily_numbers_df <- data.frame(series_date, Germany_daily, Italy_daily, Portugal_daily, Spain_daily, UK_daily, Sweden_daily)
daily_numbers_tidy <- daily_numbers_df %>%
  gather(country, daily_mortality, 'Germany_daily':'Sweden_daily')
daily_numbers_tidy %>%
  ggplot(aes(series_date, daily_mortality, color = country)) + geom_point() + geom_smooth()  + scale_x_date(limits = c(min, max))

library(zoo)

daily_numbers_tidy %>%
  mutate(daily_mortality_roll = rollmean(daily_mortality, 7, na.pad = TRUE)) %>%
  ggplot(aes(series_date, daily_mortality_roll, color = country)) + geom_point() + geom_smooth()  + scale_x_date(limits = c(min, max))

# create date frame for mort rate

mort_rate_df <- data.frame(series_date, Germany_mrate, Italy_mrate, Portugal_mrate, Spain_mrate, UK_mrate, Sweden_mrate)

# tidy df
mort_rate_tidy <- mort_rate_df %>%
  gather(country, rate_change, 'Germany_mrate': 'Sweden_mrate')

head(mort_rate_tidy)
# set max min for x axis
min <-as.Date("2020/2/21")
max <- NA


# plot data
mort_rate_tidy %>%
  ggplot(aes(series_date, rate_change, color = country)) + geom_point() + geom_smooth() + ylim(0,0.4) + scale_x_date(limits = c(min, max))

# plot data
mort_rate_tidy %>% filter(country %in% c("Italy_mrate", "UK_mrate", "Germany_mrate")) %>%
  ggplot(aes(series_date, rate_change, color = country)) + geom_point() + geom_smooth() + ylim(0,0.4) + scale_x_date(limits = c(min, max))
library(zoo)

# plot data with rolling mean over x days
mort_rate_tidy %>% 
  filter(country %in% c("Italy_mrate", "UK_mrate", "Spain_mrate", "Sweden_mrate")) %>%
  mutate(rate_change_roll = rollmean(rate_change, 3, na.pad = TRUE)) %>%
  ggplot(aes(series_date, rate_change_roll, color = country)) + geom_point() + geom_smooth() + ylim(0,0.4) + scale_x_date(limits = c(min, max))


# plot data
mort_rate_tidy %>% 
  mutate(rate_change_roll = rollmean(rate_change, 7, na.pad = TRUE)) %>%
  ggplot(aes(series_date, rate_change_roll, color = country)) + geom_point() + geom_smooth() + ylim(0,0.4) + scale_x_date(limits = c(min, max))

