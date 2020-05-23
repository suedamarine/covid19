library(tidyverse)

# read from file on internet
covid_cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid_cases <- read_csv(covid_cases_url)
head(covid_cases)

# filter for South Korea, Italy, Spain, UK, Germany, Portugal, Sweden
covid_cases_filtered <- covid_cases %>%
  filter(`Country/Region` %in% c("Italy", "Spain", "United Kingdom", "Germany", "Portugal", "Sweden"))   
head(covid_cases_filtered)

covid_cases_filtered <- covid_cases_filtered %>%
  filter(is.na(`Province/State`)) %>%
  subset(select = -`Province/State`) %>%
  gather(date, cases, -`Country/Region`, -Lat,- Long) %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
head(covid_cases_filtered)


covid_cases_filtered %>%
ggplot(aes(date, cases, color = `Country/Region`)) + geom_point() + scale_y_continuous(trans = "log10") + stat_smooth() +scale_x_date(breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B" )

covid_cases_filtered %>%
ggplot(aes(date, cases, color = `Country/Region`)) + geom_point() + scale_y_log10(limits = c(100, NA)) + stat_smooth() +scale_x_date(breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B" ) 

# create cases with province na

covid_cases_na <- covid_cases %>%
  filter(`Country/Region` %in% c("Italy", "Spain", "United Kingdom", "Germany", "Portugal", "Sweden")) %>%
  filter(is.na(`Province/State`)) %>%
  subset(select = -`Province/State`)
head(covid_cases_na)

# get length of cases
cases_length <- length(covid_cases_na)

daily_cases <- cases_length - 3

# define country daily cases
Germany_cases<- as.numeric(as.vector(covid_cases_na[1,4:cases_length]))
Italy_cases <- as.numeric(as.vector(covid_cases_na[2,4:cases_length]))
Portugal_cases <- as.numeric(as.vector(covid_cases_na[3,4:cases_length]))
Spain_cases <- as.numeric(as.vector(covid_cases_na[4,4:cases_length]))
United_Kingdon_cases <- as.numeric(as.vector(covid_cases_na[6,4:cases_length]))
Sweden_cases <- as.numeric(as.vector(covid_cases_na[5,4:cases_length]))
series_date <- seq(as.Date("2020/1/22"), by = "day", length.out = daily_cases-1)

#create date frame for case rate
# calculate daily rates
Germany_rate <- (Germany_cases[2:daily_cases]-Germany_cases[1:daily_cases-1])/Germany_cases[1:daily_cases-1]

Italy_rate <- (Italy_cases[2:daily_cases]-Italy_cases[1:daily_cases-1])/Italy_cases[1:daily_cases-1]

Portugal_rate <- (Portugal_cases[2:daily_cases]-Portugal_cases[1:daily_cases-1])/Portugal_cases[1:daily_cases-1]

Spain_rate <- (Spain_cases[2:daily_cases]-Spain_cases[1:daily_cases-1])/Spain_cases[1:daily_cases-1]

UK_rate <- (United_Kingdon_cases[2:daily_cases]-United_Kingdon_cases[1:daily_cases-1])/United_Kingdon_cases[1:daily_cases-1]

Sweden_rate <- (Sweden_cases[2:daily_cases]-Sweden_cases[1:daily_cases-1])/Sweden_cases[1:daily_cases-1]
# create date frame for case rate

case_rate_df <- data.frame(series_date, Germany_rate, Italy_rate, Portugal_rate, Spain_rate, UK_rate, Sweden_rate)

# set date limits

min <-as.Date("2020/3/1")
max <- NA

case_rate_tidy <- case_rate_df %>%
  gather(country, rate_change, 'Germany_rate': 'Sweden_rate') %>%
  ggplot(aes(series_date, rate_change, color = country)) + geom_point() + geom_smooth() + ylim(0,0.4) + scale_x_date(limits = c(min, max))

 case_rate_tidy
  
 Germany_daily_r <- (Germany_cases[2:daily_cases]-Germany_cases[1:daily_cases-1])
 Italy_daily_r <- (Italy_cases[2:daily_cases]-Italy_cases[1:daily_cases-1])
 Portugal_daily_r <- (Portugal_cases[2:daily_cases]-Portugal_cases[1:daily_cases-1])
 Spain_daily_r <- (Spain_cases[2:daily_cases]-Spain_cases[1:daily_cases-1])
 UK_daily_r <- (United_Kingdon_cases[2:daily_cases]-United_Kingdon_cases[1:daily_cases-1])
 Sweden_daily_r <- (Sweden_cases[2:daily_cases]-Sweden_cases[1:daily_cases-1])
 daily_numbers_r_df <- data.frame(series_date, Germany_daily_r, Italy_daily_r, Portugal_daily_r, Spain_daily_r, UK_daily_r, Sweden_daily_r)
 daily_numbers_r_tidy <- daily_numbers_r_df %>%
   gather(country, daily_rate, 'Germany_daily_r':'Sweden_daily_r') %>%
   filter(daily_rate >= 0)
 daily_numbers_r_tidy %>%
   ggplot(aes(series_date, daily_rate, color = country)) + geom_point() + geom_smooth()  + scale_x_date(limits = c(min, max)) + scale_y_log10(limits = c(100, 5000))
 tail(daily_numbers_r_tidy)








