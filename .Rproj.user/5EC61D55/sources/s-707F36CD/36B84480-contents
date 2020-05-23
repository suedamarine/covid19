# animated map to indicate mort rate across globe

library(tidyverse)
library(plotly)

# read from file on internet

covid_mort_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid_morts <- read_csv(covid_mort_url)

# filter for South Korea, Italy, Spain, UK, Germany, Portugal, Sweden
covid_morts_filtered <- covid_morts %>%
  filter(`Country/Region` %in% c("Italy", "Spain", "United Kingdom", "Germany", "Portugal", "Sweden"))   
head(covid_morts_filtered)


# remove UK province/states in preparation for plot
covid_morts_tidy <- covid_morts_filtered %>%
  filter(is.na(`Province/State`)) %>%
  subset(select = -`Province/State`) %>%
  gather(date, morts, -`Country/Region`, -Lat,- Long) %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))
head(covid_morts_tidy)

# mutate a column to indicate time from 1/22/20

covid_morts_age <- covid_morts_tidy %>%
  mutate(age_days = difftime(as.Date("1/22/20", format = "%m/%d/%Y"), date, unit = "days"))
tail(covid_morts_age)

# copy some code to generate a map

g <- list(showframe = FALSE,
          scope = 'europe',
          coastlinecolor = toRGB("white"),
          showland = TRUE,
          landcolor = toRGB("gray80"),
          showcountries = TRUE,
          countrycolor = toRGB("white"),
          countrywidth = 0.2,
          projection = list(type = 'Mercator'))

plot_geo(covid_morts_age,
         marker = list(color = toRGB("purple"),
                       opacity = 0.5,
                       line = list(color = toRGB("purple"),
                                   width = 1.5))) %>%
  add_markers(x = ~Long,
              y = ~Lat,
              sizes = c(1, 1000),
              size = ~morts,
              hoverinfo = "text",
              text = ~paste('Country: ', 'Country/Region',
                            '<br /> days: ', age_days,
                            '<br /> morts: ', morts)) %>%
  layout(geo = g)
