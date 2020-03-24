# data source by Johns Hopkins University Center for Systems Science and Engineering

raw_covid19_confirmed <- readr::read_csv("https://bit.ly/covid19_confirmed")
raw_covid19_deaths <- readr::read_csv("http://bit.ly/covid19_deaths")
raw_covid19_recovered <- readr::read_csv("http://bit.ly/covid19-recovered")

covid19_cases <- function(data, value_name) {
  data %>%
    tidyr::pivot_longer(
      cols = 5:ncol(data),
      names_to = "date",
      values_to = value_name
    ) %>%
    dplyr::mutate(
      date = lubridate::mdy(date)
    ) %>%
    janitor::clean_names()
}

a <- covid19_cases(raw_covid19_confirmed, "confirmed")
b <- covid19_cases(raw_covid19_deaths, "deaths")
c <- covid19_cases(raw_covid19_recovered, "recovered")

covid19_outbreak <- a %>%
  merge(b) %>%
  merge(c)

latest_covid19 <- function(data) {
  data %>%
    group_by(province_state, country_region, lat, long) %>%
    filter(date == max(date), confirmed != 0) %>%
    summarise(
      confirmed = sum(confirmed),
      deaths = sum(deaths),
      recovered = sum(recovered)
    ) %>%
    arrange(-confirmed)
}


# head(covid19_outbreak %>% filter(date == max(date) - 1) )


new_cases_covid19 <- function(data) {
  data %>%
    group_by(province_state, country_region, lat, long) %>%
    filter(date == max(date) - 1) %>%
    summarise(
      confirmed = sum(confirmed),
      deaths = sum(deaths),
      recovered = sum(recovered)
    ) %>%
    arrange(-confirmed)
}

latest_covid19 <- latest_covid19(covid19_outbreak)
# 
# 
# raw_conf <- read.csv("http://bit.ly/covid19-confirmed", stringsAsFactors = FALSE, check.names=FALSE)
# raw_deaths <- read.csv("http://bit.ly/covid19-deaths", stringsAsFactors = FALSE, check.names=FALSE)
# raw_recov <- read.csv("http://bit.ly/covid19-recovered", stringsAsFactors = FALSE, check.names=FALSE)
# 
# df_conf <- raw_conf[, 1:4]
# df_deaths <- raw_deaths[1:4]
# df_recov <- raw_recov[1:4]

# daily_updates <- function(data, df, case_name) {
#   
#   for(i in 5:ncol(data)){
#     print(i)
#     data[,i] <- as.integer(data[,i])
#     data[,i] <- ifelse(is.na(data[, i]), 0 , data[, i])
#     
#     if(i == 5){
#       df[[names(data)[i]]] <- data[, i]
#     } else {
#       df[[names(data)[i]]] <- data[, i] - data[, i - 1]
#     }
#   }
#   
#   df <- covid19_cases(df, case_name)
#   return(df)
# }
# 
# df_conf <- daily_updates(raw_conf, df_conf, "confirmed")
# df_deaths <- daily_updates(raw_deaths, df_deaths, "deaths")
# df_recov <- daily_updates(raw_recov, df_recov, "recovered")
# 
# df_outbreak <- a %>% merge(b) %>% merge(c)
# 
# rm(raw_covid19_confirmed, raw_covid19_deaths, raw_covid19_recovered, a, b, c)

# readr::write_csv(covid19_outbreak, "covid19_outbreak.csv")
# readr::write_csv(latest_covid19, "latest_covid19.csv")

# # transforming elements in data
# covid19_confirmed <- raw_covid19_confirmed %>%
#   pivot_longer(cols = 5:ncol(raw_covid19_confirmed),
#                names_to = "date",
#                values_to = "confirmed") %>%
#   clean_names()
#
# covid19_deaths <- raw_covid19_deaths %>%
#   pivot_longer(cols = 5:ncol(raw_covid19_deaths),
#                names_to = "date",
#                values_to = "deaths") %>%
#   clean_names()
#
# covid19_recovered <- raw_covid19_recovered %>%
#   pivot_longer(cols = 5:ncol(raw_covid19_recovered),
#                names_to = "date",
#                values_to = "recovered") %>%
#   clean_names()

# # combine all data frames into one
#
# covid19_outbreak <- covid19_confirmed %>%
#   merge(covid19_deaths) %>%
#   merge(covid19_recovered)
#
# covid19_outbreak$date <- mdy(covid19_outbreak$date)


# remove unnecessary dataframes


#
#
#
# covid19_outbreak <- covid19_outbreak %>%
#   group_by(province_state, country_region, lat, long)
#
#
# latest_confirmed <- covid19_outbreak %>%
#   filter(status == "confirmed", date == max(date)) %>%
#   summarise(confirmed = sum(cases)) %>%
#   arrange(-confirmed)
#
# latest_deaths <- covid19_outbreak %>%
#   filter(status == "deaths", date == max(date)) %>%
#   summarise(deaths = sum(cases)) %>%
#   arrange(-deaths)
#
# latest_recovered <- covid19_outbreak %>%
#   filter(status == "recovered", date == max(date)) %>%
#   summarise(recovered = sum(cases)) %>%
#   arrange(-recovered)
#
# latest_outbreak <- latest_confirmed %>% merge(latest_deaths) %>%
#   merge(latest_recovered)
#
# rm(latest_confirmed, latest_deaths, latest_recovered, latest_data)
#
# write_csv(latest_outbreak, "latest_outbreak.csv")