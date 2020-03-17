library(coronavirus)

data("coronavirus")

max_date <- max(coronavirus$date)

max_date 

coronavirus %>% 
  dplyr::filter(type == "confirmed", date == max_date) %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(total_cases = sum(cases)) %>%
  dplyr::arrange(-total_cases) %>%
  dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
  dplyr::ungroup() %>%
  dplyr::top_n(n = 15, wt = total_cases) %>%
  plotly::plot_ly(x = ~ country,
                  y = ~ total_cases,
                  text = ~ total_cases,
                  textposition = 'auto',
                  type = "bar") %>%
  plotly::layout(yaxis = list(title = "Number of Cases"),
                 xaxis = list(title = ""),
                 margin =  list(
                   l = 10,
                   r = 10,
                   b = 10,
                   t = 10,
                   pad = 2
                 ))


# raw_conf <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
#                      stringsAsFactors = FALSE)

df <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                     stringsAsFactors = FALSE, check.names=FALSE)





df[5:ncol(df)] <- df[5:ncol(df)]-df[,5]




# # Fixing typo
raw_conf$X2.6.20[which(raw_conf$Country.Region == "Japan")] <- 25


# Transforming the data from wide to long
# Creating new data frame
df_conf <- raw_conf[, 1:4]

daily_updates <- function(data, df) {

for(i in 5:ncol(data)){
  print(i)
  data[,i] <- as.integer(data[,i])
  data[,i] <- ifelse(is.na(data[, i]), 0 , data[, i])
  
  if(i == 5){
    df[[names(data)[i]]] <- data[, i]
  } else {
    df[[names(data)[i]]] <- data[, i] - data[, i - 1]
  }
}
  
  return(df)
  
}

dd <- daily_updates(raw_conf, df_conf)

df_conf[[names(raw_conf)[5]]]




df_conf1 <-  df_conf %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                             names_to = "date_temp",
                                             values_to = "cases_temp")

# Parsing the date
df_conf1$month <- sub("X", "",
                      strsplit(df_conf1$date_temp, split = "\\.") %>%
                        purrr::map_chr(~.x[1]) )

df_conf1$day <- strsplit(df_conf1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_conf1$date <- as.Date(paste("2020", df_conf1$month, df_conf1$day, sep = "-"))

# Aggregate the data to daily
df_conf2 <- df_conf1 %>%
  dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "confirmed",
                Country.Region = trimws(Country.Region),
                Province.State = trimws(Province.State))

head(df_conf2)



diff <- data.frame(a = c(0, 10, 15, 20, 25, 30, 40), 
                   b = c(10, 15, 20, 25, 30, 35, 45), 
                   c = c(15, 20, 25, 30, 35, 40, 50), 
                   d = c(20, 25, 30, 35, 40, 45, 55))



diff[, 2]

2:ncol(diff)

for(i in 2:ncol(diff)){
  
  # diff$a[i] <- diff$a[i+1] - diff$a[i]
  
  diff[, i] <- diff[, i] - diff[, i-1]

}

diff

diff[, 2] <- diff[, 2] - diff[, 2-1]

diff[, 3] <- diff[, 3] - diff[, 3-1]

diff[, 4] <- diff[, 4] - diff[, 4-1]


diff

