

library(lubridate)

days <- yday(top_country_date$date) - 21 # so Jan 1 = day 0 
total_days <- cumsum(days)


