top_country_date <- covid19_outbreak %>% 
  filter(deaths > 10) %>% 
  group_by(date, country_region) %>% 
  summarise(Confirmed = sum(confirmed), 
            Deaths = sum(deaths)) %>% 
  top_n(10, Deaths) %>% 
  arrange(Deaths)

# top_deaths <- top_country_date %>% 
#   tidyr::pivot_longer(
#     cols = 3:5,
#     names_to = "type",
#     values_to = "cases"
#   ) %>% 
#   filter(type == "Deaths")

library(lubridate)

days <- yday(top_country_date$date) - 21 # so Jan 1 = day 0 
# total_days <- cumsum(days)


top_country_date <- top_country_date %>% 
  group_by(country_region) %>% 
  mutate(day = yday(date) - 21)

date_top_country <- top_country_date %>%
  mutate(
    day_count = case_when(
      country_region == "China" ~ yday(date) - yday(min(date)) + 1,
      country_region == "Iran" ~ yday(date) - yday(min(date)) + 1,
      country_region == "Korea, South"  ~ yday(date) - yday(min(date)) + 1,
      country_region == "Italy" ~ yday(date) - yday(min(date)) + 1,
      country_region == "US" ~ yday(date) - yday(min(date)) + 1,
      country_region == "France"~ yday(date) - yday(min(date)) + 1,
      country_region == "Spain" ~ yday(date) - yday(min(date)) + 1,
      country_region == "Japan"  ~ yday(date) - yday(min(date)) + 1,
      country_region == "Switzerland"  ~ yday(date) - yday(min(date)) + 1,
      country_region == "United Kingdom" ~ yday(date) - yday(min(date)) + 1,
      country_region == "Netherlands" ~ yday(date) - yday(min(date)) + 1,
      country_region == "Germany" ~ yday(date) - yday(min(date)) + 1
    )
  )

hchart(date_top_country, "line", hcaes(x = day_count, y = log10(Deaths), group = country_region))

colors <- c("#487098","#484898","#34348d","#19198b","#208582","#942f59","#dd125b","#000000","#c61051","#000FFF")


death_trend <-  { highchart() %>% 
  hc_add_series(date_top_country, type = "line", 
                hcaes(x = day_count, y = round(log10(Deaths), 2), group = country_region,
                      z = country_region)) %>%
  
  hc_tooltip(headerFormat = "", pointFormat = "<b>Day:</b> {point.x} <br>
                           <b>Country:</b> {point.z} <br>
                           <br><b>Deaths:</b> {point.y} <br>")  %>% 
  
  hc_plotOptions(
    series = list(
      showInLegend = T,
      dataLabels = list(enabled = F), 
      marker = list(enabled = F)
    ),
    column = list(
      colorByPoint = TRUE
    )) %>%
  
  # Titles and credits
  hc_title(
    text = "Top Countries by Deaths (Log Scale)"
  ) %>%
  hc_add_theme(hc_theme_darkunica()) 
  
}



