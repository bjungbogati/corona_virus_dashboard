

library(lubridate)

days <- yday(top_country_date$date) - 21 # so Jan 1 = day 0 
total_days <- cumsum(days)


top_country_date <- top_country_date %>% 
  group_by(country_region) %>% 
  mutate(day = yday(date) - 21)


head(top_country_date)

hchart(date_top_country, "line", hcaes(x = day_count, y = log10(Deaths), group = country_region))

colors <- c("#487098","#484898","#34348d","#19198b","#208582","#942f59","#dd125b","#000000","#c61051","#000FFF")


highchart() %>% 
  # Data
  hc_add_series(date_top_country, type = "line", 
                hcaes(x = day_count, y = round(log10(Deaths), 2), group = country_region,
                      z = country_region)) %>%
  
  hc_tooltip(headerFormat = "", pointFormat = "<b>Day:</b> {point.x} <br>
                           <b>Country:</b> {point.z} <br>
                           <br><b>Deaths:</b> {point.y} <br>")  %>% 
  
  hc_plotOptions(
    series = list(
      showInLegend = T,
      dataLabels = list(enabled = F)
    ),
    column = list(
      colorByPoint = TRUE
    )) %>%

  # Titles and credits
  hc_title(
    text = "Top 10 Countries by Deaths (Log Scale)"
  ) %>%
  hc_add_theme(hc_theme_darkunica()) 



