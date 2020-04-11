
pacman::p_load("rvest", "lubridate", "stringr", "flexdashboard", "tidyr", "scales", "readr", "dplyr", "highcharter", "janitor", "kableExtra", "knitr", "leaflet")

source("R/extract_data.R") 
source("R/web_scraper.R")
source("R/trends.R")

# latest_outbreak <- readr::read_csv("latest_covid19.csv")
# covid19_outbreak <- readr::read_csv("covid19_outbreak.csv")

m <- latest_covid19 %>%
  group_by(country_region) %>% 
  summarise(Confirmed = sum(confirmed), 
            Deaths = sum(deaths), 
            Recovered = sum(recovered), 
            Updated = Sys.Date() - 1) %>% 
  arrange(-Confirmed)

write_csv(m, "data/covid19-latest-country-wise.csv")



# hchart(top_country_date, "line", hcaes(x = date, y = Deaths, group = country_region))
# 
# highchart() %>% 
#   hc_chart(type = "line") %>% 
#   hc_add_series(data = top_deaths, group = top_deaths$country_region, 
#                 values = top_deaths$cases)


# m1 <- m %>% mutate(Confirmed = comma(Confirmed), 
#          Deaths = comma(Deaths), 
#          Recovered = comma(Recovered))

n <- covid19_outbreak %>% 
   group_by(date) %>% 
   summarise(confirmed = sum(confirmed), 
            deaths = sum(deaths), 
            recovered = sum(recovered), 
            ) %>% 
  arrange(confirmed)

u <- n %>% pivot_longer(cols = 2:4, 
                   names_to = "case_type", 
                   values_to = "case_num")
# u <- u  %>% 
#   mutate(dates = lubridate::mdy(date))


u$date <- format(u$date, format="%b %d %Y")


# country_wise_cases_summary <- country_wise_cases_summary %>% 
#   mutate(dates = lubridate::mdy(date))

readr::write_csv(u, "data/covid19-latest-case-wise.csv")
readr::write_csv(total_cases_wise_summary, "./data/covid19_cases_wise_summary.csv") #wescraper.csv

only_deaths <- latest_covid19 %>%
  filter(deaths != 0)
        
only_recov <- latest_covid19 %>% 
 filter(recovered != 0) 

mytext <- function(data) { 
                paste("<b>",
                if_else(is.na(data$province_state), 
                        data$country_region, 
                        paste0(data$province_state, " : ", 
                               data$country_region), "</b>"), "<br/>", 
                "<b style='color: #bf4119;'> Confirmed: </b>", data$confirmed, "<br/>", 
                "<b style='color: #006400;'> Recovered: </b>", data$recovered, "<br/>", 
                "<b style='color: #333;'> Deaths: </b>", data$deaths, sep="") %>%
  lapply(htmltools::HTML)
  
}

con_text <- mytext(latest_covid19)
deaths_text <- mytext(only_deaths)
recov_text <- mytext(only_recov)


# 
# mean(latest_covid19$long)
# mean(latest_covid19$lat)

map_corona <- leaflet(options = leafletOptions(attributionControl=F)) %>%
  setView(lng = 26.834559, lat = 26.3698814, zoom = 2.4) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircleMarkers(
    lng=latest_covid19$long, lat= latest_covid19$lat, 
    radius = ifelse(latest_covid19$confirmed > 50, sqrt(latest_covid19$confirmed)/10, 
                    ifelse(latest_covid19$confirmed > 10, 3, 2)),
    stroke = FALSE, fillOpacity = 0.5, label = con_text, color = "#e94f20",
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"), 
                                textsize = "13px", 
                                direction = "auto",
                                background = "#222"), 
    group = "Confirmed"
  ) %>% 
   
    addCircleMarkers(
      lng= only_recov$long, lat= only_recov$lat,
      radius = ifelse(only_recov$recovered > 50, sqrt(only_recov$recovered)/10,
                      ifelse(only_recov$recovered > 10, 3, 2)),
      stroke = FALSE, fillOpacity = 0.5, label = recov_text, color = "#6da700",
      labelOptions = labelOptions(style = list("font-weight" = "normal",
                                               padding = "3px 8px"),
                                  textsize = "13px",
                                  direction = "auto",
                                  background = "#222"),
      group = "Recovered"
    ) %>%
    addCircleMarkers(
    lng= only_deaths$long, lat= only_deaths$lat, 
    radius = ifelse(only_deaths$deaths > 30, sqrt(only_deaths$deaths)/10, 
                    ifelse(only_deaths$deaths > 5, 3, 2)),
    stroke = FALSE, fillOpacity = 0.5, label = deaths_text, color = "#bdbdbd",
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"), 
                                textsize = "13px", 
                                direction = "auto",
                                background = "#222"), 
    group = "Deaths"
  ) %>% 
    addLayersControl(
    baseGroups = c("Confirmed", "Recovered", "Deaths"),
    options = layersControlOptions(collapsed = F)
  ) 

# 
# map_corona(latest_covid19, , "", mytext)
# 
# map_corona(latest_covid19, , "", mytext)


## modeling for trend

dat <- n %>% 
  select(-deaths, 
         -recovered
         ) %>% 
  arrange(desc(date))

x <- rev(1:nrow(dat))
cases <- dat$confirmed
model <- nls(cases ~ (x ^ b), start = c(b = 2), trace = T)


predict_days <- 10

x <- rev(1:(nrow(dat) + predict_days))
pred <- x ^ coef(model)
dates <- seq.Date(min(dat$date), max(dat$date) + predict_days, by = "days")

model_df <- data.frame(
  date = rev(dates),
  cases = c(rep(NA, predict_days), cases),
  model = floor(pred)
) %>% 
  arrange(date)

covid19_cases_trend <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = model_df$date) %>%
  hc_add_series(name = "Confirmed", data = model_df$cases) %>%
  hc_add_series(name = "Prediction", data = model_df$model) %>% 
  hc_title(text = "Covid19 Cases by Trend") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_colors(c("#da3f11","#f7a91f")) %>% 
  hc_tooltip(table = TRUE, sort = TRUE)



covid19_cases_date <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = n$date) %>%
  hc_add_series(name = "Confirmed", data = n$confirmed) %>%
  hc_add_series(name = "Recovered", data = n$recovered) %>%
  hc_add_series(name = "Deaths", data = n$deaths) %>%
  hc_title(text = "Covid19 Cases by Date") %>%
  hc_plotOptions(series = list(showInLegend = T,
                               dataLabels = list(enabled = F), 
                               marker = list(enabled = F))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_colors(c("#da3f11", "#6da700", "#bdbdbd")) %>%  
  hc_tooltip(table = TRUE, sort = TRUE)


graph_df <- function(data, graph_type) {
  highchart() %>%
  hc_chart(type = graph_type) %>%
  hc_xAxis(categories = data$country_region) %>%
  hc_add_series(name = "Confirmed", data = data$Confirmed) %>%
  hc_add_series(name = "Recovered", data = data$Recovered) %>%
  hc_add_series(name = "Deaths", data = data$Deaths) %>%
  hc_plotOptions(series = list(showInLegend = T,
                dataLabels = list(enabled = F), 
                marker = list(enabled = F)),
               column = list( colorByPoint = TRUE)) %>%
  hc_title(text = "Top Countries by Cases") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_colors(c("#da3f11", "#6da700", "#bdbdbd")) %>% 
  hc_tooltip(table = TRUE, sort = TRUE) 
}

top_countries <- head(m, 13)[, ] 

country_wise_cases_summary <- top_countries %>% select(-Updated) %>% 
                               pivot_longer(cols = 2:4, 
                               names_to = "case_type", 
                               values_to = "case_num")



readr::write_csv(country_wise_cases_summary, "./data/country_wise_cases_summary")

country_df <- function(data, graph_type) {
  highchart() %>%
    hc_chart(type = graph_type) %>%
    hc_xAxis(categories = data$country_region) %>%
    hc_add_series(name = "Confirmed", data = data$Confirmed) %>%
    hc_add_series(name = "Recovered", data = data$Recovered) %>%
    hc_add_series(name = "Deaths", data = data$Deaths) %>%
    hc_title(text = paste("Top", nrow(data), "China vs Other Countries") ) %>%
    hc_add_theme(hc_theme_darkunica()) %>%
    hc_tooltip(table = TRUE, sort = TRUE) %>% 
    hc_colors(c("#da3f11","#6da700", "#bdbdbd"))
}



# China vs Rest 5 countries

country_date <- covid19_outbreak %>%  
  group_by(country_region, date) %>% 
  summarise( Confirmed = sum(confirmed), 
             Deaths = sum(deaths),
             Recovered = sum(recovered)
             ) %>% 
  arrange(-Confirmed) 
  

# Top 10 countries with Deaths






## top 10 countries

# top_countries_graph <- graph_df(top_countries, "column")


