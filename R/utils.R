
pacman::p_load("flexdashboard", "tidyr", "readr", "dplyr", "highcharter", "janitor", "kableExtra", "knitr", "leaflet")

source("R/extract_data.R") 

# latest_outbreak <- readr::read_csv("latest_covid19.csv")
# covid19_outbreak <- readr::read_csv("covid19_outbreak.csv")

m <- latest_covid19 %>%
  group_by(country_region) %>% 
  summarise( Confirmed = sum(confirmed), 
            Deaths = sum(deaths), 
            Recovered = sum(recovered)) %>% 
  arrange(-Confirmed) 

n <- covid19_outbreak %>% 
   group_by(date) %>% 
   summarise(confirmed = sum(confirmed), 
            deaths = sum(deaths), 
            recovered = sum(recovered)) %>% 
  arrange(confirmed)

only_deaths <- latest_covid19 %>% 
  filter(deaths != 0) 
        
only_recov <- latest_covid19 %>% 
  filter(recovered != 0) 


covid19_cases_date <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = n$date) %>%
  hc_add_series(name = "Confirmed", data = n$confirmed) %>%
  hc_add_series(name = "Recovered", data = n$recovered) %>%
  hc_add_series(name = "Deaths", data = n$deaths) %>%
  hc_title(text = "Covid19 Cases by Date") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_colors(c("#b9815b","#62a758", "#b53f40")) %>% 
  hc_tooltip(table = TRUE, sort = TRUE)

mytext <- function(data) {
  
  paste("<b>",
                if_else(is.na(data$province_state), 
                        data$country_region, 
                        paste0(data$province_state, " : ", 
                               data$country_region), "</b>"), "<br/>", 
                "<b style='color: #5f4826;'> Confirmed: </b>", data$confirmed, "<br/>", 
                "<b style='color: #672223;'> Deaths: </b>", data$deaths, "<br/>", 
                "<b style='color: #31542c;'> Recovered: </b>", data$recovered, sep="") %>%
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
    radius = ifelse(latest_covid19$confirmed > 50, sqrt(latest_covid19$confirmed)/3, 
                    ifelse(latest_covid19$confirmed > 10, 3, 2)),
    stroke = FALSE, fillOpacity = 0.5, label = con_text, color = "#926444",
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"), 
                                textsize = "13px", 
                                direction = "auto",
                                background = "#222"), 
    group = "Confirmed"
  ) %>% 
    addCircleMarkers(
      lng= only_deaths$long, lat= only_deaths$lat, 
      radius = ifelse(only_deaths$deaths > 30, sqrt(only_deaths$deaths)/2.5, 
                      ifelse(only_deaths$deaths > 5, 3, 2)),
      stroke = FALSE, fillOpacity = 0.5, label = deaths_text, color = "#672223",
      labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                               padding = "3px 8px"), 
                                  textsize = "13px", 
                                  direction = "auto",
                                  background = "#222"), 
      group = "Deaths"
    ) %>% 
    addCircleMarkers(
      lng= only_recov$long, lat= only_recov$lat, 
      radius = ifelse(only_recov$recovered > 50, sqrt(only_recov$recovered)/3, 
                      ifelse(only_recov$recovered > 10, 3, 2)),
      stroke = FALSE, fillOpacity = 0.5, label = recov_text, color = "#31542c",
      labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                               padding = "3px 8px"), 
                                  textsize = "13px", 
                                  direction = "auto",
                                  background = "#222"), 
      group = "Recovered"
    ) %>% 
    addLayersControl(
    baseGroups = c("Confirmed", "Deaths", "Recovered"),
    options = layersControlOptions(collapsed = F)
  )
  
map_corona


# 
# map_corona(latest_covid19, , "", mytext)
# 
# map_corona(latest_covid19, , "", mytext)








## modeling for trend

dat <- n %>% 
  select(-deaths, -recovered) %>% 
  arrange(desc(date))

x <- rev(1:nrow(dat))
cases <- dat$confirmed
model <- nls(cases ~ (x ^ b), start = c(b = 2), trace = T)

predict_days <- 5

x <- rev(1:(nrow(dat) + predict_days))
pred <- x ^ coef(model)
dates <- seq.Date(min(dat$date), max(dat$date) + predict_days, by = "days")

df <- data.frame(
  date = rev(dates),
  cases = c(rep(NA, predict_days), cases),
  model = floor(pred)
) %>% 
  arrange(date)

covid19_cases_trend <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = df$date) %>%
  hc_add_series(name = "Confirmed", data = df$cases) %>%
  hc_add_series(name = "Fit", data = df$model) %>% 
  hc_title(text = "Covid19 Cases by Trend") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_colors(c("#b9815b","#f7a91f")) %>% 
  hc_tooltip(table = TRUE, sort = TRUE)

graph_df <- function(data, graph_type) {
  highchart() %>%
  hc_chart(type = graph_type) %>%
  hc_xAxis(categories = data$country_region) %>%
  hc_add_series(name = "Confirmed", data = data$Confirmed) %>%
  hc_add_series(name = "Recovered", data = data$Recovered) %>%
  hc_add_series(name = "Deaths", data = data$Deaths) %>%
  hc_title(text = paste("Top", nrow(data), "Countries by Cases (Excl. China)") ) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_tooltip(table = TRUE, sort = TRUE) %>% 
  hc_colors(c("#b9815b","#62a758", "#b53f40")) 
}

top_countries <- head(m, 11)[-1, ] 

## top 10 countries

# top_countries_graph <- graph_df(top_countries, "column")

