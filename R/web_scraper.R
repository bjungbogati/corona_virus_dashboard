

url <- "https://www.worldometers.info/coronavirus/"

h <- url %>% read_html()    # be kind; don't rescrape unless necessary

df <- h %>% 
  html_nodes(xpath = '//*[@id="main_table_countries_today"]') %>%  
  html_table()

df <- df[[1]] 
df <- df %>% janitor::clean_names() 

df <- df[-nrow(df),] # remove last row i.e total
df <- df[-(1:8),]


df <- df %>% filter(country_other != "Total:")


tbl_df <- df %>% select(c("country_other", "total_cases", "total_deaths", "total_recovered"))



df1 <- as_tibble(gsub("[[:punct:]]", "", as.matrix(df)))

write_csv(df1, paste0("./data/", Sys.Date(), "-data.csv"))

chr_to_num <- function(x) sum(as.numeric(x), na.rm=T)

n_total <- df1 %>% 
  summarise(confirmed = chr_to_num(total_cases), 
            deaths = chr_to_num(total_deaths), 
            recovered = chr_to_num(total_recovered),
            new_confirmed = chr_to_num(new_cases),
            new_deaths = chr_to_num(new_deaths),  
            active_cases = chr_to_num(active_cases),
            serious_cases = chr_to_num(serious_critical)) %>% 
  arrange(confirmed)

total_cases_wise_summary <- n_total %>% pivot_longer(cols= 1:7, 
                         names_to = "case_type", 
                         values_to = "case_number")



# 
# df1$total_cases <- str_replace_all(df$total_cases, "[[:punct:]]", "")
# df1$new_cases <- df$new_cases %>% str_replace_all("[[:punct:]]", "")
# df1$total_deaths <- df$total_deaths %>% str_replace_all("[[:punct:]]", "")
# df1$new_deaths <- df$new_deaths %>% str_replace_all("[[:punct:]]", "")
# df1$total_recovered <- df$total_recovered %>% str_replace_all("[[:punct:]]", "")
# df1$active_cases <- df$tactive_cases %>% str_replace_all("[[:punct:]]", "")
# df1$series_critical <- df$series_critical  %>% str_replace_all("[[:punct:]]", "")











