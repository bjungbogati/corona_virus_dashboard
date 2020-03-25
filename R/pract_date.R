starwars %>%
  select(name:mass, gender, species) %>%
  mutate(
    type = case_when(
      height > 200 | mass > 200 ~ "large",
      species == "Droid"        ~ "robot",
      TRUE                      ~  "other"
    )
  )

u <- data.frame(a = c(1, 2, 3, 4, 5), 
                b = c(5, 5, 5, 5 ,5))

u %>% mutate(
  c = case_when(
    a == 1 
  )
)

# date_top_country <- top_country_date %>% 
#   mutate(
#     day_count = case_when(
#       country_region == "China" & date >= "2020-01-22" ~ yday(date) - 21,
#       country_region == "Iran" & date >= "2020-02-24" ~ yday(date) - 54,
#       country_region == "Korea, South" & date >= "2020-02-26" ~ yday(date) - 56,
#       country_region == "Italy" & date >= "2020-02-26" ~ yday(date) - 56,
#       country_region == "US" & date >= "2020-03-06" ~ yday(date) - 65,
#       country_region == "France" & date >= "2020-03-07" ~ yday(date) - 66,
#       country_region == "Spain" & date >= "2020-03-08" ~ yday(date) - 67,
#       country_region == "Japan" & date >= "2020-03-11" ~ yday(date) - 70,
#       country_region == "Switzerland" & date >= "2020-03-14" ~ yday(date) - 72,
#       country_region == "United Kingdom" & date >= "2020-03-14" ~ yday(date) - 73,
#       country_region == "Netherlands" & date >= "2020-03-15" ~ yday(date) - 74,
#       country_region == "Germany" & date >= "2020-03-19" ~ yday(date) - 78
#     )
#   )

# date_top_country <- top_country_date %>% 
#   mutate(
#     day_count = case_when(
#       country_region == "China" & date >= min(date) ~ yday(date) - 21,
#       country_region == "Iran" & date >= min(date) ~ yday(date) - 54,
#       country_region == "Korea, South" & date >= min(date) ~ yday(date) - 56,
#       country_region == "Italy" & date >= min(date) ~ yday(date) - 56,
#       country_region == "US" & date >= min(date) ~ yday(date) - 65,
#       country_region == "France" & date >= min(date) ~ yday(date) - 66,
#       country_region == "Spain" & date >= min(date) ~ yday(date) - 67,
#       country_region == "Japan" & date >= min(date) ~ yday(date) - 70,
#       country_region == "Switzerland" & date >= min(date) ~ yday(date) - 72,
#       country_region == "United Kingdom" & date >= min(date) ~ yday(date) - 73,
#       country_region == "Netherlands" & date >= min(date) ~ yday(date) - 74,
#       country_region == "Germany" & date >= min(date) ~ yday(date) - 78
#     )
#   )


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
# 
# date_top_country <- top_country_date %>% 
# mutate(
#   day_count = case_when(
#     country_region == distinct(country_region) ~ yday(date) - yday(min(date)) + 1,
#   )
# )
# 
# class(top_country_date %>% )
# 
