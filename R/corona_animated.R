# load required packages

library(rvest)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(lubridate)
library(gganimate)
library(plotly)



anim <- ggplot(m, aes(x = country_region, y = Confirmed)) +
  geom_bar(stat = "identity", aes(fill = country_region)) +
  geom_text(aes(y = 0), vjust = 0.2, hjust = 1, size = 4.5) +
  geom_text(aes(y = value, label = Value_lbl, hjust = 0), size = 4.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  coord_flip(clip = "off", expand = FALSE) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "grey"),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    plot.title = element_text(size = 25, hjust = 0.5, face = "bold", vjust = 1),
    plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
    plot.background = element_blank(),
    plot.margin = margin(2, 2, 2, 4, "cm")
  ) +
  transition_states(year, transition_length = 3, state_length = 1) +
  enter_drift(x_mod = -2) +
  exit_drift(x_mod = 2) +
  ease_aes("cubic-in") +
  view_follow(fixed_x = TRUE) +
  labs(
    title = "Tourist Arrival by country : {closest_state}",
    subtitle = "Top 10 Countries"
  )

# create animation, declare the width height of the plot  
animate(anim, fps = 8, width = 1000, height = 550)
anim_save("simulations.gif")