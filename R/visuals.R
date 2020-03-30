library(tidyverse)
library(bbplot)

symptoms <- tribble(
  ~symptoms, ~percent,
  "Fever", 87.9, 
  "Dry cough", 67.7, 
  "Fatigue", 38.1, 
  "Sputum production", 33.4, 
  "Breath Shortness", 18.6, 
  "Sore throat", 13.9, 
  "Headache", 13.6, 
  "Myalgia or Arthralgia", 14.8, 
  "Chills, nausea or vomiting", 11.4, 
  "Nasal congestion", 4.8, 
  "Diarrhea", 3.7, 
  "Hemoptysis", 0.9, 
  "Conjunctival congestion", 0.8 
) %>% arrange(desc(percent))

symptoms

g <- ggplot(symptoms, aes(x = reorder(symptoms, percent), y = percent, fill = symptoms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Symptoms of Covid-19", 
       caption = "Source: WHO - Covid19 Report of China",
       x = "", y = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  bbc_style() +
  guides(fill = F) +
  theme(plot.margin = margin(1,2,1,0.5, "cm"), 
        text = element_text(size=8),
        axis.text.x = element_text(size = 10, hjust=1)) 

finalise_plot(g, source_name = "Source: WHO - COVID-19 Report of China", save_filepath= "/home/bjungbogati/Desktop")
