library(tidyverse)

symptoms <- tribble(
  ~symptoms, ~percent,
  "Fever", 87.9, 
  "Dry cough", 67.7, 
  "Fatigue", 38.1, 
  "Sputum production", 33.4, 
  "Shortness of breath", 18.6, 
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

ggplot(symptoms, aes(x = reorder(symptoms, percent), y = percent, fill = symptoms)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Symptoms of Covid-19", 
       caption = "Source: WHO - Covid19 Report of China",
       x = "", y = "") +
  theme_classic() +
  guides(fill = F) +
  theme(plot.margin = margin(1,2,1,0.5, "cm"))
