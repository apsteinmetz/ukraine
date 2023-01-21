# plot gdps of ukraine belligerants
# sources: UN, Kiel Institute
library(tidyverse)

gdp_raw <- read_csv("data/world_gdp_un.csv")
load("data/short_country_names.rdata")
ru_names <- read.csv("data/russian_country_names.txt") %>% 
  as_tibble()

gdp <- gdp_raw %>% 
  as_tibble %>% 
  left_join(short_country_names) %>% 
  filter(Item == "Gross Domestic Product (GDP)") %>% 
  mutate(GDP_TRN_USD = Value/1000000000000) %>% 
  filter(Year == 2020) %>% 
  filter(!is.na(aiding)) %>% 
  mutate(aiding = factor(aiding,
                         levels = c("Ukraine","Aiding Ukraine","Russia","Aiding Russia"))) %>% 
  select(Country,aiding,GDP_TRN_USD) %>% 
  bind_cols(ru_names) %>% 
  mutate(Помощь = aiding)

levels(gdp$Помощь) <- c("Украина", "Помощь Украине", "Россия", "Помощь России")

gdp %>% 
  group_by(aiding) %>% 
  tally(GDP_TRN_USD,name = "GDP_TRN_USD") %>% 
  ggplot(aes(aiding,GDP_TRN_USD)) + geom_col() + 
  labs(title = "Who Wins A War of Attrition?",
       subtitle = "GDP of Belligerents and their Helpers in the Russo-Ukraine War",
       y = "GDP in 2020 (Trillion USD)",
       x = "Countries",
       caption = "Sources: UN, Kiel Institute")

# stacked bar
gdp %>% 
  ggplot(aes(aiding,GDP_TRN_USD,fill=Country)) + 
           geom_col(color = "black") +
  scale_fill_viridis_d()  +
  annotate("text",x = 4,y=5,label = "Iran,\nN.Korea,\nBelarus") +
  labs(title = "Who Wins A War of Attrition?",
       subtitle = "GDP of Belligerents and their Helpers in the Russo-Ukraine War",
       y = "GDP in 2020 (Trillion USD)",
       x = "Countries",
       caption = "Sources: UN, Kiel Institute") + 
  theme_minimal() + 
  theme(panel.grid = element_blank())


gdp %>% 
  ggplot(aes(Помощь,GDP_TRN_USD,fill=страна)) + 
  geom_col(color = "black") +
  scale_fill_viridis_d()  +
  labs(title = "Кто выиграет войну на истощение?",
       subtitle = "ВВП воюющих сторон и их пособников в русско-украинской войне",
       y = "ВВП в 2020 г. (триллион долларов США)",
       x = "Страны",
       caption = "Источники: ООН, Кильский институт.") + 
  theme_minimal()

