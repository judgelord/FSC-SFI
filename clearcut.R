


d <- read_csv("data/clearcuts.csv")

d %>% 
  ggplot(aes(x = program, ymin= min, ymax=max, label = text)) + 
  geom_point( aes(y = min)) + 
  geom_point( aes(y = max)) + 
  geom_text( aes(y = 1), hjust = 0, size = 2.5) + 
  geom_linerange()+
  coord_flip() + 
  labs(x = "", y = "Acres",
       caption = "*Ranges reflect different maximums in different contexts") + 
  facet_grid(criteria ~., scales = "free", space = "free", switch = "y") +
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside")

