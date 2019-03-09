


d <- read_csv("data/riparian-buffers.csv") %>% 
  mutate(Criteria = glue("{criteria} {criteria2}", .na = "")) %>%
  mutate(Standard = glue("{program} {region}", .na = "")) %>%
  gather(none, tree, group, key = "type", value = "feet")

d$type %<>% factor(levels = c("group", "tree", "none") )
d$type %<>% fct_recode("Single tree only" = "tree",
                       "Limited group selectoin" = "group",
                       "No harvesting" = "none")

d$Criteria %<>% factor()
d$Criteria %<>% fct_reorder(d$feet)

d %>% 
  ggplot(aes(x = Criteria, y = feet, fill= type, label = text)) + 
  geom_col(position = "stack") + 
  geom_text( aes(y = 0), hjust = 0, size = 2, color = "#458B00")+
  coord_flip() +
  labs(x = "", y = "Feet", fill = "")+
  scale_fill_viridis_d(begin = .9, end = .5, option = "B") + 
  facet_grid(Standard ~., scales = "free", space = "free", switch = "y") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside")

