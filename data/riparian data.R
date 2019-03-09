


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

fsc <- d %>% 
  filter(program %in% c("FSC-US", "FSC-P&C")) %>% 
  ggplot(aes(x = Criteria, y = feet, fill= type, label = text)) + 
  geom_col(position = "stack") + 
  geom_text( aes(y = 0), hjust = 0, size = 2.5, color = "#458B00")+
  coord_flip() +
  labs(x = "", y = "Feet", fill = "",
       title = "Activist-backed FSC")+
  scale_fill_viridis_d(begin = .9, end = .5, option = "B") + 
  facet_grid(region ~., scales = "free", space = "free", switch = "y") +
  theme_gray() +
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside")

sfi <- d %>% 
  filter(program %in% c("SFI", "PEFC")) %>% 
  ggplot(aes(x = Criteria, y = feet, fill= type, label = text)) + 
  geom_col(position = "stack") + 
  geom_text( aes(y = 0), hjust = 0, size = 2.5, color = "#458B00")+
  coord_flip() +
  labs(x = "", y = "Feet", fill = "",
       title = "Industry-backed SFI & PEFC")+
  scale_fill_viridis_d(begin = .9, end = .5, option = "B") + 
  facet_grid(region ~., scales = "free", space = "free", switch = "y") +
  theme_gray() +
  theme(strip.text.y = element_text(angle = 180),
        strip.placement = "outside",
        legend.position = "none")

grid.newpage()
grid.draw(egg::ggarrange(fsc, sfi, 
                         ncol = 1,
                         #common.legend = T,
                         #legend = "right",
                         heights = c(2, .4)))  

