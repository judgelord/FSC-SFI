source("setup.R")


# d <- read.csv("FSC.csv")
d <- read.csv(here("data/PEFCvFSC.csv"))
# d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "FSC", Net.Change+4, Increased))
# d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "SFI", Net.Change+1, Increased))
# also not run for PEFC? 
names(d)
d %<>% gather("Measure","Change08", 7:8)
names(d)

d %<>% mutate(Measure = ifelse(Measure == "Net.Change", "Net Change", Measure))
# d.fsc <- filter(d, Program == "FSC-US")
# d.sfi <- filter(d, Program == "SFI")
d %<>% mutate(Program = ifelse(Program == "FSC", "FSC-US", Program))
d$Program %<>% as.factor()
d$Strictist %<>% as.factor()
d$Year %<>% as.numeric()
d$Prescriptiveness %<>% as.factor()
levels(d$Prescriptiveness) <- c("No prescriptive requirements", "Some prescriptive requirements", "At least as or most prescriptive")
d$Change %<>% as.factor()
levels(d$Change) <- c("Became less prescriptive", "No change", "Became more prescriptive")
d$Change %<>% relevel("Became more prescriptive")
d$Change %<>% relevel("Became less prescriptive")

#year as factor data
data <- filter(d, Measure == "Increased")
# data$Year %<>% as.factor()


# GOOD PLOTS 

# line plot (line)
line <- ggplot() + 
  geom_step(data = d %>% filter(Program %in% c(
  #"FSC-US", "SFI"
  "FSC-P&C", "PEFC"
  )), 
       aes(x = Year, y = Change08, linetype = Measure)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  #geom_segment(data = d.sfi, aes(x = 2008, y = 0, xend = 2016, yend = 12), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) + 
  #geom_segment(data = d.fsc, aes(x = 2008, y = 0, xend = 2016, yend = 16), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) +
  geom_text(data = data.frame(Program = "PEFC"), label = "Increased in
19 issues
            since 2008", x = 2015, y = 18, vjust = 1, hjust = 1, size = 3) + 
  geom_text(data = data.frame(Program = "FSC-P&C"), label = "Increased in 11
            issues since 2008", x = 2015, y = 18, vjust = 1, hjust = 1, size = 3) + 
  labs(title = "Absolute Change in Prescriptiveness") + 
  scale_y_continuous(breaks = seq(0,48, by = 4)) + 
  scale_x_continuous(breaks = seq(2008,2015, by = 1)) + 
  #scale_linetype_discrete(name = "Change in Prescriptiveness") + 
  theme(#panel.grid.major = element_blank(), 
    strip.text.x = element_blank(),
        #remove legend title
        legend.title=element_blank(),
        #legend.key.width=grid::unit(0.2,"cm"),
        axis.title.x = element_blank(),
        #change axis ticks thickness
        axis.ticks.x=element_blank(),
        # horizontal y
        #axis.title.y=element_text(angle = 0),
        # or blank y
        axis.title.y=element_blank(),
        #axis.text.x=element_blank()
    axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1)
    )

# bar plot (bar)
bar <- 
  ggplot() + 
    geom_bar(data = filter(d, Measure == "Increased" & Program %in% c("FSC-P&C", "PEFC" # "FSC-US", "SFI"
    )), aes(x = Year, alpha = Prescriptiveness, fill = Change)) +
    facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
    geom_segment(data = filter(d, Measure == "Increased" & Program == "FSC-P&C"), aes(x = 2007.5, y = 34, xend = 2011.5, yend = 34), linetype = 1, size = .5) + 
    geom_segment(data = filter(d, Program == "FSC-P&C"), aes(x = 2011.5, y = 36, xend = 2015.5, yend = 36), linetype = 1, size = .5) + 
    geom_segment(data = filter(d, Program == "PEFC"), aes(x = 2007.5, y = 20, xend = 2009.5, yend = 20), linetype = 1, size = .5) + 
    geom_segment(data = filter(d, Program == "PEFC"), aes(x = 2009.5, y = 27, xend = 2015.5, yend = 27), linetype = 1, size = .5) + 
    geom_text(data = data.frame(Program = "PEFC"), label = "As prescriptive or 
              more on 27 issues", x = 2015, y = 2, vjust = 0, hjust = 1, size = 3) + 
    geom_text(data = data.frame(Program = "FSC-P&C"), label = "As prescriptive or
              more on 36 issues", x = 2015, y = 2, vjust = 0, hjust = 1, size = 3) + 
    #geom_text(data = data.frame(Program = "PEFC"), label = "41 issues", x = 2015.5, y = 42, vjust = 0, hjust = 1, size = 3) + 
    #geom_text(data = data.frame(Program = "FSC-P&C"), label = "42 issues", x = 2015.5, y = 43, vjust = 0, hjust = 1, size = 3) + 
    labs(title = "Relative Prescriptiveness and Scope") + 
    scale_y_continuous(breaks = seq(0,48, by = 8)) + 
  #geom_point(data = data %>% filter(Program %in% c("FSC-US", "SFI"), Change %in% c("Became less prescriptive", "Became more prescriptive")), aes(shape = Change))  +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #remove legend title
        legend.title=element_blank(),
        #set a slim legend
        # legend.key.width=grid::unit(0.2,"cm"),
        axis.title = element_blank(),
        #change axis ticks thickness
        axis.ticks.x=element_blank(),
        #axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1),
        axis.text.x=element_blank(),
        #strip.text.x = element_blank(),
        strip.background = element_blank())



# tile plot (tile)
tile <- ggplot(d %>% filter(Program %in% c(
  #"FSC-US", "SFI"
  "FSC-P&C", "PEFC"
  )), 
                 aes(x = Year, y = Issues)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(alpha = Prescriptiveness, fill = Change, color = Strictist),#)+
            color = "white") + 
  #labs(title = "Scope of Key Issues Addressed") + 
# geom_point(data = data %>% filter(Program %in% c("FSC-US", "SFI"), Change %in% c("Became less prescriptive", "Became more prescriptive")), aes(shape = Change))  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #remove legend title
        legend.title=element_blank(),
        #set a slim legend
        # legend.key.width=grid::unit(0.2,"cm"),
        axis.title = element_blank(),
        #change axis ticks thickness
        axis.ticks=element_blank(),
        #axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())
  
# both
# ggpubr::ggarrange(line, bar, tile, ncol = 1, nrow = 3, align = "v", heights = c(.7, .7,2))
          #common.legend = T,
          #legend = "right",
          #label.y = "Key Issues",
          
       # PORTRATE 
grid.newpage()
grid.draw(egg::ggarrange(bar, tile, line, 
                    ncol = 1,
                    #common.legend = T,
                    #legend = "right",
                    heights = c(.4, 2, .4)))       

# flip 3
grid.draw(egg::ggarrange(bar, line, tile, 
                         ncol = 1,
                         #common.legend = T,
                         #legend = "right",
                         
                         heights = c(.4, .3,2)))       

# flip 4
grid.newpage()
grid.draw(egg::ggarrange(tile, bar, line,
                         ncol = 1,
                         #common.legend = T,
                         #legend = "right",
                         heights = c(2, .4,.3)))      








 # LANDSCAPE 

# line plot (lineright)
lineright <- ggplot(d %>% filter(Program %in% c("FSC-US", "SFI")), 
              aes(x = Year, y = Change, linetype = Measure)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  #geom_segment(data = d.sfi, aes(x = 2008, y = 0, xend = 2016, yend = 12), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) + 
  #geom_segment(data = d.fsc, aes(x = 2008, y = 0, xend = 2016, yend = 16), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) +
  geom_step() + 
  labs(title = "Change in Prescriptiveness") + scale_y_continuous(breaks = seq(0,48, by = 4)) + 
  #scale_linetype_discrete(name = "Change in Prescriptiveness") + 
  theme(#panel.grid.major = element_blank(), 
    #remove legend title
    legend.title=element_blank(),
    legend.position = "top",
    #legend.key.width=grid::unit(0.2,"cm"),
    axis.title.x = element_blank(),
    #change axis ticks thickness
    axis.ticks.x=element_blank(),
    # horizontal y
    #axis.title.y=element_text(angle = 0),
    # or blank y
    axis.title.y=element_blank(),
    axis.text.x=element_blank())



data$Change %<>% {gsub("less prescriptive", "less prescriptive          .",.)}
# bar plot (bar)
right <- ggplot(data %>% filter(Program %in% c("FSC-US", "SFI")), 
                 aes(x = Year)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_bar(aes(alpha = Prescriptiveness, fill = Change)) +
  scale_y_continuous(breaks = seq(0,48, by = 8)) + 
  labs(title = "Little Change in Scope") +
  #geom_point(data = data %>% filter(Program %in% c("FSC-US", "SFI"), Change %in% c("Became less prescriptive", "Became more prescriptive")), aes(shape = Change))  +
  theme(#panel.grid.major = element_blank(), 
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    #remove legend title
    legend.title=element_blank(),
    legend.direction='vertical',
    legend.box='vertical',
    legend.justification = 0,
    legend.text.align = 0,
    legend.position="bottom",
    # legend.key.width=grid::unit(0.2,"cm"),
    #axis.title.y=element_text(angle = 0),
    axis.title=element_blank(),
    axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1),
    #change axis ticks thickness
    axis.ticks.x=element_blank())


# tile plot (left)
left <- ggplot(data %>% filter(Program %in% c("FSC-US", "SFI")), 
                 aes(x = Year, y = Issue)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(alpha = Prescriptiveness, fill = Change, color = Strictist),#)+
            color = "white") + 
  labs(title = "48 Key Issues") +
  # geom_point(data = data %>% filter(Program %in% c("FSC-US", "SFI"), Change %in% c("Became less prescriptive", "Became more prescriptive")), aes(shape = Change))  +
  theme(panel.grid.major = element_blank(), 
        #remove legend title
        legend.title=element_blank(),
        legend.position = "none",
        #set a slim legend
        # legend.key.width=grid::unit(0.2,"cm"),
        axis.title = element_blank(),
        #change axis ticks thickness
        axis.ticks=element_blank(),
        axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1),
        #strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.grid.minor = element_blank())

# LANDSCAPE 
Right <- ggpubr::ggarrange(lineright, right, nrow = 2, heights = c(1,2))
ggpubr::ggarrange(left, Right, ncol = 2, heights = c(1, .8), widths = c(1.5,1))



