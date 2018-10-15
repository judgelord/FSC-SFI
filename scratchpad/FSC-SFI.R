# install.packages("ggpubr")
# install.packages("devtools")       
# install.packages("egg")
# install.packages("tidyverse")
# install.packages("magrittr")


options(stringsAsFactors = FALSE)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
library(gtable)


getwd()
setwd("/Users/judgelord/Downloads")
setwd("/Users/Patron/Downloads")
setwd("/Users/user/Downloads")

d <- read.csv("FSC.csv")
d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "FSC", Net.Change+4, Net.Change))
d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "SFI", Net.Change+1, Increased))
d %<>% gather("Measure","Change", 6:7)
names(d)
d %<>% group_by(Program)
d %<>% gather("Prescriptiveness","Issues", 3:5)
d$Prescriptiveness %<>% as.factor()
d$Prescriptiveness %<>% relevel("Some.Requirements")
d$Prescriptiveness %<>% relevel("Most.Prescriptive")
d %<>% ungroup()
d %<>% mutate(Program = ifelse(Program == "FSC", "FSC-US", Program))
d$Program %<>% as.factor()
d %<>% mutate(Measure = ifelse(Measure == "Net.Change", "Net Change", Measure))

d.fsc <- filter(d, Program == "FSC-US")
d.sfi <- filter(d, Program == "SFI")




# ISSUE DATA
data <- read.csv("FSCissues.csv")
data$Change <- gsub("Bacame", "Became", data$Change)
unique(data$Change)
names(data)
data$Strictist %<>% as.factor()
# data$Year %<>% as.factor()
#correction
data %<>% mutate(Change = ifelse(Issue  == "Plantations" & Year == "2015" & Program == "SFI", "Became more prescriptive", Change))
data %<>% mutate(Prescriptiveness = ifelse(Issue  == "Plantations" & Year %in% c("2015", "2016") & Program == "SFI", 2, Prescriptiveness))
data$Prescriptiveness %<>% as.factor()
data$Change %<>% as.factor()
levels(data$Prescriptiveness) <- c("No prescriptive requirements", "Some prescriptive requirements", "At least as or most prescriptive")
data$Issue %<>% as.factor()







# GOOD PLOTS 

# line plot (top)
top <- ggplot() + 
  geom_step(data = d %>% filter(Program %in% c(
    "FSC-US", "SFI")), 
  aes(x = Year, y = Change, linetype = Measure)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  #geom_segment(data = d.sfi, aes(x = 2008, y = 0, xend = 2016, yend = 12), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) + 
  #geom_segment(data = d.fsc, aes(x = 2008, y = 0, xend = 2016, yend = 16), linetype = 1, alpha = .005, size = 5, arrow = arrow(length = unit(0.3, "npc"))) +
  geom_text(data = data.frame(Program = "SFI"), label = "Increased in 13
            issues since 2008", x = 2016, y = 14, vjust = 0, hjust = 1, size = 3) + 
  geom_text(data = data.frame(Program = "FSC-US"), label = "Increased in
20 issues
            since 2008", x = 2016, y = 0, vjust = 0, hjust = 1, size = 3) + 
  labs(title = "Absolute Change in Prescriptiveness") + 
  scale_y_continuous(breaks = seq(0,48, by = 4)) + 
  scale_x_continuous(breaks = seq(2008,2016, by = 1)) + 
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

# bar plot (middle)
middle <- 
  ggplot() + 
  geom_bar(data = filter(data, Program %in% c("FSC-US", "SFI")), 
           aes(x = Year, alpha = Prescriptiveness, fill = Change)) +
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  #geom_segment(data = filter(data, Program == "FSC-US"), aes(x = 2007.5, y = 34, xend = 2011.5, yend = 34), linetype = 1, size = .5) + 
  #geom_segment(data = filter(data, Program == "FSC-US"), aes(x = 2011.5, y = 36, xend = 2015.5, yend = 36), linetype = 1, size = .5) + 
  #geom_segment(data = filter(data, Program == "SFI"), aes(x = 2007.5, y = 20, xend = 2009.5, yend = 20), linetype = 1, size = .5) + 
  #geom_segment(data = filter(data, Program == "SFI"), aes(x = 2009.5, y = 27, xend = 2015.5, yend = 27), linetype = 1, size = .5) + 
  geom_text(data = data.frame(Program = "SFI"), label = "As prescriptive or 
            more on 27 issues", x = 2016, y = 2, vjust = 0, hjust = 1, size = 3) + 
  geom_text(data = data.frame(Program = "FSC-US"), label = "As prescriptive or
            more on 36 issues", x = 2016, y = 2, vjust = 0, hjust = 1, size = 3) + 
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


# tile plot (bottom)
bottom <- ggplot(data %>% filter(Program %in% c("FSC-US", "SFI")), 
aes(x = Year, y = Issue)) + 
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
# ggpubr::ggarrange(top, middle, bottom, ncol = 1, nrow = 3, align = "v", heights = c(.7, .7,2))
#common.legend = T,
#legend = "right",
#label.y = "Key Issues",

nolegend <- ggplot(data %>% filter, 
                   aes(x = Year, y = Issue)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(fill = Change),
            color = "white", alpha=.1) +
  scale_fill_discrete(name="No prescriptive requirements")
  
somelegend <- ggplot(data, 
                     aes(x = Year, y = Issue)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(fill = Change),
            color = "white", alpha=.5) +
  scale_fill_discrete(name="At least as or more prescriptive")

mostlegend <- ggplot(data, 
                   aes(x = Year, y = Issue)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(fill = Change),
            color = "white", alpha=1) +
  scale_fill_discrete(name="At least as or more prescriptive")
#  This is a function found in this post to create a legend grob.
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
#  Now use this function to create 2 legend grobs with different color schemes, then use grid.arrange to put everything together:
    
legendless <- g_legend(lesslegend)
legendmore <- g_legend(morelegend)
legendno <- g_legend(nolegend)


# PORTRATE 
grid.newpage()
grid.draw(egg::ggarrange(middle, bottom, top, legendno, legendmore, legendless,
                         ncol = 2, nrow = 3,
                         #common.legend = T,
                         #legend = "right",
                         heights = c(.4, 2, .4)))    

grid.newpage()
grid.draw(egg::ggarrange(legendno, legendmore, legendless,
                         ncol = 1, nrow = 3,
                         #common.legend = T,
                         #legend = "right",
                         heights = c(.4, 2, .4)))



#########################

















 # LANDSCAPE 

# line plot (topright)
topright <- ggplot(d %>% filter(Program %in% c("FSC-US", "SFI")), 
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
# bar plot (middle)
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
Right <- ggpubr::ggarrange(topright, right, nrow = 2, heights = c(1,2))
ggpubr::ggarrange(left, Right, ncol = 2, heights = c(1, .8), widths = c(1.5,1))



