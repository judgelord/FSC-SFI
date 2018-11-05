options(stringsAsFactors = FALSE)

requires <- c("gmailr", 
              "dplyr", 
              "ggplot2", 
              "magrittr",
              "here",
              "gridExtra",
              "ggpubr",
              "grid", 
              "egg",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(here)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
library(gtable)
library(egg)
library(ggpubr)



d <- read.csv(here("data/FSC.csv"))
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
data <- read.csv(here("data/FSCissues.csv"))
data$Change <- gsub("Bacame", "Became", data$Change)
data$Issue %<>% gsub(" $", "", .)
data$Change %<>% gsub(" * .$", "", .)
unique(data$Change)
names(data)
data$Strictist %<>% as.factor()
# data$Year %<>% as.factor()

#correction
data %<>% mutate(Change = ifelse(Issue  == "Plantations" & Year == "2015" & Program == "SFI", "Became more prescriptive", Change))
data %<>% mutate(Change = ifelse(Issue  == "Riparian" & Year == "2010" & Program == "FSC-US", "Became more prescriptive", Change))
data %<>% mutate(Prescriptiveness = ifelse(Issue  == "Plantations" & Year %in% c("2015", "2016") & Program == "SFI", 2, Prescriptiveness))
data$Prescriptiveness %<>% as.factor()
# data$Change %<>% as.factor()
levels(data$Prescriptiveness) <- c("No prescriptive requirements", "Some prescriptive requirements", "At least as or most prescriptive")
# data$Issue %<>% as.factor()


# issues where increased 
fscissues <- filter(data, Program == 'FSC-US', Change == 'Became more prescriptive', Year == 2010) %>% distinct()
fscissues
# 
sfiissues <- filter(data, Program == 'SFI', Change == 'Became more prescriptive', Year == 2010) %>% select(Issue, Prescriptiveness, Strictist) %>% distinct()
sfiissues
# 
both <- filter(fscissues, Issue %in% sfiissues$Issue)
both
filter(fscissues, !Issue %in% both$Issue)
filter(sfiissues, !Issue %in% both$Issue)



# issues where decreased 
fscissues <- filter(data, Program == 'FSC-US', Change == "Became less prescriptive") %>% select(-Year) %>% distinct()
fscissues
# 
sfiissues <- filter(data, Program == 'SFI', Change == "Became less prescriptive") %>% select(-Year) %>% distinct()
sfiissues
# 
both <- filter(fscissues, Issue %in% sfiissues$Issue)
both


#######################################################################################
# make table 
issues <- filter(data, Year == 2010) %>% 
  group_by(Issue) %>% mutate(Change = paste(Strictist, Change)) %>%
  group_by(Issue) %>% mutate(pattern = paste(Change, collapse = ": ")) %>%
  mutate(pattern = ifelse(pattern == "1 No change: 0 No change", "Equilibrium", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 No change: 1 No change", "Equilibrium", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 No change: 0 No change", "Equilibrium", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 Became less prescriptive: 1 No change", "Downward divergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "1 Became less prescriptive: 0 No change", "Downward convergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "1 Became more prescriptive: 0 No change", "Upward divergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 Became more prescriptive: 1 No change", "Upward convergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "1 No change: 0 Became more prescriptive", "Upward convergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 No change: 1 Became more prescriptive", "Upward divergence", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "1 Became less prescriptive: 0 Became less prescriptive", "Downward parallell", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "0 Became less prescriptive: 1 Became less prescriptive", "Downward parallell", pattern)) %>% 
  mutate(pattern = ifelse(pattern == "1 Became more prescriptive: 0 Became more prescriptive", "Upward parallell", pattern))  %>% 
  mutate(pattern = ifelse(pattern == "0 Became more prescriptive: 0 Became more prescriptive", "Upward parallell", pattern))  %>% 
  mutate(pattern = ifelse(pattern == "0 Became more prescriptive: 1 Became more prescriptive", "Upward parallell", pattern))  %>% 
  mutate(pattern = ifelse(pattern == "1 Became less prescriptive: 0 Became more prescriptive", "Opposing divergence", pattern)) %>%
  mutate(pattern = ifelse(pattern == "1 Became more prescriptive: 0 Became less prescriptive", "Opposing divergence", pattern)) %>%
  mutate(pattern = ifelse(pattern == "0 Became less prescriptive: 1 Became more prescriptive", "Opposing divergence", pattern)) %>%
  mutate(pattern = ifelse(pattern == "0 Became less prescriptive: 0 Became more prescriptive", "Opposing divergence", pattern)) # aesthetic feels wrong 

# Table
Issues <- issues %>% group_by(pattern)  %>% select(pattern, Issue) %>% distinct() %>% mutate(issues = paste(Issue, collapse = ": ")) %>% group_by(pattern, issues) %>% tally() %>% arrange(-n)


  
















# GOOD PLOTS 

# line plot (line)
line <- ggplot() + 
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

# bar plot (bar)
bar <- 
  ggplot() + 
  geom_bar(data = filter(data, Program %in% c("FSC-US", "SFI")), 
           aes(x = Year, alpha = Prescriptiveness, fill = Change), position = "stack") +
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
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
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
tile <- ggplot(data %>% filter(Program %in% c("FSC-US", "SFI")), 
aes(x = Year, y = Issue)) + 
  facet_grid(. ~ Program) + # scale_color_grey() + scale_fill_grey() + scale_color_grey() +
  geom_tile(aes(alpha = Prescriptiveness, fill = Change, color = Strictist),#)+
            color = "white") + 
  geom_point(aes(shape = Change)) +
  scale_shape_manual(values=c("-", "+", ""))+
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


# TRY INSTEAD A NEW TILE PLOT 3x3

# both
# ggpubr::ggarrange(line, bar, tile, ncol = 1, nrow = 3, align = "v", heights = c(.7, .7,2))
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
    
legendless <- g_legend(somelegend)
legendmore <- g_legend(mostlegend)
legendno <- g_legend(nolegend)


# PORTRATE 
grid.newpage()
grid.draw(egg::ggarrange(bar, tile, line,
                         ncol = 1, nrow = 3,
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






