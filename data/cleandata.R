

# ISSUE DATA
data <- read.csv(here("data/FSCissues.csv"))
data$Change <- gsub("Bacame", "Became", data$Change)
data$Issue %<>% gsub(" $", "", .)
data$Change %<>% gsub(" * .$", "", .)

data$Strictist %<>% as.factor()

# PROGRAM NAMES 
data %<>% mutate(Program = ifelse(Program == "FSC-US", "Activist-backed FSC-US", Program))
data %<>% mutate(Program = ifelse(Program == "SFI", "Industry-backed SFI", Program))
data %<>% mutate(Program = ifelse(Program == "PEFC", "Industry-backed PEFC", Program))

# corrections
data %<>% mutate(Change = ifelse(Issue  == "Plantations" & Year == "2015" & Program == "Industry-backed SFI", "Became more prescriptive", Change))

data %<>% mutate(Change = ifelse(Issue  == "Riparian" & Year == "2010" & Program == "Activist-backed FSC-US", "Became more prescriptive", Change))

data %<>% mutate(Prescriptiveness = ifelse(Issue  == "Plantations" & Year %in% c("2015", "2016") & Program == "Industry-backed SFI", 2, Prescriptiveness))

# RELEVEL
data$Prescriptiveness %<>% str_replace("At least as or m", "M")
data$Prescriptiveness %<>% as.factor()
levels(data$Prescriptiveness) <- c("No prescriptive requirements", "Some prescriptive requirements", "Most prescriptive")


SFIvFSC <- data 
SFIvFSC$Issue %<>% as.factor()








# make table 
issues <- filter(data) %>% 
  group_by(Issue, Year) %>% mutate(Change = paste(Strictist, Change)) %>%
  group_by(Issue, Year) %>% mutate(pattern = paste(Change, collapse = ": ")) %>%
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
# correction to above method
issues %<>% mutate(pattern = ifelse(pattern == "Upward parallell" & Issue == "Riparian", "Upward divergence", pattern))






















# Get data 
d <- read.csv(here("data/FSC.csv"))
d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "FSC", Net.Change+4, Net.Change))
d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "SFI", Net.Change+1, Increased))
d %<>% gather("Measure","Change", 6:7)
d %<>% group_by(Program)
d %<>% gather("Prescriptiveness","Issues", 3:5)
d$Prescriptiveness %<>% as.factor()
#d$Prescriptiveness %<>% relevel("Some.Requirements")
#d$Prescriptiveness %<>% relevel("Most.Prescriptive")
d %<>% ungroup()
d %<>% mutate(Program = ifelse(Program == "FSC", "Activist-backed FSC-US", Program))
d %<>% mutate(Program = ifelse(Program == "SFI", "Industry-backed SFI", Program))
d$Program %<>% as.factor()
d %<>% mutate(Measure = ifelse(Measure == "Net.Change", "Net Change", Measure))

# corrections
d %<>% mutate(Change = if_else(Year > 2009 & Program == "Activist-backed FSC-US" & Measure == "Increased", 21, Change))
d %<>% mutate(Change = if_else(Year > 2009 & Program == "Activist-backed FSC-US" & Measure == "Net Change", 17, Change))

d %<>% mutate(Change = if_else(Year > 2009 & Program == "Industry-backed SFI" & Measure == "Increased", 8, Change))
d %<>% mutate(Change = if_else(Year > 2012 & Program == "Industry-backed SFI" & Measure == "Increased", 9, Change))
d %<>% mutate(Change = if_else(Year > 2014 & Program == "Industry-backed SFI" & Measure == "Increased", 12, Change))

d %<>% mutate(Change = if_else(Year > 2009 & Program == "Industry-backed SFI" & Measure == "Net Change", 7, Change))
d %<>% mutate(Change = if_else(Year > 2012 & Program == "Industry-backed SFI" & Measure == "Net Change", 8, Change))
d %<>% mutate(Change = if_else(Year > 2014 & Program == "Industry-backed SFI" & Measure == "Net Change", 11, Change))

#data <- filter(d, Measure == "Increased")

SFIvFSCnetChange <- d






# PEFC FSC 


d <- read.csv(here("data/PEFCvFSC.csv"))
# d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "FSC", Net.Change+4, Increased))
# d %<>% mutate(Increased = ifelse(Year > 2009 & Program == "Industry-backed SFI", Net.Change+1, Increased))
# also not run for PEFC? 
d %<>% gather("Measure","Change08", 7:8)

d %<>% mutate(Measure = ifelse(Measure == "Net.Change", "Net Change", Measure))
# d.fsc <- filter(d, Program == "Activist-backed FSC-US")
# d.sfi <- filter(d, Program == "Industry-backed SFI")
d$Program %<>% as.character()
d %<>% mutate(Program = ifelse(Program == "FSC-P&C", "Activist-backed FSC P&C", Program))
d %<>% mutate(Program = ifelse(Program == "PEFC", "Industry-backed PEFC", Program))
d$Program %<>% as.factor()
d$Strictist %<>% as.factor()
d$Year %<>% as.numeric()
d$Prescriptiveness %<>% as.factor()
levels(d$Prescriptiveness) <- c("No prescriptive requirements", "Some prescriptive requirements", "Most prescriptive")
d$Change %<>% as.factor()
levels(d$Change) <- c("Became less prescriptive", "No change", "Became more prescriptive")
d$Change %<>% relevel("Became more prescriptive")
d$Change %<>% relevel("Became less prescriptive")

PEFCvFSC <- d
PEFCvFSC$Issues %<>%  as.factor()

#year as factor data
data <- filter(d, Measure == "Increased")
# data$Year %<>% as.factor()

d %<>% mutate(Change08 = ifelse(Year > 2011 & Program == "Activist-backed FSC P&C" & Measure == "Increased", 13, Change08))

d %<>% mutate(Change08 = ifelse(Year > 2011 & Program == "Activist-backed FSC P&C" & Measure == "No change", 9, Change08))

d %<>% mutate(Change08 = ifelse(Year > 2009 & Program == "Industry-backed PEFC", 18, Change08))

PEFCvFSCnetChange <- d



save.image(here("data/SFIvFSC.Rdata"))


