Issues             pres, high
fsc2008 <- rbind(
  c("Forest Extent", 3, 1),
  c("Conservation Value Forests",3, 1),
  c("Protected Areas", 3,1),
  c("Ecological Function", 3, 0),
  c("Snags and Wood", 3, 0),
  c("Plantations", 3, 1),
  c("Species Diversity", 3, 1),
  c("Species at Risk", 3 , 1),
  c("Genetic Diversity", 3, 1),
  c("GMOs", 3, 1), 
  c("Biotech", 3, 1), 
  c("Restoration", 3, 0), 
  c("Chemical Use", 3, 1), 
  c("Timber Production", 3, 0), 
  c("Non-timber Forest Products", 3, 0), 
  c("Environmental Services", 3, 0), 
  c("Reforestation", 3, 0), 
  c("Tenure", 3, 1), 
  c("Traditional Knowledge", 3, 1), 
  c("Community Benefit", 3, 1), 
  c("Right to Organize", 3, 1), 
  c("Health and Safety", 3, 1), 
  c("Public Reporting & Consultation", 3, 1), 
  c("Economic Viability", 3, 0), 
  c("Forest Law", 3, 1), 
  c("Management Plan", 3, 0), 
  c("Impact Assessment", 3, 1), 
  c("Monitoring", 3, 1), 
  c("Continual Improvement", 3, 0), 
  c("Training", 3, 1), 
  c("Riparian", 3, 0), 
  c("Exotics", 3, 0), 
  c("Natural Disturbances", 3, 1), 
  c("Research", 3, 1), 
  c("Soil", 2, 0), 
  c("Residual Trees", 2, 0), 
  c("Roads", 2, 0), 
  c("Special Sites", 2, 0), 
  c("Recreation", 2, 0), 
  c("Utilizaion", 2, 0), 
  c("Wages", 0, 0), 
  c("Carbon", 0, 0), 
  c("Old Growth", 0, 0), 
  c("Clearcutting", 0, 0), 
  c("Public Land", 0, 0), 
  c("Education", 0, 0), 
  c("Public Access", 0, 0), 
  c("Aesthetics", 0, 0)
)

fsc2008 %<>% as.data.frame()
names(fsc2008) <- c("Issues", "Prescriptiveness", "Strictist" )
names(fsc2008)
fsc2008$Program <- "FSC-P&C"
fsc2008$Change <- 0
fsc2008$Year <- 2008

fsc2009 <- mutate(fsc2008, Year = 2009)
fsc2010 <- mutate(fsc2008, Year = 2010)
fsc2011 <- mutate(fsc2008, Year = 2011)



Issues             pres, high, Change
fsc2012 <- rbind(
  c("Forest Extent", 3, 0, 0),
  c("Conservation Value Forests", 3, 1 ,0),
  c("Protected Areas", 3,1,1),
  c("Ecological Function", 3, 0, 0),
  c("Snags and Wood", 3, 0, 0),
  c("Plantations", 3, 1, 0),
  c("Species Diversity", 3, 1, 1),
  c("Species at Risk", 3 , 1, 1),
  c("Genetic Diversity", 3, 1, 0),
  c("GMOs", 3, 0, 0), 
  c("Biotech", 3, 1, 0), 
  c("Restoration", 3, 0, 1), 
  c("Chemical Use", 3, 0, 1), 
  c("Timber Production", 3, 0, -1), 
  c("Non-timber Forest Products", 3, 0, 1), 
  c("Environmental Services", 3, 0, 1), 
  c("Reforestation", 3, 0, 0), 
  c("Tenure", 3, 1, 0), 
  c("Traditional Knowledge", 3, 1, 0), 
  c("Community Benefit", 3, 0, 0), 
  c("Right to Organize", 3, 1, 0), 
  c("Health and Safety", 3, 0, 0), 
  c("Public Reporting & Consultation", 3, 0, 1), 
  c("Economic Viability", 3, 0, 0), 
  c("Forest Law", 3, 0, 0), 
  c("Management Plan", 3, 0, 0), 
  c("Impact Assessment", 3, 1, 0), 
  c("Monitoring", 3, 1, 0), 
  c("Continual Improvement", 3, 0, 0), 
  c("Training", 3, 1, 1), 
  c("Riparian", 3, 0, 0), 
  c("Exotics", 3, 1, 1), 
  c("Natural Disturbances", 3, 0, -1), 
  c("Research", 3, 0, -1), 
  c("Soil", 2, 0, 0), 
  c("Residual Trees", 2, 0, 0), 
  c("Roads", 2, 0, 0), 
  c("Special Sites", 2, 0, -1), 
  c("Recreation", 2, 0, 1), 
  c("Utilizaion", 2, 0, 0), 
  c("Wages", 3, 1, 1), 
  c("Carbon", 3, 1, 1), 
  c("Old Growth", 0, 0, 0), 
  c("Clearcutting", 0, 0, 0), 
  c("Public Land", 0, 0, 0), 
  c("Education", 0, 0, 0), 
  c("Public Access", 0, 0, 0),  
  c("Aesthetics", 0, 0, 0)
)

fsc2012 %<>% as.data.frame()
names(fsc2012) <- c("Issues", "Prescriptiveness", "Strictist", "Change")
names(fsc2012)
fsc2012$Program <- "FSC-P&C"
fsc2012$Year <- 2012

fsc2013 <- mutate(fsc2012, Year = 2013)
fsc2013$Change <- 0
fsc2014 <- mutate(fsc2013, Year = 2014)
fsc2015 <- mutate(fsc2013, Year = 2015)


####################################################


Issues             pres, high
pefc2008 <- rbind(
  c("Forest Extent", 0, 0),
  c("Conservation Value Forests",2, 0),
  c("Protected Areas", 2,0),
  c("Ecological Function", 3, 0),
  c("Snags and Wood", 3, 0),
  c("Plantations", 0, 0),
  c("Species Diversity", 2, 0),
  c("Species at Risk", 2 , 0),
  c("Genetic Diversity", 2, 0),
  c("GMOs", 0, 0), 
  c("Biotech", 2, 0), 
  c("Restoration", 3, 0), 
  c("Chemical Use", 2, 0), 
  c("Timber Production", 3, 0), 
  c("Non-timber Forest Products", 3, 0), 
  c("Environmental Services", 3, 0), 
  c("Reforestation", 3, 0), 
  c("Tenure", 2, 0), 
  c("Traditional Knowledge", 2, 0), 
  c("Community Benefit", 2, 0), 
  c("Right to Organize", 0, 0), 
  c("Health and Safety", 2, 0), 
  c("Public Reporting & Consultation", 0, 0), 
  c("Economic Viability", 3, 0), 
  c("Forest Law", 0, 0), 
  c("Management Plan", 3, 0), 
  c("Impact Assessment", 0, 0), 
  c("Monitoring", 2, 0),
  c("Continual Improvement", 3, 0), 
  c("Training", 2, 0), 
  c("Riparian", 3, 0), 
  c("Exotics", 3, 0), 
  c("Natural Disturbances", 0, 0), 
  c("Research", 0, 0), 
  c("Soil", 3, 1), 
  c("Residual Trees", 3, 1), 
  c("Roads", 3, 1), 
  c("Special Sites", 3, 1), 
  c("Recreation", 3, 1), 
  c("Utilizaion", 3, 1), 
  c("Wages", 0, 0), 
  c("Carbon", 0, 0), 
  c("Old Growth", 0, 0), 
  c("Clearcutting", 0, 0), 
  c("Public Land", 0, 0), 
  c("Education", 0, 0), 
  c("Public Access", 3, 1), 
  c("Aesthetics", 3, 1)
)

pefc2008 %<>% as.data.frame()
names(pefc2008) <- c("Issues", "Prescriptiveness", "Strictist" )
names(pefc2008)
pefc2008$Program <- "PEFC"
pefc2008$Change <- 0
pefc2008$Year <- 2008

pefc2009 <- mutate(pefc2008, Year = 2009)



#Issues             pres, high, Change
# for future, can do sum of Change 1 and Change 2
pefc2010 <- rbind(
  c("Forest Extent", 3, 0, 1),
  c("Conservation Value Forests",2, 0, 1),
  c("Protected Areas", 2, 0, 0),
  c("Ecological Function", 3, 0, 0),
  c("Snags and Wood", 3, 0, 0),
  c("Plantations", 0, 0, 0),
  c("Species Diversity", 2, 0, 0),
  c("Species at Risk", 2 , 0, 1),
  c("Genetic Diversity", 2, 0, 0),
  c("GMOs", 3, 0, 1), 
  c("Biotech", 2, 0, 0), 
  c("Restoration", 2, 0, 0), 
  c("Chemical Use", 3, 0, 1), 
  c("Timber Production", 3, 0, 0), 
  c("Non-timber Forest Products", 3, 0, 1), 
  c("Environmental Services", 3, 0, 0), 
  c("Reforestation", 3, 0, 1), 
  c("Tenure", 2, 0, 1), 
  c("Traditional Knowledge", 2, 0, 0), 
  c("Community Benefit", 3, 0, 1), 
  c("Right to Organize", 2, 0, 1), 
  c("Health and Safety", 3, 0, 1), 
  c("Public Reporting & Consultation", 3, 0, 1), 
  c("Economic Viability", 3, 0, 0), 
  c("Forest Law", 3, 0, 1), 
  c("Management Plan", 3, 0, 1), 
  c("Impact Assessment", 2, 0, 1), 
  c("Monitoring", 2, 0, 0),
  c("Continual Improvement", 3, 0, 0), 
  c("Training", 2, 0, 0), 
  c("Riparian", 3, 0, 0), 
  c("Exotics", 2, 0, 0), 
  c("Natural Disturbances", 3, 1, 1), 
  c("Research", 3, 1, 1), 
  c("Soil", 3, 1, 0), 
  c("Residual Trees", 3, 1, 0), 
  c("Roads", 3, 1, 0), 
  c("Special Sites", 3, 1, 1), 
  c("Recreation", 3, 1, 0), 
  c("Utilizaion", 3, 1, 0), 
  c("Wages", 0, 0, 0), 
  c("Carbon", 0, 0, 0), 
  c("Old Growth", 0, 0, 0), 
  c("Clearcutting", 0, 0, 0), 
  c("Public Land", 0, 0, 0), 
  c("Education", 0, 0, 0), 
  c("Public Access", 3, 1, 0), 
  c("Aesthetics", 3, 1, 0)
)


pefc2010 %<>% as.data.frame()
names(pefc2010) <- c("Issues", "Prescriptiveness", "Strictist", "Change")
names(pefc2010)
pefc2010$Program <- "PEFC"
pefc2010$Year <- 2010

pefc2011 <- mutate(pefc2010, Year = 2011)
pefc2011$Change <- 0
pefc2012 <- mutate(pefc2011, Year = 2012)
pefc2013 <- mutate(pefc2011, Year = 2013)
pefc2014 <- mutate(pefc2011, Year = 2014)
pefc2015 <- mutate(pefc2011, Year = 2015)


d <-rbind(
  fsc2008,
  fsc2009,
  fsc2010,
  fsc2011,
  fsc2012,
  fsc2013,
  fsc2014,
  fsc2015,
  pefc2008,
  pefc2009,
  pefc2010,
  pefc2011,
  pefc2012,
  pefc2013,
  pefc2014,
  pefc2015
)

d$Change %<>% as.numeric()
d$Year %<>% as.numeric()

d %<>% mutate(Increased = ifelse(Program == "FSC-P&C" & Year > 2011, 11, 0))
d %<>% mutate(Increased = ifelse(Program == "PEFC" & Year > 2009, 19, Increased))
d %<>% mutate(Net.Change = ifelse(Program == "PEFC" & Year > 2009, 19, 0))
d %<>% mutate(Net.Change = ifelse(Program == "FSC-P&C" & Year > 2011, 11-4, Net.Change))

write.csv(d, "PEFCvFSC.csv", row.names = F)
