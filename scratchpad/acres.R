# install.packages("tidyverse")
# install.packages("magrittr")
options(stringsAsFactors = FALSE)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
library(gtable)

getwd()
setwd("/Users/Patron/Downloads")
setwd("/Users/user/Downloads")

acres <- read.csv("acres.csv")
  
  ggplot(acres) + 
  geom_col(aes(y = Acres, x = bar, fill = color)) + 
    coord_flip() + 
    labs(y = "Acres (Millions)", x = "") + 
    scale_y_continuous(breaks = seq(0,140, 20)) +
    theme(legend.title=element_blank(),
      #legend.key.width=grid::unit(0.2,"cm"),
      axis.title.y = element_blank(),
      #change axis ticks thickness
      axis.ticks.y=element_blank()) # + scale_fill_grey()
  
  
  cert <- ggplot(acres %>% filter(bar != "Total Corporate-owned timberland",
                                  bar != "Family forest (managed)",
                                  bar != "Family forest (non-corporate)",
                                  bar != "Family forest (unmanaged)")) + 
    geom_col(aes(y = Acres, x = bar, fill = color)) + 
    coord_flip() + 
    labs(y = "Acres (millions)", x = "") + 
    scale_fill_grey(name = "Global Label") + 
    scale_y_continuous(breaks = seq(0, 150, 30), limits = c(0,155)) +
    theme(axis.title.y = element_blank(),
          legend.background = element_blank(),
          #change axis ticks thickness
          axis.ticks.y=element_blank())  
  
  corp <- ggplot(acres %>% filter(bar %in% c("Total Corporate-owned timberland",
                                  #"Family Forest (Managed)",
                                  "Family forest (non-corporate)")))+
    geom_col(aes(y = Acres, x = bar, color = color, linetype = color), fill = "white") + 
    coord_flip() + 
    labs(y = "Acres (millions)", x = "") + 
    scale_y_continuous(breaks = seq(0, 150, 30), limits = c(0,155)) +
    theme(legend.title=element_blank(),
          #legend.key.width=grid::unit(0.2,"cm"),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          #change axis ticks thickness
          axis.ticks.y=element_blank())  + scale_fill_grey()
  
  pdf("acres.pdf",width=6.5,height=2.25,paper='special') 
  #grid.newpage()
  grid.draw(egg::ggarrange(corp, cert, 
                           ncol = 1,
                           #common.legend = T,
                           #legend = "right",
                           heights = c(.5, 1)))       

 dev.off()
 
 
 
 
 
 
 
 # acres 2 (witout family forests )
 
 corp <- ggplot(acres %>% filter(bar %in% c("Total Corporate-owned timberland"))) + 
   #"Family Forest (Managed)",
   #"Family Forest (Non-corporate)"
   geom_col(aes(y = Acres, x = bar, color = color, linetype = color), fill = "white") + 
   coord_flip() + 
   labs(y = "Acres (millions)", x = "") + 
   scale_y_continuous(breaks = seq(0, 150, 30), limits = c(0,155)) +
   theme(legend.title=element_blank(),
         #legend.key.width=grid::unit(0.2,"cm"),
         axis.title = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         #change axis ticks thickness
         axis.ticks.y=element_blank())  + scale_fill_grey()
 
 pdf("acres2.pdf",width=6.5,height=2,paper='special') 
 #grid.newpage()
 grid.draw(egg::ggarrange(corp, cert, 
                          ncol = 1,
                          #common.legend = T,
                          #legend = "right",
                          heights = c(.24, 1)))
 dev.off()

  
  
if(F){
  504 mil timberland 
  
  .22 federal
  .20 private industrial 
  
  504*.2
  504*.14
  U.S. timberland ownership is 59 percent private non-industrial, 14 percent forest industry, 10 percent national forest, and 17 percent other public.
  
  Some 36 million acres of U.S. timberlands are reserved for non-timber uses through special legislation.
  
  
  337.7 private 2016 https://www.forest2market.com/hubfs/Blog/Forest2Market_Economic_Impact_of_Privately-Owned_Forests_April_2016.pdf?t=1518187820363
  
  Corporate owners own the other third
  of the private forest land (138 million acres or 18 percent of
                              all forest land) 2006
  https://www.fs.fed.us/nrs/pubs/gtr/gtr_wo78.pdf
  
  http://forisk.com/blog/2017/05/04/forisk-forecast-tracking-top-timberland-owners-managers-u-s-canada-2017-update/
}