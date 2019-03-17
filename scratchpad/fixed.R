data %<>% 
  mutate(Issue = as.factor(Issue))


# tile plot (tile)
data %>% 
  filter(Program %in% c("Activist-backed FSC-US", "Industry-backed SFI")) %>%
  mutate(Issue = as.factor(Issue)) %>%
  mutate(Change = ifelse(Change == "No change", NA, Change)) %>% 
  ggplot() +
  facet_grid(. ~ Program) +
  geom_tile(aes(x = Year, 
                y = Issue, 
                fill = Prescriptiveness), color = "white") + 
  scale_fill_manual(values = c("white", "light grey","dark grey")) + 
  # scale_shape_manual(values=c("-", "+", ""))+
  ## Add NE arrows
  geom_segment(  aes(x = Year -.5, 
                     y = as.numeric(Issue) -.4,
                     yend = ifelse(Change == "Became more prescriptive", as.numeric(Issue) + .4, NA),
                     xend = ifelse(Change == "Became more prescriptive", Year + .5, NA),
                     color = Change), arrow = arrow(length = unit(0.2,"cm")), na.rm = T )+
  geom_segment(  aes(x = Year -.5, 
                     y = as.numeric(Issue) +.4,
                     yend = ifelse(Change == "Became less prescriptive", as.numeric(Issue) - .4, NA),
                     xend = ifelse(Change == "Became less prescriptive", Year + .5, NA),
                     color = Change), arrow = arrow(length = unit(0.2,"cm")), na.rm = T )+
  scale_color_brewer(breaks=c("Became more prescriptive", "Became less prescriptive"),type = "qual", palette = 6) + 
  #labs(title = "Scope of Key Issues Addressed") +  adkfthadht 
  # scale_y_discrete(limits = rev(levels(data$Issue) ) ) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #remove legend title
        legend.title=element_blank(),
        #set a slim legend
        # legend.key.width=grid::unit(0.2,"cm"),
        axis.title = element_blank(),
        #change axis ticks thickness
        axis.ticks=element_blank(),
        # axis.text.x=element_text(angle = 45, vjust = 1.3, hjust = 1),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())












 


# STACK OVERFLOW EXAMPLE 

library(dplyr)
library(ggplot2)
library(magrittr)
d <- tibble(year = c(1, 1, 2, 2),
       policy = rep( c('policy 1', 'policy 2'), 2),
       prediction = c(NA, 'increase', 'decrease', NA),
       predictionUnicode = c(NA, '\u2197', '\u2198', NA))

ggplot(d) + 
  geom_point(aes(x = year, y = policy, color = prediction, shape = predictionUnicode))

Sys.setenv(LANG = "Unicode")
Encoding('ʘ')
library(Unicode)

ggplot(d) + 
  geom_tile( aes(x = year, y = policy), color = "black", fill = "white") +   
  # geom_point does not allow new fonts?
  geom_point(aes(x = year, y = policy, 
                 color = prediction), shape = "\u2198") + 
  # geom_text does allow new fonts, but the legend text is fixed to "a"
  geom_text(aes(x = year, y= policy, 
                color = prediction,
                label = predictionUnicode), 
            family = "Calibri") + 
  scale_x_continuous(breaks = c(1,2)) +
  theme_gray(base_family = "Calibri") 


ggsave(p, filename = "whatever.pdf", device = cairo_pdf)
cairo_pdf("p2.pdf", family="Arial Unicode MS", 4,4)
p2
dev.off()

d %<>%
  mutate(policy = as.factor(policy)) %>% 
  mutate(ybegin = ifelse(prediction == "increase", as.numeric(policy)- .4, NA)) %>% 
  mutate(ybegin = ifelse(prediction == "decrease", as.numeric(policy)+ .4, ybegin)) %>% 
  mutate(yend = ifelse(prediction == "increase", as.numeric(policy)+ .4, NA)) %>% 
  mutate(yend = ifelse(prediction == "decrease", as.numeric(policy)- .4, yend)) 

ggplot(d) +
  geom_tile( aes(x = year, y = policy), color = "black", fill = "white") +   
  ## Add NE arrows
  geom_segment(  aes(x = year -.4, 
                     xend = year +.4,
                     y = ybegin,
                     yend = yend,
                     color = prediction),
                 arrow = arrow(length = unit(0.2,"cm"))) + 
  scale_color_brewer(breaks=c("increase", "decrease"),type = "qual") + 
  scale_x_continuous(breaks = c(1,2))
  





ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape="\u25D2", colour="red", size=3) +
  geom_point(shape="\u25D3", colour="blue", size=3) +
  geom_point(shape=1, colour="white", size=3.05) +
  geom_point(shape=1, colour="white", size=2.8) +
  theme_bw()

p2 = ggplot(mtcars) +
  geom_point(aes(wt-0.027, mpg), shape=d1, colour="red", size=3) +
  geom_point(aes(wt+0.027, mpg), shape="\u2198", colour="blue", size=3) +
  theme_bw()


cairo_pdf("p1.pdf", family="Symbola", 4,4)
p1
dev.off()

cairo_pdf("p2.pdf", family="Symbola", 4,4)
p2
dev.off()


data <- data.frame(
  val = rnorm(40),
  grp = c(rep('a', 20), rep('b', 20)),
  x   = rep(letters[1:4], 5),
  y   = rep(letters[1:5], 4)
)

p <- ggplot(data, aes(x = x, y = y, color = val, shape = grp)) +
  geom_point(size = 18) +
  scale_shape_manual(values=c("\u25E4","\u25E2")) +
  theme_minimal() +
  theme(panel.grid = element_blank())
p



library(ggplot2)

w6_2 <- sample(c(1:6), 2000, replace = T) + sample(c(1:6), 2000, replace = T)

# see stringi::stri_escape_unicode
d1 <- "\u2680"
d2 <- "\u2681"
d3 <- "\u2682"
d4 <- "\u2683"
d5 <- "\u2684"
d6 <- "\u2685"

dice <- c(
  "2"  = paste0(d1, d1),
  "3"  = paste0(d1, d2, "\n", d2, d1),
  "4"  = paste0(d2, d2, "\n", d1, d3, "\n", d3, d1),
  "5"  = paste0(d1, d4, "\n", d2, d3, "\n", d3, d2, "\n", d4, d1),
  "6"  = paste0(d1, d5, "\n", d2, d4, "\n", d3, d3, "\n", d4, d2, "\n", d5, d1),
  "7"  = paste0(d1, d6, "\n", d2, d5, "\n", d3, d4, "\n", d4, d3, "\n", d5, d2, "\n", d6, d1),
  "8"  = paste0(d2, d6, "\n", d3, d5, "\n", d4, d4, "\n", d5, d3, "\n", d6, d2),
  "9"  = paste0(d3, d6, "\n", d4, d5, "\n", d5, d4, "\n", d6, d3),
  "10" = paste0(d4, d6, "\n", d5, d5, "\n", d6, d4),
  "11" = paste0(d5, d6, "\n", d6, d5),
  "12" = paste0(d6, d6)
)

ggplot(NULL, aes(x = w6_2)) +
  geom_histogram(bins = 11, color = "white", fill = "#1aadff", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Distribution of Dice Rolls",
       subtitle = "After 2000 Rolls with 2 Dice",
       x = "Combinations (row-wise)", y = "Count") +
  theme_minimal()

ggplot(NULL, aes(x = w6_2)) +
geom_histogram(bins = 11, color = "white", fill = "blue", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Distribution of Dice Rolls",
       x = "Combinations (row-wise)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = rel(2), family = "FreeSerif", lineheight = .5))

plot(x = c(1,2,3), y = c(1,2,3))
points(x = c(1,2,3), y = c(1,2,3), pch = "2299")
"\u2685"

plot(0)


plot(1, xlab=iconv('ʘ', from='latin1', to="UTF-8"))


library(extrafont)
font_import()
fonts()

# Then find a font that has the unicode: 
# https://indesignsecrets.com/find-font-glyph.php


# ↘︎
# SOUTH EAST ARROW
# Unicode: U+2198 U+FE0E, UTF-8: E2 86 98 EF B8 8E

Arial Unicode MS
Batang
Calibri
Cambria
Gulim


Sys.setenv(LANG = "Unicode")

ggplot(NULL) + 
  geom_point(aes(x = 0, y = 0), shape = "\u2198", size = 10) + 
  geom_text(aes(x = 0, y=1), label = "\u2198", size = 10, family = "Calibri") + 
  labs(x = "\u2198") + 
  theme_gray(base_family = "Calibri") 

fonts <- fonts()
for(i in 1:length(fonts) ){
  tryCatch({
  ggplot(NULL) + 
    geom_point(aes(x = 0, y = 0), shape = "\u2198", size = 10) + 
      geom_text(aes(x = 0, y=1), label = "\u2198", size = 10)
    theme_gray(base_family = fonts[i])
  }, error=function(e){})
  tryCatch({
  ggsave(filename = paste0("test",i,".png"))
}, error=function(e){})
}

arrow <-   "\u2198"

ggplot(NULL) + 
  geom_text(aes(x = 0, y=0, label = arrow, color = arrow), size = 10, family = "Calibri") + 
  geom_text(aes(x = 0, y=0, label = arrow), size = 10, family = "Calibri") + 
  theme_gray(base_family = "Calibri") 

ggplot(NULL) + 
  geom_text(aes(x = 0, y=0), label = "\u2198", size = 10, family = "Calibri")

ggplot(NULL) + 
  geom_point(aes(x = 0, y=0, shape = "\u2198"), family = "Calibri")

# ↗︎
# NORTH EAST ARROW
# Unicode: U+2197 U+FE0E, UTF-8: E2 86 97 EF B8 8E









# Thanks to djangodude's comment about ggplot's font usage, I found the `family` argument of `geom_text`, which allows different fonts. Thus, Unicode "shapes" can be plotted as characters with `geom_text`. However, the legend for geom text is [fixed at a](https://github.com/tidyverse/ggplot2/issues/2004)

### To get the right font:
1. Look for an installed font that contains the Unicode character you seek. I found [these instructions](https://indesignsecrets.com/find-font-glyph.php) helpful. 

2. Import installed fonts into R
```
library(extrafont)
font_import()
fonts()
```

### To plot a SOUTH EAST ARROW, Unicode: U+2198:
```
ggplot(NULL) + 
  geom_text(aes(x = 0, y=0), label = "\u2198", size = 10, family = "Calibri") + 
  theme_gray(base_family = "Calibri") 
```
Note: these solutions for Unicode [skull and crossbones](https://stackoverflow.com/questions/54803891/custom-shape-in-ggplot-geom-point) and [half-filled points](https://stackoverflow.com/questions/25053072/how-to-draw-half-filled-points-in-r-preferably-using-ggplot/53129919#53129919) also failed to render for me. 

                                                                                                                                                                  A font `family` argument for `shape` in `geom_point` (like `family` in `geom_text`) could vastly expand available glyphs to Unicode characters missing from the default font. Alternatively, `base_family` in complete themes could be used when Unicode is supplied to the `shape` scale. While `geom_text` plots Unicode, the text legend does not behave like the shape legend, and a more versatile shape scale seems better than a more versatile text legend.
                                                                                                                                                                  
                                                                                                                                                                  The example uses the "Calibri" font for Unicode missing from my base font:
                                                                                                                                                                    ```{r}
                                                                                                                                                                  library(extrafont)
                                                                                                                                                                  font_import()
                                                                                                                                                                  glyphs <- c('\u2197', '\u2198')
                                                                                                                                                                  p <- ggplot(NULL, aes(x = 0, y=c(1,2), color = glyphs))
                                                                                                                                                                  ```
                                                                                                                                                                  
                                                                                                                                                                  `geom_text` plots Unicode symbols from Calibri, but only "a" in the legend. 
                                                                                                                                                                  ```{r}
                                                                                                                                                                  p +
                                                                                                                                                                    geom_text( aes(label = glyphs), family = "Calibri")
                                                                                                                                                                  ```
                                                                                                                                                                  
                                                                                                                                                                  `geom_point` seems like the right way to plot Unicode glyphs, if only it and/or `scale_shape` had a `family` argument like this:
                                                                                                                                                                    ```{r}
                                                                                                                                                                  p +
                                                                                                                                                                    geom_point( aes(shape = glyphs), family = "Calibri") +
                                                                                                                                                                    scale_shape_manual(values = glyphs)
                                                                                                                                                                  ```
                                                                                                                                                                  
                                                                                                                                                                  Or if `base_family` applied to Unicode supplied to the `shape` scale:
                                                                                                                                                                    ```{r}
                                                                                                                                                                  p + 
                                                                                                                                                                    geom_point( aes(shape = glyphs)  ) +
                                                                                                                                                                    scale_shape_manual(values = glyphs) + 
                                                                                                                                                                    theme_gray(base_family = "Calibri") 
                                                                                                                                                                  ```
                                                                                                                                                                  
                                                                                                                                                                  
                                                                                                                                                                  This idea is a result of failing to find solutions that include a legend for extra font glyph points for these stack overflow questions:
                                                                                                                                                                    https://stackoverflow.com/questions/52902946/using-unicode-characters-as-shape
                                                                                                                                                                  https://stackoverflow.com/questions/48531257/ggplot-geom-point-how-to-set-font-of-custom-plotting-symbols
                                                                                                                                                                  https://stackoverflow.com/questions/34637063/black-star-symbol-for-geom-point-in-ggplot2
                                                                                                                                                                  https://stackoverflow.com/questions/25053072/how-to-draw-half-filled-points-in-r-preferably-using-ggplot/53129919#53129919
                                                                                                                                                                  https://stackoverflow.com/questions/54870912/ggplot-shape-unicode-e-g-arrows-like-u2198-or-latex-searrow/54873915?noredirect=1#comment96525295_54873915
                                                                                                                                                                  
                                                                                                                                                                  I apologize if I missed an existing method. Thank you for this amazing tool! 