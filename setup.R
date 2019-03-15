options(stringsAsFactors = FALSE)

requires <- c("tidyverse", 
              "magrittr",
              "scales",
              "magrittr",
              "here",
              "gridExtra",
              "egg",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(tidyverse)
library(dplyr) # in case tydyverse fails (problem on linux)
library(ggplot2); theme_set(theme_minimal())
library(magrittr)
library(scales)
library(here)
library(grid)
library(gridExtra)
library(gtable)
library(egg)
library(glue)

knitr::opts_chunk$set(echo = TRUE, 
                      cache = FALSE, 
                      include =FALSE,
                      fig.width=8.5, fig.align = 'center', fig.path='Figs/',
                      warning=FALSE, message=FALSE)

