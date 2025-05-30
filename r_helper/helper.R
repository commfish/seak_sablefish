
# Libraries, ggplot themes, and user-defined fxns.
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov (ummjane@gmail.com)
# Last edited: June 2020

# NOTES
# For changing figure themes, user must manually comment out theme_set() when
# developing figs for text vs presentations

options(scipen = 999) # turn off scientific notation


# libraries ----

if(!require("mosaic"))   install.packages("mosaic") # derivedFactor, derivedVariable. Masks over a lot of fxns, but generally only improves their utility
if(!require("tidyverse"))   install.packages("tidyverse") # dplyr, ggplot, etc.
if(!require("tidyr"))   install.packages("tidyr") 
if(!require("lubridate"))   install.packages("lubridate") # dates functions like yday, dmy/mdy
if(!require("mgcv"))   install.packages("mgcv") # gams
if(!require("gridExtra"))   install.packages("gridExtra") # multipanneled plots
if(!require("data.table"))   install.packages("data.table") # dcast, foverlaps
if(!require("broom"))   install.packages("broom") # tidying regression model output
if(!require("padr"))   install.packages("padr") # fills in missing values in a time series
if(!require("tidyr"))   install.packages("tidyr") # reshaping data
if(!require("knitr"))   install.packages("knitr") # r markdown
if(!require("forcats"))   install.packages("forcats") # releveling factors
if(!require("cowplot"))   install.packages("cowplot") # plot_grid and so much else
if(!require("zoo"))   install.packages("zoo") # interpolation
if(!require("ggridges"))   install.packages("ggridges") # used to visualize length comps
if(!require("ggthemes"))   install.packages("ggthemes") # color palettes, ggthemes

# install.packages("devtools")
# devtools::install_github("ben-williams/FNGr")
# library("FNGr")

# ggplot themes ----

windowsFonts(Times=windowsFont("Times New Roman"))

theme_sleek <- function(base_size = 12, base_family = "Times") {
  half_line <- base_size/2
  theme_light(base_size = 12, base_family = "Times") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
}

theme_set(theme_sleek())

theme_present <- function(base_size = 16) {
  half_line <- base_size/2
  theme_light(base_size = 16) +
    theme(
      # axis.title.y = element_text(angle=0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = unit(0.9, "lines"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,

    )
}
# theme_set(theme_present())

theme_set(theme_bw(base_size = 12)) #, base_family = "Times"
