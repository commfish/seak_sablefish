
# Libraries, ggplot themes, and user-defined fxns.
# Author: Jane Sullivan
# Contact: jane.sullivan1@alaska.gov
# Last edited: 2017-10-03

# libraries ----

if(!require("mosaic"))   install.packages("mosaic") # derivedFactor, derivedVariable. Masks over a lot of fxns, but generally only improves their utility
if(!require("tidyverse"))   install.packages("tidyverse") # dplyr, ggplot, etc.
if(!require("tidyr"))   install.packages("tidyr") 
if(!require("lubridate"))   install.packages("lubridate") # dates functions like yday, dmy/mdy
if(!require("mgcv"))   install.packages("mgcv") # gams
if(!require("gridExtra"))   install.packages("gridExtra") # multipanneled plots
if(!require("data.table"))   install.packages("data.table") # dcast, foverlaps
if(!require("ROracle"))   install.packages("ROracle") # database access through R

# ggplot themes ----

theme_set(theme_bw(base_size=12)+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# # basic eda (exporatory data analysis theme)
# eda_theme <- theme_classic() +
#   theme(panel.grid.major = element_line(colour="grey95"),
#         panel.grid.minor = element_line(colour="grey95"),
#         strip.text = element_text(size=10),
#         axis.text.x = element_text(size=10, angle=45, hjust=1),
#         axis.text.y = element_text(size=10),
#         axis.title.x = element_text(size=10),
#         axis.title.y = element_text(size=10),
#         legend.background = element_rect(fill = "transparent", colour = NA))

# # eda theme for faceted figs
# eda_facet <- theme_bw() + 
#   theme(strip.text = element_text(size=10),
#         axis.text.x = element_text(size=10, colour="black", angle=45, hjust=1),
#         axis.text.y = element_text(size=10, colour="black"),
#         axis.title.x = element_text(size=10, colour="black"),
#         axis.title.y = element_text(size=10, colour="black"),
#         plot.background = element_rect(fill = "transparent", colour = NA),
#         legend.background = element_rect(fill = "transparent", colour = NA)) 
# 
# # mapping theme
# map_theme <- theme(
#   plot.title = element_text(size = rel(1.5)),
#   axis.line=element_blank(),
#   axis.text.x=element_blank(),
#   axis.text.y=element_blank(),
#   axis.ticks=element_blank(),
#   axis.title.x=element_blank(),
#   axis.title.y=element_blank(),
#   #legend.position="bottom",
#   panel.background=element_blank(),
#   panel.border=element_blank(),
#   panel.grid.major=element_blank(),
#   panel.grid.minor=element_blank(),
#   #legend.text=element_text(size = rel(1.5)),
#   plot.background=element_blank())
# 
# # barplot theme
# bar_theme <- theme_bw() + 
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text.x=element_text(angle = 45, hjust = 1), #angle = 45
#         #axis.title.y=element_text(angle=0),
#         panel.background = element_blank())
# 
# # closer to publication-style theme
# report_theme <- theme_bw() +
#   theme(text = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         strip.text.x = element_text(size = 14),
#         axis.line = element_line(size = 0.5, color = "black"),
#         legend.position = "bottom")