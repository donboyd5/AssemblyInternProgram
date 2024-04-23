library(rlang)
library(tidyverse)
tprint <- 75  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

library(fs)
library(googlesheets4)

# tools
library(vroom)
library(readxl)
# library(openxlsx) # for writing xlsx files
# library(lubridate)
# library(RColorBrewer)
# library(RcppRoll)
# library(fredr)
# library(tidycensus)
# library(skimr)
# 
# # boyd libraries
# library(btools)
# library(bdata)
# library(bggtools)
# library(bmaps)
# 
# # graphics
# library(scales)
# library(ggbeeswarm)
# library(patchwork)
# library(gridExtra)
# library(ggrepel)
# library(ggbreak)
# 
# # tables
# library(knitr)
# library(kableExtra)
# library(DT)
# library(gt)
# library(gtExtras)
# library(janitor)
