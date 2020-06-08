# configure data directories
# source base functions
# load libraries
#######################################
library(here)
library(ggplot2)
library(data.table)
library(plotly)
library(magrittr)
library(knitr)
library(httr)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(tidyverse)
library(zip)
library(scales)
library(lubridate)
library(pander)
library(ggrepel)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(growthstandards)

# define directories
data_dir <- paste0(here::here(), "/monitoring/data/raw/")
out_dir_w <- paste0(here::here(),"/monitoring/output/weekly_sendout/")
out_dir_d <- paste0(here::here(),"/monitoring/output/daily_sendout/")
code_dir <- paste0(here::here(), "/monitoring/code/functions/")
code_dir_main <- paste0(here::here(), "/monitoring/code/scripts/")
tab_dir <- paste0(here::here(), "/monitoring/tables/")

# source base functions