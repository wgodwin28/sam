##Purpose: Knit and render markdown files of daily and weekly reports for SAM trial
# clear memory
#source("~/Documents/sam/monitoring/code/scripts/00_render_all.R")
rm(list=ls())

#load library and directory
library(rmarkdown)
#setwd("~/Documents/sam")
day <- weekdays(Sys.Date())
date <- format(Sys.Date(), "%d%b%Y")
day <- "Friday"

#get pandoc directory
pan.dir <- Sys.getenv("RSTUDIO_PANDOC")
Sys.setenv(RSTUDIO_PANDOC=pan.dir)

# configure directories, load libraries and base functions
## NOTE: in order for this to work from terminal window, you must open R 
## after you've navigated to project directory
source(paste0(here::here(), "/monitoring/code/functions/config.R"))

#make necessary directories
dir.create(out_dir_w, recursive = T, showWarnings = F)
dir.create(out_dir_d, recursive = T, showWarnings = F)
dir.create(data_dir, recursive = T, showWarnings = F)

if(day == "Friday"){
  #weekly report
  rmarkdown::render(paste0(code_dir_main, "01_weekly_report.rmd"),
                    output_format ="pdf_document",
                    output_file = paste0(out_dir_w, "sam_daily_report_", date),
                    envir = new.env())
}else{
  #daily report
  rmarkdown::render(input = paste0(code_dir_main, "05_malaria_report.Rmd"),
                    output_format ="pdf_document",
                    output_file = paste0(out_dir_d, "sam_malaria_report_", date),
                    #envir = new.env(),
                    params = list(study="g"))
}