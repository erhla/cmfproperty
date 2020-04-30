
#fred code
#from https://fred.stlouisfed.org/series/CPIHOSNS

file_loc <- "~/../Downloads/CPIHOSNS.csv"

fred <- readr::read_csv(file_loc)
usethis::use_data(fred, internal=TRUE, overwrite=TRUE)

#see README.Rmd
library(data.table)
library(tidyverse)

sales <- fread("~/../Downloads/Cook_County_Assessor_s_Residential_Sales_Data.csv", colClasses = "character") #from 2013 to 2019
assessments <- fread("~/../Downloads/Cook_County_Assessor_s_Residential_Assessments.csv", colClasses = "character") #from 2015 to 2019

sales <- sales %>% select(PIN, `Sale Year`, `Sale Price`, `Deed No.`) %>% filter(`Sale Year` > 2014)
assessments <- assessments %>% select(PIN, YEAR, CERTIFIED)

# Filtering data to remove duplicate sales and low value sales
sales <- sales %>% distinct(`Deed No.`, .keep_all = TRUE) %>% select(-`Deed No.`)
sales <- sales %>% filter(as.numeric(`Sale Price`) > 2500)

# Join assessments to sales based on PIN (a unique identifier) and Year.
joined <- sales %>% left_join(assessments, by=c("PIN"="PIN", "Sale Year"="YEAR"))
joined <- joined %>% rename(SALE_YEAR = `Sale Year`, SALE_PRICE = `Sale Price`, ASSESSED_VALUE = CERTIFIED)

fwrite(joined, paste0("~/../Downloads/example_data.csv"))

example_data <- fread("~/../Downloads/example_data.csv", colClasses = c("character", "integer", "integer", "integer"))

usethis::use_data(example_data, internal=FALSE, overwrite=TRUE)
