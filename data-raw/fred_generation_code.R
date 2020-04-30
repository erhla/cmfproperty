#from https://fred.stlouisfed.org/series/CPIHOSNS

file_loc <- "~/../Downloads/CPIHOSNS.csv"

fred <- readr::read_csv(file_loc)
usethis::use_data(fred, internal=TRUE, overwrite=TRUE)
