## code to prepare `all_cause_mortality` dataset goes here
file.init <- "data-raw/01_all_cause_mortality.csv"

all_cause_mortality  <- read.csv(file = file.init, stringsAsFactors = F)

# Create .rda object for initial set of parameters and store it in 'data' folder
usethis::use_data(all_cause_mortality)
