## code to prepare `01_init_params` dataset goes here
file.init <- "data-raw/01_init_params.csv"

df_params_init  <- read.csv(file = file.init, stringsAsFactors = F)

# Create .rda object for initial set of parameters and store it in 'data' folder
usethis::use_data(df_params_init)
