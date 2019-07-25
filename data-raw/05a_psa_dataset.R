## code to prepare `05a_psa_dataset` dataset goes here
load(file = "output/05a_psa_dataset.RData")

# Create .rda object with the PSA object from dampack
usethis::use_data(l_psa)