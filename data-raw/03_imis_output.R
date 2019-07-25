## code to prepare `03_imis_output` dataset goes here
load(file = "output/03_imis_output.RData")

# Create .rda object for each component in 03_imis_output.RData
usethis::use_data(m_calib_post,
                   v_calib_post_map)
