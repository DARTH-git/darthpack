### Closely following: https://rtask.thinkr.fr/blog/rmd-first-when-development-starts-with-documentation/

# Ignore this file for the git syncing
usethis::use_build_ignore("dev_history.R")

# Add code fo conduct
usethis::use_code_of_conduct()

# Add a vignette
usethis::use_vignette("aa-model")

# Create .rda object for initial set of parameters
file.init <- "data/01_init_params.csv"
df_params_init  <- read.csv(file = file.init, stringsAsFactors = F)
save(file = "data/01_init_params.rda", df_params_init)

# Create .rda object for calibrated parameters
load(file = "output/03_imis_output.RData")
devtools::use_data(m_calib_post,
                   v_calib_post_map)

# Create .rda object for PSA dataset
load(file = "output/05b_psa_dataset.RData")
devtools::use_data(l_psa)

# create test file
usethis::use_test("test_filters")

# Document functions and dependencies
attachment::att_to_description()

# Check the package
devtools::check()

# Install chameleon package
devtools::install_github("ThinkR-open/chameleon")
# Build a book from vignettes inside a package
template <- system.file("rstudio/templates/project/resources", package = "bookdown")
chameleon::create_book(path = "report/", clean = TRUE,
            template = template)
chameleon::open_guide_function(path = "report/")
# Create documentation of package on line
chameleon::build_pkgdown()
