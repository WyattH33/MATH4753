## data-raw/fire.R  (run this script to create data/fire.rda)

# 1) Read the CSV (change the path if your file is elsewhere)
fire <- read.csv("FIREDAM.csv", stringsAsFactors = FALSE)

# 2) Quick sanity check (optional)
str(fire)
head(fire)

# 3) Save as .rda into data/ (created automatically)
usethis::use_data(fire, overwrite = TRUE)
## code to prepare `DATASET` dataset goes here


