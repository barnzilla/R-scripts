# Load packages that extend base R
package_names <- c("vtree")
load_packages <- lapply(package_names, require, character.only = TRUE)

# Import data
d <- read.csv(file.choose(), header = TRUE, sep = ",")

# Let's play with the vtree function
vtree(d, "sex")
vtree(d, c("sex", "age_years"), horiz = FALSE)
