# Load packages
library(dplyr); library(tidyr)

# Create fake data
d <- tibble(
  id = c(rep(2, 2), rep(1, 2)),
  email = c(rep("jb@barnzilla.ca", 2), rep("mg@barnzilla.ca", 2)),
  module = c("brain health module", "safety module", "brain health module", "safety module"),
  score = c(1:4)
)

# View fake data
View(d)

# Reshape fake data from long to wide format
d_reshaped <- d %>% pivot_wider(id_cols = c(id, email), names_from = module, values_from = score)

# View reshaped fake data
View(d_reshaped)
