# Install package
install.packages("read.gt3x")

# Load package
library(read.gt3x)

# Get path to sample accelerometer data
gt3x_file <- system.file(
  "extdata",
  "TAS1H30182785_2019-09-17.gt3x",
  package = "read.gt3x"
)

# More data available here
#gt3x_file <- gt3x_datapath(1)

# Import sample accelerometer data
d <- read.gt3x(gt3x_file)

# Convert data to a data frame
d <- as.data.frame(d)

# View the data
# The x vector represents right-left
# The y vector represents forward-backward
# The z vector represents up-down
View(d)
