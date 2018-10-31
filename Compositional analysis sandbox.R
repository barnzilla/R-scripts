# Load packages to extend base R
library("compositions")

# Import SimpleData.txt
kitchen <- read.table("http://www.stat.boogaart.de/compositionsRBook/SimpleData.txt", sep = "")

# Close the amounts in the parts of interest
coi <- clo(kitchen, parts = c("Fat", "Carbonates", "Protein"), total = 100)

# Alternate approach
amounts <- kitchen[, c("Fat", "Carbonates", "Protein")]
sumPresent <- totals(rplus(amounts))

# Create new dataset with new column
Other <- kitchen[,c("total")] - sumPresent
Amounts <- cbind(amounts, Other)
clo(Amounts)

# Demonstrating compositional equivalence
Amounts[4, ]
Amounts[4,] * 100 / 82
acomp(Amounts[4,])