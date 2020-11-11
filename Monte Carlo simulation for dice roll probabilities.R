# Set samples (number of dice rolls) to run
n <- 100000

# Roll dice 1
dice1 <- sample(x = 1:6, size = n, replace = TRUE)

# Roll dice 2
dice2 <- sample(x = 1:6, size = n, replace = TRUE)

# Sum the dice rolls by sample
total <- dice1 + dice2

# Compute the proportion (probability) of dice rolls summing to less than 7
sum(total < 7) / n

# Compute the proportion (probability) of dice rolls summing to greater than 6
sum(total > 6) / n
