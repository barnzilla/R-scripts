# Create a data structure
beads <- c(rep("red", 2), rep("blue", 3))

# Take a sample from beads
sample(beads, 1)

# Monte Carlo simulation with replicate() (note: selection occurs with replacement, that is, the bead is put back into the urn after selection)
events <- replicate(10000, sample(beads, 1))

# Tabulate our distribution
tab <- table(events)
tab

# Compute proportions
prop.table(tab)

# Monte Carlo simulation with sample()
events <- sample(beads, 10000, replace = TRUE)

# Tabulate our distribution
tab <- table(events)
tab

# Compute proportions
prop.table(tab)

# Among a group of 50 people, what is the probability that 2 people share the same birthday?
events <- replicate(10000, {
	bdays <- sample(1:365, 50, replace = TRUE)
	any(duplicated(bdays))	
})
mean(events)

# Turn this into a function so we can iterate
compute_prob <- function(n, B = 10000) {
	events <- replicate(10000, {
		bdays <- sample(1:365, n, replace = TRUE)
		any(duplicated(bdays))	
	})
	mean(events)

}
compute_prob(50)
n <- seq(1:60)
prob <- sapply(n, compute_prob)
plot(prob)

# Compute exact probability
exact_prob <- function(n) {
	prob_unique <- seq(365, 365 - n + 1) / 365
	1 - prod(prob_unique)
}
exact_prob(50)
eprob <- sapply(n, exact_prob)
plot(prob)
lines(n, eprob, col = "red")

# Monty Hall problem
B <- 10000
# Don't switch doors
stick <- replicate(B, {
	doors <- as.character(1:3)
	prize <- sample(c("car", "goat", "goat"))
	prize_door <- doors[prize == "car"]
	my_pick <- sample(doors, 1)
	show <- sample(doors[! doors %in% c(my_pick, prize_door)], 1)
	stick <- my_pick
	stick == prize_door
})
mean(stick)

# Switch doors
switch <- replicate(B, {
	doors <- as.character(1:3)
	prize <- sample(c("car", "goat", "goat"))
	prize_door <- doors[prize == "car"]
	my_pick <- sample(doors, 1)
	show <- sample(doors[! doors %in% c(my_pick, prize_door)], 1)
	stick <- my_pick
	switch <- doors[! doors %in% c(my_pick, show)]
	switch == prize_door
})
mean(switch)