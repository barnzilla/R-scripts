# Load packages to extend base R
library("ggplot2")

# Create data structure
d <- data.frame(
  sleep <- c(14.6, 13.6, 12.9, 12.6, 12.9, 12.6, 12, 11.5, 9.7, 9.4, 9.3, 9.3, 9.1, 9, 8.9),
  age <- c("0-2 m", "3 m",  "6 m",  "9 m",  "12 m", "1-2 y", "2-3 y", "4-5 y", "6 y", "7 y", "8 y", "9 y",  "10 y", "11 y", "12 y")
)

# Create line plot 
ggplot(data = d, aes(x = 1:length(age), y = sleep)) + 
  geom_point(size = 2, colour = "#484848") + 
  labs(x = "Age", y = "Sleep duration (hours/day)") + 
  scale_x_continuous(labels = d$age, breaks = 1:15) + 
  geom_smooth(method = "loess", se = TRUE, colour = "#3366ff")



