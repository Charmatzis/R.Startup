# Poker winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)

# Roulette winnings from Monday to Friday
roulette_vector <- c(-24, -50, 100, -350, 10)

# Add names to both poker_vector and roulette_vector
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- c(days)
names(roulette_vector) <- c(days)

poker_vector1 <- c(140, -50, 20, -120, 240)
names(poker_vector1) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

poker_vector2 <- c(Monday = 140, -50, 20, -120, 240)

roulette_vector1 <- c(-24, -50, 100, -350, 10)
days_vector <- names(poker_vector1)
names(roulette_vector1) <- days_vector

roulette_vector2 <- c(-24, -50, 100, -350, 10)
names(roulette_vector2) <- "Monday"
length(poker_vector1)
length(poker_vector2)


# Casino winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)
roulette_vector <- c(-24, -50, 100, -350, 10)
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days_vector
names(roulette_vector) <- days_vector

# Select amounts for profitable roulette days: roulette_profits
roulette_profits <- roulette_vector[roulette_vector > 0]

# Sum of the profitable roulette days: roulette_total_profit
roulette_total_profit <- sum(roulette_profits)

# Number of profitable roulette days: num_profitable_days
num_profitable_days <- sum(roulette_vector > 0)
num_profitable_days