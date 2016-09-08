box <- c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)


matrix(box, ncol = 2, byrow = TRUE)

new_hope <- c(460.998, 314.400)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.800)


# option A
star_wars_matrix <- rbind(new_hope, empire_strikes, return_jedi)
rownames(star_wars_matrix) <- c("US", "non-US")
colnames(star_wars_matrix) <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")

# option B
col <- c("US", "non-US")
row <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
rbind(new_hope, empire_strikes, return_jedi, names = c(col, row))

# option C
col <- c("US", "non-US")
row <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
star_wars_matrix <- matrix(c(new_hope, empire_strikes, return_jedi), byrow = TRUE, nrow = 3, dimnames = list(col, row))

# option D
col <- c("US", "non-US")
row <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
star_wars_matrix <- matrix(c(new_hope, empire_strikes, return_jedi),
                           byrow = TRUE, nrow = 3, dimnames = list(row, col))
summary(star_wars_matrix)