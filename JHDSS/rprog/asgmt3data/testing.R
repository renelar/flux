# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Learning R
#
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
options(digits=4)
options(width=77)

x <- c(rnorm(20))
x
# [1]  0.16666483 -1.04393972  0.69287198  1.07889466  0.88293485  0.80939372
# [7]  1.02741714  0.93771959 -0.48756502 -0.04641779  1.66490570  1.22312571
# [13]  1.44910682 -1.22107125  0.13591442  0.59380681 -1.41811666  1.73485766
# [19]  0.77342105  0.63897757
x <- c(rnorm(20, 3))
x

# Simulate 100 roll of two die. Each roll is designated by a letter of the
# alphabet; there may be duplicate designators.
rolls <- data.frame(ti = sample(LETTERS, 100, replace = TRUE),
                    th = sample(letters, 100, replace = TRUE),
                    d1 = sample(1:6, 100, replace = T),
                    d2 = sample(1:6, 100, replace = T)
                   )

# Calculate the sum of both dice in a roll and add the column named t
# to the data frame.
rolls[,"t"] <- rowSums(rolls[c("d1", "d2")])

# Select a subset based on a roll designator
st <- "e"
rt <- rolls[rolls$th == st,]
# Sort the subset based on die value
rl <- "d1"
index <- order(rt$th, rt[[rl]])
rt <- rt[index, ]
index <- c()
# Pick the first roll designator
rs <- data.frame(rt[which.min(rt[[rl]]), ])
array(rs$ti)

# Sort the rolls according to throw designator
rl <- "d1"
index <- order(rolls$th, rolls[[rl]])
#index <- order(rolls$th, rolls$d1)
rolls <- rolls[index, ]
index <- c()


st <- "e"
rt <- rolls[rolls$th == st,]
rt <- data.frame(rt[which.min(rt$d1), ])

rt <- rolls[rolls$d1 == min(rolls$d1),]
nrow(rt)


