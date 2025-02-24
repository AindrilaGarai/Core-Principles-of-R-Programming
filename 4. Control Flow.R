# control flow
# two primary tools of control flow: choices and loops

# quiz:

# 1.
# if works with scalars; ifelse() works with vectors.

# 2.
x = T
(y <- if (x) 3)
x = F
(y <- if (x) 3)
x = NA
(y <- if (x) 3)

# 3.
switch("x", x = , y = 2, z = 3)

# choices 
grade <- function(x) {
  if (x > 90) {
    "A"
  } else if (x > 80) {
    "B"
  } else if (x > 50) {
    "C"
  } else {
    "F"
  }
}
grade(70)

x1 <- if (TRUE) 1 else 2
x2 <- if (FALSE) 1 else 2

c(x1, x2)

greet <- function(name, birthday = FALSE) {
  paste0(
    "Hi ", name,
    if (birthday) " and HAPPY BIRTHDAY"
  )
}
greet("Maria", FALSE)

greet("Jaime", TRUE)

# invalid inputs 

if("x") 1
if(logical()) 1
if(NA) 1
if (c(TRUE, FALSE)) 1

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
if (c(TRUE, FALSE)) 1

x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))
ifelse(x %% 2 == 0, "even", "odd")
       
x <- c(1:10, 70, NA, TRUE)
dplyr::case_when(          # allow any number of condition-vector pairs
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "???",
  TRUE ~ as.character(x)
)

x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2" 
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}
x_option("d")
x_option("a")

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value")
  )
}

(switch("c", a = 1, b = 2)) # the last component of a switch() should always throw an error, 
# ow unmatched inputs will invisibly return NULL.

legs <- function(x) {
  switch(x,
         cow = ,
         horse = ,
         dog = 4,
         human = ,
         chicken = 2,
         plant = 0,
         stop("Unknown input")
  )
}
legs("cow")

# exercise

# 1.
ifelse(TRUE, 1, "no")
ifelse(FALSE, 1, "no")
ifelse(NA, 1, "no")

# 2.
x <- 1:10
if (length(x)) "not empty" else "empty"

x <- numeric()
if (length(x)) "not empty" else "empty"
x <- numeric(length = 1)
if (length(x)) "not empty" else "empty"

# loops 

for (i in 1:10) {
  if (i < 3) 
    next # exits the current loop
  
  print(i)
  
  if (i >= 5)
    break # exit the entire loop
}

# common pitfalls

means <- c(1, 50, 20)
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}
out

means <- c()
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}

means <- c(1, 50, 20)
seq_along(means) # always return a value of the same length x
out <- vector("list", length(means))
for (i in seq_along(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}
out

xs <- as.Date(c("2020-01-01", "2010-01-01")) # S3 vectors
for (x in xs) {
  print(x)
}

for (i in seq_along(xs)) {
  print(xs[[i]])
}

x <- numeric()
out <- vector("list", length(x))
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2
}
out

# related tools
# while(condition) action: performs action while condition is TRUE.
# repeat(action): repeats action forever (i.e. until it encounters break).
# repeat > while > for (according to flexibility)

xs <- c(1, 2, 3)
for (x in xs) {
  xs <- c(xs, x * 2)
}
xs

for (i in 1:3) {
  i <- i * 2
  print(i) 
}
