# Quiz:

# 1.
# Positive integers select elements at specific positions, 
# negative integers drop elements; 
# logical vectors keep elements at positions corresponding to TRUE; 
# character vectors select elements with matching names.

# 2.
# [ selects sub-lists: it always returns a list. If you use it with a single positive integer, it returns a list of length one. 
# [[ selects an element within a list. 
# $ is a convenient shorthand: x$y is equivalent to x[["y"]].
 
# 3.
# Use drop = FALSE if you are subsetting a matrix, array, or data frame and you want to preserve the original dimensions. 
                               
# 4.                                                                                                  
# If x is a matrix, x[] <- 0 will replace every element with 0, keeping the same number of rows and columns. 
# In contrast, x <- 0 completely replaces the matrix with the value 0.
   
# 5.                                                                                                                              
# A named character vector can act as a simple lookup table: c(x = 1, y = 2, z = 3)[c("y", "z", "x")]
 
# selecting multiple elements                                                                                                                                
x <- c(2.1, 4.2, 3.3, 5.4)
x[c(3,1)]
x[order(x)]
x[c(1, 1)]
x[c(2.1, 2.9)] # Real numbers are silently truncated to integers

x[-c(3, 1)]
x[c(-1, 2)]
# only 0's may be mixed with negative subscripts
x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]

x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE, FALSE)] # Equivalent to

x[c(TRUE, TRUE, NA, FALSE)]
x[] # nothing returns the original vector
x[0] # zero returns the 0 length vector

(y <- setNames(x, letters[1:4])) # character vectors
y[c("d", "c", "a")]
y[c("a", "a", "a")]
z <- c(abc = 1, def = 2)
z[c("a", "d")]

y[factor("b")] # Factors are not treated specially when subsetting

# lists
a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ]
a[c(TRUE, FALSE, TRUE), c("B", "A")]
a[0, -2]
a[1,]
a[1,1] 

# arrays in R are stored in column-major order

vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals
vals[c(4, 15)] # not matrix type, column wise elements selection

# You can also subset higher-dimensional data structures with an integer matrix (or, if named, a character matrix). 
# Each row in the matrix specifies the location of one value, and each column corresponds to a dimension in the array. 
# This means that you can use a 2 column matrix to subset a matrix, a 3 column matrix to subset a 3D array, and so on. 
  
select <- matrix(ncol = 2, byrow = TRUE, c(
    1, 1,
    3, 1,
    2, 4
  ))
vals[select]

# data.frame and tibbles

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")]
df[, c("x", "z")]
str(df["x"]) # tibble return
str(df[, "x"])

df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])

str(df["x"])
str(df[, "x"])

# preserving dimensionality
a <- matrix(1:4, nrow = 2)
str(a[1, ])
str(a[1, , drop = FALSE]) # to preserve the original dimension
str(a[,,drop=F])

df <- data.frame(a = 1:2, b = 1:2)
str(df[, "a"])
str(df[, "a", drop = FALSE]) # to preserve the original dimension

z <- factor(c("a", "b"))
z[1]
z[1, drop = TRUE]

# exercise

# 1.
mtcars[mtcars$cyl == 4, ]
mtcars[-c(1:4), ]
mtcars[mtcars$cyl <= 5,]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]

# 2.
x <- 1:5
x[NA] # n contrast to NA_real, NA has logical type and logical vectors are recycled
# to the same length as the vector being subset, i.e. x[NA] is recycled to x[NA, NA, NA, NA, NA].

# 3.
x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)] # returns a logical matrix, which contains TRUE values above the diagonal and FALSE values everywhere else.

# 4.
# as mtcars has only 11 columns, the index will be out of bounds and an error is thrown.

# 5.
diago <- function(a)
{
  rtn <- NULL
  if(ncol(a)==nrow(a))
  {for (i in 1:ncol(a)) {
    rtn <- c(rtn,a[i,i])
  }}
  else{print("out of bound")}
  return(rtn)
}
diago(x)

diag2 <- function(x) {
  n <- min(nrow(x), ncol(x))
  idx <- cbind(seq_len(n), seq_len(n))
  
  x[idx]
}

# 6.
df[is.na(df)]<-0 # replaces the NAs in df with 0

# selecting a single element
x <- list(1:3, "a", 4:6)

for (i in 2:length(x)) {
  out[i] <- fun(x[i], out[i - 1])
}

for (i in 2:length(x)) {
  out[[i]] <- fun(x[[i]], out[[i - 1]]) # better, can return only single value
}

var <- "cyl" 
mtcars$var # Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars[[var]]

x <- list(abc = 1)
x$a
#> [1] 1
x[["a"]]

# To help avoid this behaviour, setting the global option warnPartialMatchDollar to TRUE:
options(warnPartialMatchDollar = TRUE)
x$a

# missing and out-of-bounds
x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)

purrr::pluck(x, "a", 1) # allows to mix integer and character indices
purrr::pluck(x, "c", 1) # provide an alternate default value if the item does not exist
purrr::pluck(x, "c", 1, .default = NA)

# two additional subsetting operators for S4 object: @, slot()

# exercise
# 1.
mtcars$cyl[[3]]
mtcars[ , "cyl"][[3]]
mtcars[["cyl"]][[3]]
with(mtcars, cyl[[3]])
mtcars[3, ]$cyl
mtcars[3, "cyl"]
mtcars[3, ][ , "cyl"]
mtcars[3, ][["cyl"]]
mtcars[3, 2]
mtcars[[c(2, 3)]]

# 2.
mod <- lm(mpg ~ wt, data = mtcars)
mod$df.residual
mod[["df.residual"]]
summary(mod)$r.squared

# subsetting and assignment
x <- 1:5
x[c(1, 2)] <- c(101, 102)
x

x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

y <- list(a = 1, b = 2)
y["b"] <- list(NULL)
str(y)

mtcars[] <- lapply(mtcars, as.integer)
is.data.frame(mtcars)

mtcars <- lapply(mtcars, as.integer)
is.data.frame(mtcars) # becoming a list

# applications
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])

grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

id <- match(grades, info$grade) # returns the positions of grade from info
id
info[id, ]

df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])
df[sample(nrow(df)), ]

df[sample(nrow(df), 3), ]
df[sample(nrow(df), 6, replace = TRUE), ]

x <- c("b", "c", "a")
order(x)
x[order(x)]

df2 <- df[sample(nrow(df)), 3:1]
df2
df2[order(df2$x), ]
df2[, order(names(df2))]

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]
df[setdiff(names(df), "z")] # columns to keep

mtcars[mtcars$gear == 5 ,]
mtcars[mtcars$gear == 5 & mtcars$gear == 4,]

# boolean algebra vs set
x <- sample(10) < 4
which(x)

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)

(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))

x1 & y1
intersect(x2, y2)

# X | Y <-> union(x, y)
x1 | y1
union(x2, y2)
# X & !Y <-> setdiff(x, y)
x1 & !y1
setdiff(x2, y2)

# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
xor(x1, y1)
setdiff(union(x2, y2), intersect(x2, y2))

# exercise

# 1.
# Permute columns
mtcars[sample(ncol(mtcars))]
# Permute columns and rows in one step
mtcars[sample(nrow(mtcars)), sample(ncol(mtcars))]

# 2.
m <- 10
mtcars[sample(nrow(mtcars), m), ]
start <- sample(nrow(mtcars) - m + 1, 1)
end <- start + m - 1
mtcars[start:end, , drop = FALSE]

# 3.
mtcars[order(names(mtcars))]
mtcars[sort(names(mtcars))]


