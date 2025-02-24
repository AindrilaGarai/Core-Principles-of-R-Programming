# Introduction:
# vectors are one data type in R - atomic vectors and list
# NULL is closely realted to vector

# the other data type is node - functions, env 
# Ncells stands for nodes, Vcells stands for vectors

# every vector can have attributes- dimension (turns vectors into matrices and arrays), class (powers the S3 obj system)

# Quiz 

# 1.
# 4 types of atomic vectors: logical, integer, double, character(contains strings)
# integer + double = numeric vectors
# two rare types - complex (not needed in stat) and raw (to handle binary data)

# 2.
# Attributes allow you to associate arbitrary additional metadata to any object. 
# set individual attributes with attr(x, "y") and attr(x, "y") <- value; 
# or set all attributes at once with attributes().

# 3. 
# The elements of a list can be any type (even a list); the elements of an atomic vector are all of the same type. 
# Similarly, every element of a matrix must be the same type; in a data frame, different columns can have different types.

# 4.
# yes, we can make a list-array by assigning dimensions to a list. 
# You can make a matrix a column of a data frame with df$x <- matrix(), 
# or by using I() when creating a new data frame data.frame(x = I(matrix())).

# 5. 
# Tibbles have an enhanced print method, never coerce strings to factors, 
# and provide stricter subsetting methods.

# scalars

?Quotes # quoting in R

lgl_var <- c(TRUE, FALSE)
int_var <- c(1L, 6L, 10L)
dbl_var <- c(1, 2.5, 4.5)
chr_var <- c("these are", "some strings")

(c(c(1, 2), c(3, 4)))

typeof(lgl_var); length(lgl_var)
typeof(int_var)
typeof(dbl_var)
typeof(chr_var)

# missing values

NA > 5
10 * NA
!NA

# some exceptions: occur when some identity holds for all possible inputs
NA ^ 0
NA | TRUE
NA & FALSE

# propagation of missingness leads to a common mistake
(x <- c(NA, 5, NA, 10)) 
x == NA # this is true as NA means missing value, maybe one missing value is not same to the other missing value 

is.na(x) # whether there is a missing value

# Testing and coercion
# When you attempt to combine different types they will be coerced in a fixed order: 
# character → double → integer → logical

str(c("a", 1)) # combining a character and an integer yields a character

x <- c(FALSE, FALSE, TRUE)
as.numeric(x) # coercion
sum(x)
mean(x)

as.integer(c("1", "1.5", "a")) # failed coercion of strings 

# exercise
# 1.
?raw
?complex

as.raw(42)
charToRaw("A")

complex(length.out = 1, real = 1, imaginary = 1)
complex(length.out = 2, real = c(2,8), imaginary = 1)

# 2.
c(1, FALSE)
c("a", 1)
c(TRUE, 1L)

# 3. because of coercion 
1=="1"
-1 < F
"one" < 2

# 4.
# If NA were a character and added to a set of other values all of these would be coerced to character as well.

# 5.
# is.atomic() tests if an object is an atomic vector (as defined in Advanced R) or is NULL (!).
# is.numeric() tests if an object has type integer or double and is not of class factor, Date, POSIXt or difftime.
# is.vector() tests if an object is a vector (as defined in Advanced R) or an expression and has no attributes, apart from names.

# attributes

a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")

attr(a, "y") <- 4:6
str(attributes(a))

# Or equivalently
a <- structure(1:2, x = "abcdef", y = 7:12, z = 1:4)
str(attributes(a))

# Attributes should generally be thought of as ephemeral,
# most attributes are lost by most operations
attributes(a[1])
attributes(sum(a))

# There are only two attributes that are routinely preserved:
# names, a character vector giving each element a name.
# dim, short for dimensions, an integer vector, used to turn vectors into matrices or arrays.

# names
x <- c(a = 1, b = 2, c = 3)
x <- 1:3
names(x) <- c("a", "b", "c")
x <- setNames(1:3, c("a", "b", "c"))

x <- unname(x) 
names(x) <- NULL # removing names

# dimensions
(x <- matrix(1:6, nrow = 2, ncol = 3))
(y <- array(1:12, c(1, 3, 4)))
z <- 1:6
dim(z) <- c(3, 2); z

# str() to reveal differences
str(1:3)                   
str(matrix(1:6, ncol = 2)) 
str(matrix(1:6, nrow = 2)) 
str(array(1:6, 4))         

# exercises

# 1.
setNames <- function(object = nm, nm) {
  names(object) <- nm
  object}

setNames( , c("a", "b", "c"))

unname <- function(obj, force = FALSE) {
  if (!is.null(names(obj))) 
    names(obj) <- NULL
  if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj))) 
    dimnames(obj) <- NULL
  obj
} # source code

# 2.
x <- 1:10
dim(x)
nrow(x); NROW(x)
ncol(x); NCOL(x)
length(x)

# 3.
x1 <- array(1:5, c(1, 1, 5))  # 1 row,  1 column,  5 in third dim.
x2 <- array(1:5, c(1, 5, 1))  # 1 row,  5 columns, 1 in third dim.
x3 <- array(1:5, c(5, 1, 1))  # 5 rows, 1 column,  1 in third dim.

# 4.
structure(1:5, comment = "my attribute")

foo <- structure(1:5, comment = "my attribute")

attributes(foo)
attr(foo, which = "comment")

# S3 atomic vectors
# Categorical data, where values come from a fixed set of levels recorded in factor vectors.
# Dates (with day resolution), which are recorded in Date vectors.
# Date-times (with second or sub-second resolution), which are stored in POSIXct vectors.
# Durations, which are stored in difftime vectors.

# A factor is a vector that can contain only predefined values. It is used to store categorical data. 
# Factors are built on top of an integer vector with two attributes: a class, “factor”, which makes it behave differently from regular integer vectors, 
# and levels, which defines the set of allowed values.
x <- factor(c("a", "b", "b", "a")) # store categorical data
x
typeof(x)
attributes(x)

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

typeof(table(sex_char))
attributes(table(sex_char))
table(sex_factor)
attributes(table(sex_factor))

# order factor's labels are meaningful
grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade

# dates: built on top of double vectors
today <- Sys.Date()

typeof(today)
attributes(today)

date <- as.Date("1970-01-01")
typeof(date)
unclass(date)

# date-time

# POSIXlt: portable operating system interface: family of cross-platform standards
now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct
typeof(now_ct)
attributes(now_ct) # tzone controls how the date-time is formatted

structure(now_ct, tzone = "Asia/Tokyo")
structure(now_ct, tzone = "America/New_York")
structure(now_ct, tzone = "Australia/Lord_Howe")
structure(now_ct, tzone = "Europe/Paris")

# durations

one_week_1 <- as.difftime(1, units = "weeks")
one_week_1
typeof(one_week_1)
attributes(one_week_1)

one_week_2 <- as.difftime(7, units = "days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)

# exercise

# 1.
x <- table(mtcars[c("vs", "cyl", "am")])
typeof(x)
attributes(x)
x[ , , 1]
x[ , , 2]

# 2.
f1 <- factor(letters)
levels(f1) <- rev(levels(f1))
as.integer(f1)
# The underlying integer values stay the same, 
# but the levels are changed, making it look like the data has changed.

# 3.
f2 <- rev(factor(letters))
as.integer(f2)
f3 <- factor(letters, levels = rev(letters))
as.integer(f3)

# lists

l1 <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))

typeof(l1)
str(l1)
attributes(l1)

library(lobstr)
obj_size(mtcars)

l2 <- list(mtcars, mtcars, mtcars, mtcars)
obj_size(l2)

# lists are sometimes called recursive vectors
l3 <- list(list(list(1)))
str(l3)
l3

l4 <- list(list(1, 2), c(3, 4))
l5 <- c(list(1, 2), c(3, 4))
str(l4)
str(l5)

# testing and coercion
list(1:3)
as.list(1:3)

# metrics and arrays
l <- list(2:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l
l[,2]

l[[1, 1]]
l[1,1]

# exercise

# 1.
# Atomic vectors are always homogeneous. Lists may be heterogeneous.
# Atomic vectors point to one address in memory, while lists contain a separate reference for each element. 
lobstr::ref(1:2)
lobstr::ref(list(1:2, 2))

# Subsetting with out-of-bounds and NA values leads to different output. 
(1:2)[3]                                                                                     
as.list(1:2)[3]                                     
as.list(1:2)[NA]                                                                                      
  
# 2.
# A list is already a vector, though not an atomic one!
# Note that as.vector() and is.vector() use different definitions of “vector!”
is.vector(as.vector(mtcars))
                                                                                   
# 3.
# While dates store the number of days since the reference date 1970-01-01 (also known as “the Epoch”) in days, 
# date-time-objects (POSIXct) store the time difference to this date in seconds.
date    <- as.Date("1970-01-02")
dttm_ct <- as.POSIXct("1970-01-01 01:00", tz = "UTC")
unclass(date)
unclass(dttm_ct)

c(date, dttm_ct)  # dispatches on its first argument,
c(dttm_ct, date)  

unclass(c(date, dttm_ct))
date + 3599

c(dttm_ct, date)
unclass(c(dttm_ct, date))
c(date, dttm_ct)
unclass(c(date, dttm_ct))

(dttm_ct <- as.POSIXct("1970-01-01 01:00", tz = "HST"))
attributes(c(dttm_ct))
unlist(list(date, dttm_ct))  

# data frames and tibbles
# s3 vectors built on top of lists - data.frame and tibbles

df1 <- data.frame(x = 1:3, y = letters[1:3]); df1
typeof(df1)
attributes(df1)

library(tibble)
# the only diff bet data.frame and tibble is that class is longer and includes tbl_df
df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)
df2
attributes(df2)

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
str(df2)

df1 <- data.frame(x = 1:3,y = c("a", "b", "c"),stringsAsFactors = FALSE)
str(df1)

# tibble never coerce their input
df2 <- tibble(x=1:3, y=c("a","b","c"))
str(df2)

# tibble doesn't transform non-syntactic names
names(data.frame(`1` = 1))
names(tibble(`1` = 1))

# while data frames automatically recycle columns that are an integer multiple of the longest column, 
# tibbles will only recycle vectors of length one.
data.frame(x = 1:4, y = 1:2)
data.frame(x = 1:4, y = 1:3)
tibble(x = 1:4, y = 1)
tibble(x = 1:4, y = 1:2)

# tibble allows to refer variable during construction, left -> right
tibble(x = 1:3, y = x * 2)

# row names
(df3 <- data.frame(age = c(35, 27, 18),hair = c("blond", "brown", "black"),row.names = c("Bob", "Susan", "Sam")))
rownames(df3); df3["Bob", ]
# transpose in data frame is invalid

# rownames should be unique but for bootstrapping the case is different so, we do
df3[c(1,1,1),]
as_tibble(df3, rownames = "name") # tibble don't support row names

# printing
library(dplyr)
starwars

# sub-setting- subsetting tibble always provide tibble whereas subsetting data frames returns a vector 
df1 <- data.frame(xyz = "a")
df2 <- tibble(xyz = "a")
str(df1$x) # error occurs- so only use df1[,""] or df1[["x"]]
str(df2$x)

# testing and coercion- tibbles are data frame but data frame is not tibbles
is.data.frame(df1);is.data.frame(df2)
is_tibble(df1); is_tibble(df2)
# you can coerce using as.tibble() or as.data.frame()

df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4); df # column is a list

data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
tibble(x = 1:3, y = list(1:2, 1:3, 1:4)) # better to understand

dfm <- data.frame(x = 1:3 * 10)
dfm$y <- matrix(1:9, nrow = 3)
dfm$z <- data.frame(a = 3:1, b = letters[1:3], stringsAsFactors = FALSE)
str(dfm)
dfm[1,] # maybe it's confusing

# exercise
# 1.
data.frame(a = integer(), b = logical())
data.frame(row.names = 1:3)  # or data.frame()[1:3, ]
data.frame()

mtcars[0, ] # 0 rows
mtcars[ , 0]  # 0 columns
mtcars[0, 0] # 0 rows, 0 columns

# 2.
data.frame(row.names = c("x", "y", "y")) # error occurs
df <- data.frame(x = 1:3)
row.names(df) <- c("x", "y", "y")
df[c(1, 1, 1), , drop = FALSE]

# 3.
df <- data.frame(x = 1:3, y = letters[1:3])
is.matrix(df)
is.matrix(t(df))
is.matrix(t(t(df))) # using t(), it gets coerced

dim(df)
#> [1] 3 2
dim(t(df))
#> [1] 2 3
dim(t(t(df)))
#> [1] 3 2

df;t(df) # as coerced to matrix -> matrix's all elements are of same type -> all character 

# 4.
df_coltypes <- data.frame(a = c("a", "b"),b = c(TRUE, FALSE),c = c(1L, 0L),d = c(1.5, 2),e = factor(c("f1", "f2")))
as.matrix(df_coltypes)
data.matrix(df_coltypes) # all integer type, no character

# NULL
typeof(NULL) # NULL is always of length 0 and can't have any attributes
length(NULL)
x <- NULL
attr(x, "y") <- 1
is.null(x)
c()

# Two common uses of NULL-
# 1. to represent an empty vector
# 2. to represent an absent vector- often used as default function argument 

