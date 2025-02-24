# A functional is a function that takes a function as an input and returns a vector as output.

randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(mean)
randomise(sum)

# A common use of functionals is as an alternative to for loops. 

library(purrr)

triple <- function(x) x * 3
map(1:3, triple) # map refers to “an operation that associates each element of a given set with one or more elements of a second set”.

map_chr(mtcars, typeof) # always returns a character vector
map_lgl(mtcars, is.double) # always returns a logical vector

n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique) # always returns a integer vector

map_dbl(mtcars, mean) # always returns a double vector

pair <- function(x) c(x, x)
map_dbl(1:2, pair)

map(1:2, pair)
map(1:2, as.character)

map_dbl(mtcars, function(x) length(unique(x)))
map_dbl(mtcars, ~ length(unique(.x))) # a special shortcut
as_mapper(~ length(unique(.x)))

x <- map(1:3, ~ runif(2))
str(x)

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")

# Or by position
map_dbl(x, 1)

# Or by both
map_dbl(x, list("y", 1))
map(x,"y")

# You'll get an error if a component doesn't exist:
map_chr(x, "z")

# Unless you supply a .default value
map_chr(x, "z", .default = NA)

x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))
map_dbl(x, mean, na.rm = TRUE)

plus <- function(x, y) x + y
x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
map_dbl(x, ~ plus(.x, runif(1)))

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
map_dbl(trims, ~ mean(x, trim = .x))
map_dbl(trims, function(trim) mean(x, trim = trim))
map_dbl(trims, mean, x = x)

# exercise:

# 1.
as_mapper(c(1, 2))  
as_mapper(c("a", "b"))  
as_mapper(list(1, "b"))  

# Define custom accessor function
get_class <- function(x) attr(x, "class")
pluck(mtcars, get_class)

# Use attr_getter() as a helper
pluck(mtcars, attr_getter("class"))

# 2.
as_mapper(~ runif(2))
as_mapper(runif(2))

# 3.
map_dbl(mtcars, sd)

penguins <- palmerpenguins::penguins
penguins_numeric <- map_lgl(penguins, is.numeric)
map_dbl(penguins[penguins_numeric], sd, na.rm = TRUE)

penguins_factor <- map_lgl(penguins, is.factor)
map_int(penguins[penguins_factor], ~ length(levels(.x)))

# 4.
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(10, 7)))

library(ggplot2)

df_trials <- tibble::tibble(p_value = map_dbl(trials, "p.value"))

df_trials %>%
  ggplot(aes(x = p_value, fill = p_value < 0.05)) +
  geom_dotplot(binwidth = .01) +  # geom_histogram() as alternative
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top"
  )

# 5.

x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)

# Don't name the argument
map(x, map, triple)

# Use magrittr-style anonymous function
map(x, . %>% map(triple))

# Use purrr-style anonymous function
map(x, ~ map(.x, triple))

# 7.
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
models <- map(formulas, lm, data = mtcars)

# 7.
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

bootstraps %>%
  map(~ lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# purr style
by_cyl <- split(mtcars, mtcars$cyl)
by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef) %>% #  we want to fit a linear model, then extract the second coefficient 
  map_dbl(2)

by_cyl %>% 
  lapply(function(data) lm(mpg ~ wt, data = data)) %>% 
  lapply(coef) %>% 
  vapply(function(x) x[[2]], double(1))

models <- lapply(by_cyl, function(data) lm(mpg ~ wt, data = data))
vapply(models, function(x) coef(x)[[2]], double(1))

slopes <- double(length(by_cyl))
for (i in seq_along(by_cyl)) {
  model <- lm(mpg ~ wt, data = by_cyl[[i]])
  slopes[[i]] <- coef(model)[[2]]
}
slopes

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)
modify(df, ~ .x * 2)
df <- modify(df, ~ .x * 2)

simple_modify <- function(x, f, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- f(x[[i]], ...)
  }
  x
}

xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

map_dbl(xs, mean)
map_dbl(xs, weighted.mean, w = ws)
map2_dbl(xs, ws, weighted.mean)
map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)

simple_map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")
map(names, welcome)
walk(names, welcome)

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)

imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))

x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The highest value of ", .y, " is ", max(.x)))

pmap_dbl(list(xs, ws), weighted.mean)
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)

params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif)

l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)

out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
out

reduce(l, intersect)

reduce(l, union)

simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

accumulate(l, intersect)

x <- c(4, 3, 10)
reduce(x, `+`)

accumulate(x, `+`)
reduce(1, `+`)
reduce("a", `+`)
reduce(integer(), `+`)
reduce(integer(), `+`, .init = 0)
reduce("a", `+`, .init = 0)

sum(integer())  # x + 0 = x

prod(integer()) # x * 1 = x

min(integer())  # min(x, Inf) = x

max(integer())  # max(x, -Inf) = x

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)

detect_index(df, is.factor)


str(keep(df, is.factor))
str(discard(df, is.factor))

df <- data.frame(
  num1 = c(0, 10, 20),
  num2 = c(5, 6, 7),
  chr1 = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

str(map_if(df, is.numeric, mean))

str(modify_if(df, is.numeric, mean))

str(map(keep(df, is.numeric), mean))

a2d <- matrix(1:20, nrow = 5)
apply(a2d, 1, mean)

apply(a2d, 2, mean)


a3d <- array(1:24, c(2, 3, 4))
apply(a3d, 1, mean)

apply(a3d, c(1, 2), mean)

a1 <- apply(a2d, 1, identity)
identical(a2d, a1)

a2 <- apply(a2d, 2, identity)
identical(a2d, a2)

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
apply(df, 2, mean)


integrate(sin, 0, pi)

str(uniroot(sin, pi * c(1 / 2, 3 / 2)))

str(optimise(sin, c(0, 2 * pi)))

str(optimise(sin, c(0, pi), maximum = TRUE))


