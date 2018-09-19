
# Avoid Growing Objects ---------------------------------------------------
x <- 1:10
append(x, 11)
x

# Growing a Result Set
make_sequence_grow <- function(n) {
  vec <- numeric()
  for (i in 1:n) vec <- c(vec, i)
  vec
}
make_sequence_grow(10)

# Pre-allocating a Result Set
make_sequence_preallocate <- function(n) {
  vec <- numeric(n)
  for (i in 1:n) vec[i] <- i
  vec
}

# Comparing Performance
library(microbenchmark)
n <- 1000
res <- microbenchmark(
  make_sequence_grow(n), 
  make_sequence_preallocate(n)
)
print(res, signif = 2)


# Making Use of Vectorisation ---------------------------------------------
x <- 1:1000
y <- 1000:1
x + y

# Element-wise approach
add <- function(x, y) {
  result <- numeric(length(x))
  for (i in seq_along(x)) result[i] <- x[i] + y[i]
  result
}
res <- microbenchmark(x + y, add(x, y))
print(res, signif = 2)

# if vs. if_else
if_elementwise <- function(x) {
  result <- logical(length(x))
  for (i in seq_along(x)) result[i] <- if (x[i] > 0.5) TRUE else FALSE
  result
}
if_vectorised <- function(x) {
  ifelse(x > 0.5, TRUE, FALSE)
}
x <- rnorm(1000)
res <- microbenchmark(if_elementwise(x), if_vectorised(x))
print(res, signif = 2)


# Don't Reinvent a Slower Wheel -------------------------------------------
n <- 1000
res <- microbenchmark(make_sequence_preallocate(n),
                      seq(n),
                      seq.int(n))
print(res, signif = 2)


# colMeans vs apply
res <- microbenchmark(
  apply(mtcars, 2, mean),
  colMeans(mtcars)
)
print(res, signif = 2)


# Exercise ----------------------------------------------------------------
# Q1. Write a function that takes a vector of input and using a loop iterates 
#     over all of the values calculating the sum up to that value (i.e. the 
#     cumulative sum). For example, myfun(1:10) returns:
#      1  3  6  10  15  21  28  36  45  55

# Q2. Using the tools of this chapter, determine how long it takes to run your function.

# Q3. Use the initialization and vectorization techniques to improve the speed 
# of your function. Check that this is in fact more efficient. 

# Q4. Can you find a function in R that will do this for you?
#     Compare the speed of that function to your most efficient version.

# Q5. The function below is designed to take a vector of starting values and 
#     project each value forward by a number of periods, multiplying the 
#     starting value by a growth rate each period. Profile the code.

#' Project Investments
#' @param startingAmounts numeric vector of starting balances
#' @param growthRate single number for growth rate, e.g. 1.1 = 10% growth
#' @param nPeriods single number for periods to project forwards
#' @return matrix with one row per startingAmount, one column per period
#' @examples projectInvestments(c(150, 200, 101), growth = 1.1, nPeriods = 10)
projectInvestments <- function(startingAmounts, growthRate, nPeriods) {
  result <- numeric()
  # For each element in startingAmounts, use it as the base value and create
  # the next period's value by multiplying by the growthRate
  for (startingAmount in startingAmounts) {
    projectedAmounts <- startingAmount
    for (i in seq_len(nPeriods)) {
      projectedAmounts[i + 1] <- projectedAmounts[i] * growthRate
    }
    result <- rbind(result, projectedAmounts)
  }
  unname(result)
}

# Q6. How many times faster can you make it using the techniques in this chapter?


# For Loops ---------------------------------------------------------------

# Explicit instructions on how to apply a function to each element
result <- numeric(length(x))
for (i in seq_along(x)) {
  result[i] <- aFunction(x[i])
}
result

# Implicit application of a function to each element
lapply(x, aFunction)
  

# Choosing the Right Data Structures --------------------------------------

# data.frames enforce their rules
mtcars$newCol <- 1:100

# matrices are faster than data.frames

# Create data.frame
df1 <- data.frame(A = seq_len(1e3), B = 0)
# Create matrix version
mt1 <- data.matrix(df1)

updateColumn <- function(x) {
  for (i in seq_len(nrow(x))) { 
    x[i, "B"] <- i 
  }
  x
}
res <- microbenchmark(
  updateColumn(df1),
  updateColumn(mt1),
  rowSums(df1), 
  rowSums(mt1))
print(res, signif = 2)


x <- rnorm(1e6)
topn1 <- function(x, n = 10) sort(x)[seq_len(n)]
topn2 <- function(x, n = 10) sort(x, partial = seq_len(n))[seq_len(n)]
topn3 <- function(x, n = 10) sort(x, method = "radix")[seq_len(n)]
topn1(x)
topn2(x)
topn3(x)

microbenchmark(topn1(x), topn2(x), topn3)


# Using Existing Implementations Effectively -------------------------------

# Reading tables
library(microbenchmark)

# Using read.csv with colClasses
dat <- as.data.frame(matrix(rnorm(1e6), ncol = 20))
dat[,1] = "a"
headings <- LETTERS[1:20]
write.table(dat, 
            file = "dat.csv", 
            sep = ",", 
            col.names = headings, 
            row.names = FALSE)

res <- microbenchmark(times = 10, list = alist(
  withoutCols = read.csv("dat.csv", header = TRUE),
  withCols = read.csv("dat.csv", header = TRUE, 
                      colClasses = c("character", rep("numeric", 19)))
))
print(res, signif = 2)

# Comparing file reading alternatives
res <- microbenchmark(times = 10, list = alist(
  withoutCols = read.csv("dat.csv", header = TRUE),
  withCols = read.csv("dat.csv", header = TRUE, 
                      colClasses = c("character", rep("numeric", 19)))
))
print(res, signif = 2)

res <- microbenchmark(times = 10,
                      read.csv("dat.csv"),
                      readr::read_csv("dat.csv"),
                      data.table::fread("dat.csv")
)
print(res, signif = 2)


# Exercise ----------------------------------------------------------------

# Q1.Create a matrix with one million elements and 20 columns.
#    Compare the speed of saving the file in .Rds format between 
#    base::saveRDS and readr::write_rds. Which is faster and why?

# Q2.	The createRefs function below generates random ten-letter reference codes, 
#     returning them as a sorted character vector.
#      a. 	Benchmark the existing function, recording its typical speed when n = 1000?
#      b. 	What is the slowest part of the function?
#      c. 	Using the techniques in this chapter, how many times faster can you make the function?
  
# Original function:
#' Create References
#' Creates sorted random 10-letter reference codes
#' @param nRef Number of references to generate
#' @return sorted (ascending) character vector of 10-letter codes
createRefs <- function(nRef = 1000) {
  result <- data.frame()
  for (i in seq_len(nRef)) {
    
    # Generate a ten-letter reference code
    ref <- character()
    for (j in seq_len(10)) {
      ref <- paste0(ref, LETTERS[floor(runif(1, min = 0, max = 27))])
    }
    result <- rbind(result, data.frame(ref = ref, 
                                       stringsAsFactors = FALSE))
  }
  
  result <- result[order(result$ref), ]
  return(result)
}
