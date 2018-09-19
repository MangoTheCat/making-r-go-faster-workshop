
# Avoiding Common Mistakes ------------------------------------------------

# Exercise ----------------------------------------------------------------
# Q1. Write a function that takes a vector of input and using a loop iterates 
#     over all of the values calculating the sum up to that value (i.e. the 
#     cumulative sum). For example, myfun(1:10) returns:
#      1  3  6  10  15  21  28  36  45  55

slowFun <- function(x) {
  for (i in 1:length(x)) {
    if (i == 1) { 
      out <- x[i] 
    } else {
      out <- c(out, sum(out[i - 1], x[i]))
    }
  }
  return(out)
}
slowFun(x = 1:10)


# Q2. Using the tools of this chapter, determine how long it takes to run your function.
microbenchmark(slowFun(x = 1:10))

# Q3. Use the initialization and vectorization techniques to improve the speed 
# of your function. Check that this is in fact more efficient. 
slowFun2 <- function(x){
  result <- numeric(length(x))
  result[1] <- x[1]
  for (i in 2:length(x)) {
    result[i] <- result[i - 1] + x[i]
  }
  result
}

microbenchmark(slowFun(1:10), slowFun2(1:10))

# Q4. Can you find a function in R that will do this for you?
#     Compare the speed of that function to your most efficient version.
fastFun <- function(x) {
  cumsum(x)
}

microbenchmark(slowFun(1:10), slowFun2(1:10), fastFun(1:10))


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

library(profvis)
profvis(projectInvestments(1:100, growthRate = 1.1, nPeriods = 1000))
    

# Q6. How many times faster can you make it using the techniques in this chapter?
projectInvestments2 <- function(startingAmounts, growthRate, nPeriods) {
  cumulativeGrowth <- cumprod(c(1, rep(growthRate, nPeriods)))
  vapply(cumulativeGrowth, `*`, FUN.VALUE = startingAmounts, startingAmounts)
}
microbenchmark(projectInvestments(c(1, 5, 9), growthRate = 1.1, nPeriods = 60),
               projectInvestments2(c(1, 5, 9), growthRate = 1.1, nPeriods = 60))


# Choosing the Right Data Structures --------------------------------------
# Using Existing Implementations Effectively -------------------------------

# Exercise ----------------------------------------------------------------

# Q1.Create a matrix with one million elements and 20 columns.
#    Compare the speed of saving the file in .Rds format between 
#    base::saveRDS and readr::write_rds. Which is faster and why?

dat <- as.data.frame(matrix(rnorm(1e5), ncol = 20))
outFile <- tempfile(fileext = "rds")
microbenchmark(times = 10,
               write_rds(dat, path = outFile),
               saveRDS(dat, file = outFile)
)
# readr::write_rds appears faster than saveRDS because it turns off compression
# by default. However, it is simply a wrapper to saveRDS designed to provide a
# consistent interface. Calling saveRDS directly with compression=FALSE would be
# faster than write_rds.

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

# Q2a. Benchmark and profile the original function
microbenchmark(times = 10, createRefs(1000))
profvis::profvis(createRefs())

# Q2b. Slowest part is rbind on a data.frame. We could use a simpler data
#      structure.

# Q2c. Faster implementations:

# New implementation using existing packages
createRefs2 <- function(nRef = 1000) {
  result <- stringi::stri_rand_strings(n = 5, length = 10, pattern = "[A-Z]")
  sort(result, method = "radix")
}

# New implementation using base R
createRefs3 <- function(nRef = 1000) {
  x <- matrix(sample(LETTERS, size = nRef * 10, replace = TRUE), ncol = 10)
  result <- apply(x, 1, paste, collapse = "")
  sort(result, method = "radix")
}

microbenchmark(times = 10, createRefs(), createRefs2(), createRefs3())
