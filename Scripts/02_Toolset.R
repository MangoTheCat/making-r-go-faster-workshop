

# Profiling Code ----------------------------------------------------------

# Running the Profiler
library(profvis)
profvis({
  nums <- runif(10000)
  hist(nums)
})



# Exercise ----------------------------------------------------------------


# Q1. Use the profiler to profile a single call to the library function, 
#     using a package that is not currently loaded.

# Q2.	How long does the call take to run in total?
#     This is machine-dependent. View the top-level of Data tab from profvis

# Q3. Within the call to library, which function occupies the most time?
# The inner function that takes most time within the library() call is
# loadNamespace

# Q4. Examine the code below that plots a simple linear regression model and 
#     displays the slope of the regression line. Can you guess which part of the
#     code takes longest to run?

simple_lm_plot <- function(formula, data) {
 
  lm1 <- lm(formula, data = data)
  
  vars <- all.vars(formula)[1:2]  # get first 2 formula vars
  plot(lm1$model[rev(vars)], pch = 19, col = "lightblue")
  abline(lm1, col = "blue", lty = 2)
  slope = round(coefficients(lm1)[2], 2)
  title(main = paste(vars, collapse = " by "),
        sub = paste("Slope:", slope))
  invisible(lm1)
  
}

# Q5. Find out for sure by profiling a call to the function.


# Timing Functions Accurately ----------------------------------------------

# Using Microbenchmark
library(microbenchmark)
chars <- sample(letters, 1e4, replace = TRUE)
res <- microbenchmark(
  sort(chars, method = "quick"), 
  sort(chars, method = "radix"), 
  chars[order(chars)])
res
plot(res)


# Exercise ----------------------------------------------------------------

# Q1. Finding the fastest way to filter a data.frame.

# Q2. Plot the results to see which functions are most consistent.

# Q4. Does it make a difference to the dplyr filter results if it is called 
#     using the pipe operator?

# Q5. Have you found the fastest way to filter a data.frame, or are there any 
#     other methods you might use, or other packages?
