

# Profiling Code ----------------------------------------------------------

# Exercise ----------------------------------------------------------------

# Q1. Use the profiler to profile a single call to the library function, 
#     using a package that is not currently loaded.

library(profvis)
profvis({library(ggplot2)})

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
# We can guess that plot() is slowest as there is lots of plot set-up and
# drawing.

# Q5. We can confirm this slowest part using the profiler:
profvis(simple_lm_plot(mpg ~ wt, data = mtcars))


# Timing Functions Accurately ----------------------------------------------

# Exercise ----------------------------------------------------------------

# Q1. Finding the fastest way to filter a data.frame.
library(dplyr)
res <- microbenchmark(
  quakes[quakes$mag > 6, ],
  subset(quakes, mag > 6),
  filter(quakes, mag > 6),
  quakes %>% filter(mag > 6),
  
  times = 50)
res
# Q2. Plot the results to see which functions are most consistent.
plot(res)


# Q4. Does it make a difference to the dplyr filter results if it is called 
#     using the pipe operator?
# There is a small overhead to using the %>% operator

# Q5. Have you found the fastest way to filter a data.frame, or are there any 
#     other methods you might use, or other packages?
# Other filtering implementations include
# quakes[which(quakes$mag > 6), ]
# data.table package: data.table(quakes)[mag > 6]
