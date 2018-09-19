# Where to Optimise -------------------------------------------------------

# Exercise ----------------------------------------------------------------

# Q1. Take a look at the following example code. Assuming we want to make it go
#     faster, what more information would you need in order to decide where to 
#     improve?

source("./utils.R")
data <- read.csv("records.csv")
vars <- read.csv("variablesForProcessing.csv")[[1]]
data <- preProcess(data)
result <- numeric()

for (i in 1:nrow(data)) {
  
  tmp <- applyNormalisation(data[i, ])
  
  for (var in vars) {
    result <- c(result, doCalculation(data = tmp, var = var))
  }
}

saveRDS(result, file = "result.rds")  

# We would need to know how fast each expression was when called with 
# with a typical dataset; and how much repetition the for() loops undergo.


# Q2.	Without knowing how the functions are written, can you still recommend a 
#     good place to start optimising?

# There is a for loop within another for loop, which will be run most 
# frequently, so doCalculation() may be a good starting point.
