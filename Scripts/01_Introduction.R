
# When to Optimise --------------------------------------------------------

# R's Flexibility:
summary(faithful)  # summarise a data.frame
summary(faithful$eruptions)  # summarise a numeric vector
str(c(TRUE, 2L, 3.0))  # arguments to c() are converted to same data type
class(faithful) <- "not_a_data.frame"
faithful  # now prints as a list not a data.frame

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
