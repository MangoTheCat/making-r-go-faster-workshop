# The following functions will:
#  - Manipulate the data to find the 5% and 95% intervals of the given variable at
#  each time point
#  - Find the median value at wach time point
#  - Plot the data with a polygon of showing the 5 to 95% interval
#  - Add a line showing the median values

manipulateData <- function(data, col1, col2){

  dataLower <- aggregate(list(Var = data[ , col1]), list(ID = data[ ,col2]), 
                        quantile, probs = c(0.05))

  dataUpper <- aggregate(list(Var = data[ , col1]), list(ID = data[ ,Col2]), 
                         quantile, probs = c(0.95))
  
  dataInt <- rbind(dataLower, dataUpper[nrow(dataUpper):1, ])
  
  dataInt

}


medData <- function(data, col1, col2){

  aggregate(list(Var = data[ , col1]), list(ID = data[ ,col2]), 
            quantile, probs = c(0.5))

}


plotData <- function(x, y, data){

  polyPlot <- ggplot()
  
  summaryData <- manipulatedata(data, y, x)
  
  polyPlot <- polyPlot + geom_polygon(data = summaryData, aes(x = ID, y = var),
                                      fill = "orange", alpha = 0.3, col = "orange")
  
  medianData <- medData(data, x, y)
  
  polyPlot <- polyPlot + geom_line(data = summaryData, aes(x = ID, y = Var), 
                                   lwd = 1.5, col = "orange")
  
  polyPlot + theme_classic() + xlab(x) + ylab(y)

}

# test the above functions using airquality
plotData("Month", "Temp", airquality)


