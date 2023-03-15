# Creating a null model to calculate probability of type I error

# Code adapted from: http://www.sethspielman.org/courses/geog5023/r_examples/Type_I_and_II_Errors.html
# And from: https://stats.stackexchange.com/questions/148526/how-to-simulate-type-i-error-and-type-ii-error

library(dplyr)

# Model set up
n <- 1000 # number of models to simulate  
alphaSet = c(0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05)   # alpha values to test
sigTests <- data.frame(err = NA, pValue = NA, alpha = NA) # create empty data frame
counter <- 1 # keep track of row numbers in the output table

for (i in 1:n){                                                        # repeat 1000 times
  for (alpha in alphaSet){                                             # repeat for each alpha level
    x <- as.data.frame(1965:2021)                                      # years over which to model
    x$Distance <- rnorm(nrow(x), mean=661, sd=406)                     # generate distances using parameters from actual distances
    colnames(x) <- c("Year", "Distance")                               # check column names
    for(j in 1:50){                                                    # start a for loop that removes each row if it is less than the previous row, and repeats 50 times to make sure it finished properly
      x <- x %>% filter((Distance>(lag(Distance)))| row_number()==1)   
    }
    if (nrow(x)>1)                                                    # if there is more than one row in x then create a linear model
      mod <- lm(Distance~Year, x)
    p <- summary(mod)$coefficients[2,4]                               # extract the p value
    p[is.na(p)] <- 0                                                  # if the p value is NA then just assign a value of 0
    if (p < alpha) {                                                  # if p is significant...
      sigTests[counter, 1] <- 1                                       # assign a value of 1 to show error 
      sigTests[counter, 2] <- p                                       # fill in data with p value
      sigTests[counter, 3] <- alpha                                   # fill in data with alpha value
    } else {                                                          # if p is not significant...
      sigTests[counter, 1] <- 0                                       # assign a value of 0 to show no error
      sigTests[counter, 2] <- p                                       # fill in data with p value
      sigTests[counter, 3] <- alpha                                   # fill in data with alpha value
    }
    counter <- counter+1                                              # move to next row
  }
}

# Now summarise the results
# The count of the number of type I errors for each level of alpha
aggregate(sigTests$err ~ sigTests$alpha, FUN=sum) 

# Output:
#  alpha     count_error   %_error  
#  0.00001        72          7%
#  0.00005        89          9%
#  0.0001         88          9%
#  0.0005        102         10%
#  0.001         109         11%
#  0.005         179         18%
#  0.01          268         27%
#  0.05          468         47%
  
# % error is (x/1000)*100