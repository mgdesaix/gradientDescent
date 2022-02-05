# gradientDescent


I created this Shiny app because I found gradient descent to be confusing, I wasn't sure exactly how the weights were being updated. 

In the app, I code a simple implementation of stochastic gradient descent (SGD) for a linear regression model. The parameters specifying the observed data can be manipulated, and so can the learning rate and number of epochs for SGD.
After running the model, plots output the root mean square error, and the weights for the coefficients of the model (intercept and slope). It also plots the predicted versus observed y values for a given epoch, so the crappy performance at earlier epochs can be observed.
