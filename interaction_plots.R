## Two-variable interaction plots in R
## Anton Strezhnev 
## 06/17/2013

## interaction_plot_continuous: Plots the marginal effect for one variable interacted with a continuous moderator variable
## Usage
## Required
# model: linear or generalized linear model object returned by lm() or glm() function
# effect: name of the "effect" variable in the interaction (marginal effect plotted on y-axis) - character string
# moderator: name of the moderating variable in the interaction (plotted on x-axis) - character string
# interaction: name of the interaction variable in the model object - character string
## Optional
# varcov: Variance-Covariance matrix - if default, then taken from the model object using vcov()
# minimum: Smallest value of moderator for which a marginal effect is calculated, if "min" then equal to the minimum value of the moderator in the dataset
# maximum: Largest value of moderator for which a marginal effect is calucated, if "max" then equal to the maximum value of the moderator in the dataset
# num_points: Total number of points for which a marginal effect is calculated - increase to make confidence bounds appear smoother
# conf: Size of confidence interval around coefficient estimates - 0-1, default is .95 (95% confidence)
# mean: Mark the mean mediator value by a vertical red line
# median: Mark the median mediator value by a vertical blue line
# alph: Transparency level of the histogram plot - 0-100, decrease to make the histogram more transparent
# rugplot: Include a rug plot of the mediator values below the figure
# histogram: Include a histogram of mediator values behind the figure - only plotted if minimum="min" and maximum="max"
# title: Title of the plot
# xlabel: Label of the X axis
# ylabel: Label of the Y axis
interaction_plot_continuous <- function(model, effect, moderator, interaction, varcov="default", minimum="min", maximum="max", incr="default", num_points = 50, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, histogram=T, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient"){
  
  # Define a function to make colors transparent
  makeTransparent<-function(someColor, alpha=alph){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
      blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]], na.rm =T)
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]], na.rm = T)
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Make the histogram color
  hist_col = makeTransparent("grey")
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel, ylab=ylabel, main=title)
  
  # Plot estimated effects
  lines(y=delta_1, x=x_2)
  lines(y=upper_bound, x=x_2, lty=2)
  lines(y=lower_bound, x=x_2, lty=2)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
  # Add a vertical line at the mean
  if (mean){
    abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
  }
  
  # Add a vertical line at the median
  if (median){
    abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
  }
  
  # Add Rug plot
  if (rugplot){
    rug(mod_frame[[moderator]])
  }
  # Add Histogram (Histogram only plots when minimum and maximum are the min/max of the moderator)
  if (histogram & minimum=="min" & maximum=="max"){
    par(new=T)
    hist(mod_frame[[moderator]], axes=F, xlab="", ylab="",main="", border=hist_col, col=hist_col)
  }
}

## interaction_plot_binary: Plots the marginal effect for one variable interacted with a binary variable
## Usage
## Required
# model: linear or generalized linear model object returned by lm() or glm() function
# effect: name of the "effect" variable in the interaction (marginal effect plotted on y-axis) - character string
# moderator: name of the moderating variable in the interaction (plotted on x-axis) - character string - Variable must be binary (0-1)
# interaction: name of the interaction variable in the model object - character string
## Optional
# varcov: Variance-Covariance matrix - if default, then taken from the model object using vcov()
# conf: Size of confidence interval around coefficient estimates - 0-1, default is .95 (95% confidence)
# title: Title of the plot
# xlabel: Label of the X axis
# ylabel: Label of the Y axis
# factor_labels: Labels for each of the two moderator values - default = "0" and "1"
interaction_plot_binary <- function(model, effect, moderator, interaction, varcov="default", conf=.95, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", factor_labels=c(0,1)){
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- c(0,1)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(-.5, 1.5), xlab=xlabel, ylab=ylabel, main=title, xaxt="n")
  
  # Plot points of estimated effects
  points(x=x_2, y=delta_1, pch=16)
  
  # Plot lines of confidence intervals
  lines(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), lty=1)
  points(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), pch=c(25,24), bg="black")
  lines(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), lty=1)
  points(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), pch=c(25,24), bg="black")
  
  # Label the axis
  axis(side=1, at=c(0,1), labels=factor_labels)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
}

##### Examples!
## Demo on Air Quality data

# Load air quality Data
data(airquality)
# Linear model - Max Temperature (F) = Ozone Level (ppb) + Avg Wind Speed (mph) + Ozone*WindSpeed
test_model <- lm(Temp ~ Ozone + Wind + Wind*Ozone, data=airquality)
summary(test_model)

# Create an interaction plot
interaction_plot_continuous(test_model, effect="Wind", moderator="Ozone", interaction="Ozone:Wind", mean=T, title="Relationship between Wind Speed and Maximum Temperature\nfor different levels of Ozone (ppb)",xlabel="Mean Ozone (ppb)", ylabel="Marginal effect of wind speed (mph) on temperature (F)")

## Demo on LaLonde dataset
require(Matching)
data(lalonde)

# Earnings in '78 = Treatment (Job training) + Married + treatment*married
test_model_2 <- lm(re78 ~ treat + married + treat*married, data=lalonde)
summary(test_model_2)

# Create an interaction plot with binary moderator
interaction_plot_binary(test_model_2, effect="treat", moderator="married", interaction="treat:married", factor_labels=c("Not Married","Married"), xlabel="Marital status", ylabel="Effect of treatment assignment on earnings", title="Interaction between Job training assignment\nand marital status on earnings (Non-significant)")
