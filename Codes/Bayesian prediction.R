## File: bayes_soln.R
## Author: John Rothfels (rothfels)
## Description: Bayesian Prediction
## --------------------------------

MidpointRule <- function(f, min, max, k, ...) {
  ## Numerically integrates a function using the midpoint rule.
  ## Args:
  ##   f - the function to integrate
  ##   min - low bound for integration
  ##   max - high bound for integration
  ##   k - the number of points to evaulate the function at
  ##   ... - any additional arguments to the function being integrated

  width <- (max - min) / k   # width of the Reimann rectangles
  heights <- sapply(1:k, function(i) f(min + (i - 0.5) * width, ...))
  return(sum(width * heights))
}

UPrior <- function(theta) {
  ## The unnormalized prior density function for the +1/-1 model. The valid
  ## range for theta is (0, 1) with the endpoints not included.
  ## Returns:
  ##   f(theta)

  val <- theta^(-1/2) * (1 - theta)^(-1/2) * (0.2 + exp(-100 * (theta - 0.5)^2))
  return(val)
}

PriorNorm <- function(k) {
  ## Computes the normalizing constant for the prior density function.
  ## Args:
  ##   k - number of points to use for integration

  return(MidpointRule(UPrior, 0, 1, k))
}

Likelihood <- function(theta, x) {
  ## The likelihood function. This is the probability of seeing x given theta.
  ## Args:
  ##   theta - distribution parameter
  ##   x - the data points (+1/-1 values)

  ChangeCount <- function(x) {
    ## Find the count of changes in the data sequence. Returns the number of
    ## times the value changes from one time to the next.

    n <- length(x)
    return(sum(x[2:n] != x[1:(n-1)]))
  }

  n <- length(x)
  c <- ChangeCount(x)
  return(0.5 * theta^(n - 1 - c) * (1 - theta)^c)
}

UPost <- function(theta, x) {
  ## The unnormalized posterior density.
  ## Args:
  ##   theta - prior distribution parameter
  ##   x - the data points (+1/-1 values)

  ## P(X, theta) = P(X | theta) * P(theta)
  return(UPrior(theta) * Likelihood(theta, x))
}

Predict <- function(x, k) {
  ## Predicts the next data point. Passed the previous data points and the
  ## number of points to use for integrations using the midpoint rule.
  ## Integrates the prediction for the next data point given theta with respect
  ## to the unnormalized posterior distribution of theta. Since the posterior
  ## is unnormalized, this results in unnormalized probabilities of the next
  ## point being +1 and -1, which are normalized with respect to the total
  ## unnormalized probability.
  ##
  ## Args:
  ##   x - the data points (+1/-1 values)
  ##   k - the number of points to use for integration
  ## Returns:
  ##   A list with fields $prediction and $probability. The $prediction field
  ##   is the value (+1/-1) predicted for the next data point (whichever has
  ##   higher probability of being the next data point). The $probability field
  ##   is the probability given to that prediction.



  uprob.same <- MidpointRule(function(theta, x)
                            theta * UPost(theta, x), 0, 1, k, x)
  uprob.diff <-  MidpointRule(function(theta, x)
                            (1 - theta) * UPost(theta, x), 0, 1, k, x)

  prob.same <- uprob.same / (uprob.same + uprob.diff)
  prob.diff <- uprob.diff / (uprob.same + uprob.diff)

  if (prob.same > prob.diff) {
    return(list(prediction = x[length(x)], prob = prob.same))
  } else {
    return(list(prediction = x[length(x)] * -1, prob = prob.diff))
  }
}

BayesFactor <- function(x, k) {
  ## Find the Bayes factor versus the trivial model.

  n <- length(x)
  return((MidpointRule(UPost, 0, 1, k, x) / PriorNorm(k)) / 0.5^n)
}
