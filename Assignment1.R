#==========Question_1==============
#Given data
given_data <- c(1, 5, 10, 4)
NA_value <- 2
initial_mean <- 3

# Combining known and missing data
data <- c(given_data, rep(NA, NA_value))
data

# EM algorithm
max_iterations <- 100
convergence_point <- 0.05
iteration <- 1
previous_mean <- initial_mean

while (iteration <= max_iterations) {
  # E-step: Impute missing values using the current mean
  imputed_data <- ifelse(is.na(data), previous_mean,data)
  
  # M-step: Update mean estimate using imputed data
  new_mean <- mean(imputed_data)
  
  # Check for convergence
  if (abs(new_mean - previous_mean) < convergence_point) {
    break
  }
  
  previous_mean <- new_mean
  iteration <- iteration + 1
}

final_mean_estimate <- new_mean;final_mean_estimate
final_iterations <- iteration;final_iterations
estimated_data=imputed_data;estimated_data

#=====Question_2=====
set.seed(10)
data <- c(1, 5, NA, NA, 10, 4)
initial<- 9
max_iterations <- 100
convergence<- 0.05
iteration <- 1
previous_mean <- initial
while (iteration <= max_iterations) {
  imputed_data <- ifelse(is.na(data), previous_mean, data)
  new_mean <- sum(imputed_data, na.rm = TRUE) / length(imputed_data)
  if (abs(new_mean - previous_mean) < convergence) {
    break
  }
  
  previous_mean <- new_mean
  iteration <- iteration + 1
}
Estimated_Mean=new_mean;Estimated_Mean
Number_of_Iterations=iteration;Number_of_Iterations

#=====Question_3=====

set.seed(1710027)
EM = function(N, lambda, mis) {
  data = rpois(N, lambda)
  data[19:20] = NA
  m = round(sum(data, na.rm = T) / N)
  m
  mu = vector("numeric")
  for (i in 1:N) {
    m = (sum(data, na.rm = T) + mis * m) / N
    mu[i] = m
  }
  data[19:20] = mu[length(mu)]
  sd = sd(data)
  return(list(
    Mu = mu[length(mu)],
    Sigma = sd,
    All_mu = mu,
    Number_of_Iteration = i,
    Data = data
  ))
}
EM(20, 15, 2)


#=====Question_4=====
set.seed(1710027)
EM = function(N, lambda, mis) {
  data = rexp(N, rate = 1 / lambda)
  data[5:6] = NA
  m = round(sum(data, na.rm = T) / N)
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  mean1 = mean(data, na.rm = T)
  var1 = var(data, na.rm = T)
  data[5:6]  = mu[length(mu)]
  mean2 = mean(data)
  var2 = var(data) / N
  
  result = list(
    Mu = mu,
    Data = data,
    Mean_Estimate = mean2,
    Variance_Estimate = var2
  )
  return(result)
}
results = EM(10, 15, 2)

#a
MVUE = results$Mean_Estimate
MVUE

#b
UVSM = results$Variance_Estimate
UVSM



