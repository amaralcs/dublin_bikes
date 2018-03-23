"
  Author: Carlos Amaral
  Date: 18/11/17
  Last modified: 22/11/17
  Description: 
      This applies the queuing model referenced in the paper by
      Schuijbroek, Hampshire and van Hoeve.
"

" NEED TO FIX THE FUNCTION G "

" Development on this stopped since we opted for going with the random forest model"

library(tidyverse)
library(lubridate)

############################### Define support functions ##########################
# Define function pii(sigma)
pii <- function(sigma, Ci, ro){
  return (
    if_else(
      ro == 1,
      1/(Ci+1),
      (1-ro)/(1-ro^(Ci+1))*ro^(sigma)
    )
  )
}

# Define function K (capital K to distinguish from lowercase k)
cap_K <- function(m, mu, k, s, Ci, ro, sigma){
  # Define each component separetely for clarity
  mu_k_ratio <- mu/k
  arg_sin_start_a <- (m*s*pi)/(Ci+1)
  arg_sin_start_b <- (m*(s+1)*pi)/(Ci+1)
  arg_sin_target_a <- (m*sigma*pi)/(Ci+1)
  arg_sin_target_b <- (m*(sigma+1)*pi)/(Ci+1)
  
  product <- mu_k_ratio * 
    (sin(arg_sin_start_a) - sqrt(ro)*sin(arg_sin_start_b)) *
    (sin(arg_sin_target_a) - sqrt(ro)*sin(arg_sin_target_b))
  
  return(product)
}

# Define function k (lowecase)
low_k <- function(lambda, mu, m, Ci){
  arg_cos <- (m*pi)/(Ci+1)
  
  return (lambda + mu - 2*sqrt(lambda*mu)*cos(arg_cos))
}

# Define function for the sum in the probability
prob_sum <- function(Ci, s, ro, sigma, lambda, mu, t){
  # var to keep track of sum
  ret <- 0
  
  # Iterate over m and add to sum
  for(m in 1:Ci){
    k <- low_k(lambda, mu, m, Ci)
    K <- cap_K(m, mu, k, s, Ci, ro, sigma)
    e_of_t <- exp(-k*t)
    
    ret <- ret + K*e_of_t/k
  }
  
  return (ret)
}

# Define function for Probability at time t
prob <- function(Ci, s, sigma, ro, lambda, mu, t){
  frac <- (2*ro^((sigma-s)*1/2))/(Ci+1)
  m_sum <- prob_sum(Ci, s, ro, sigma, lambda, mu, t)
  
  return (pii(sigma, Ci, ro)*t - frac*m_sum)
}

##################################################################

df <- read_rds("./saved_data_frames/db_queue_data.rds")

# Initial try for charlemont
ch_df <- df %>%
  filter(number == 5)

"Below is a description of the variables used:
  i <- station number
  Ci <- capacity of station i (i.e. number of docks)
  s <- # of bikes at station i at time t
  lambda <- mean customer inter arrival (bike returns)
  mu <- mean service time (bike pickups)
  ro <- customer ia / service time
"
temp <- ch_df %>% 
  mutate(
    weekday = weekdays(date, abbreviate = TRUE)
  ) %>%
  group_by(number, name, weekday, h) %>%
  summarise(
    Ci = first(bike_stands), # For this it doesn't matter what grouping we take, there's only one value
    s = first(available_stands), # For this it doesn't matter what grouping we take, there's only one value
    lambda = as.numeric(mean(cust_ia, na.rm = TRUE)),
    mu = as.numeric(mean(service_ia, na.rm = TRUE)),
    ro = lambda / mu
  )

" This sets up most of the variables. Next, we need to find Pi(s, sigma, T) by summing over Ci"  
" sigma is however many bikes we want to have at time t"
" lets take sigma = 30 for testing purposes "
sigma <- 30

######################### simple test for the functions ############
" Test functions one at a time "
temp$lambda[1]
temp$mu[1]
temp$Ci[1]

low_k(temp$lambda[1], temp$mu[1], 5, temp$Ci[1])

cap_K(5, temp$mu[1], 4.389882, temp$s[1], temp$Ci[1], temp$ro[1], sigma)

# Test for t = 1
prob(temp$Ci, temp$s, sigma, temp$ro, temp$lambda, temp$mu, 1)
prob(temp$Ci, temp$s, sigma, temp$ro, temp$lambda, temp$mu, 0)


######################### Define g(s, sigma) ###########################
" There's a fatal flaw with this method in that it doesn't take the initial period values and 
  finishing period values properly, it is actually taking the same values for both periods
  Need to rework this, perhaps sending the data frame and the specified time periods then filter
  based on that."
func_g <- function(Ci, s, sigma, ro, lambda, mu, t, ti){
  return ((1/t)*(prob(Ci, s, sigma, ro, lambda, mu, t) - prob(Ci, s, sigma, ro, lambda, mu, ti)))
}

######################### Define s_min, s_max ##########################
# Set satisfaction levels for pick ups / returns
beta_min <- 0.95
beta_max <- 0.95

# function to find a minimum requirement for sevice level
s_min <- function(Ci, sigma, ro, lambda, mu, t, ti){
  sigma <- 0
  # Set minimum to be a very large value at first
  min <-  .Machine$integer.max
  
  # Loop through possibilities to find the one that minimises it
  for(s in 0:Ci){
    curr_s <- 1 - func_g(Ci, s, sigma, ro, lambda, mu, t, ti)
    #print(paste("s:", s, "~", curr_s))
    # Check if this value satisfies threshold
    if(curr_s >= beta_min){
      # Return s, as this will be the minimum that satisfies the threshold, since values increase
      return(s)
    }
  } 
}

# function to find a maximum boundary for sevice level
s_max <- function(Ci, sigma, ro, lambda, mu, t, ti){
  sigma <- Ci
  # Set max to be a small value at first
  argmax <- -1
  
  # Loop through possibilities to find the one that minimises it
  for(s in 0:Ci){
    curr_s <- 1 - func_g(Ci, s, sigma, ro, lambda, mu, t, ti)
    #print(paste("s:", s, "~", curr_s))
    # Check if this value satisfies threshold
    if(curr_s >= beta_max){
      # check if this is out new max
      if(s > argmax){
        argmax <- s
      }
    }
  } 
  #print(paste("s: ", max))
  return (argmax)
}
  
# Example calculations
s_min(temp$Ci[1], sigma, temp$ro[1], temp$lambda[1], temp$mu[1], 10, 6)
s_max(temp$Ci[1], sigma, temp$ro[1], temp$lambda[1], temp$mu[1], 10, 6)

temp %>%
  filter(weekday == "Mon" & h >5) %>%
  
  
