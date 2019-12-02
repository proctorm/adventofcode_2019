# Star 1
original_fuel <- function (module_weights) {
 (sum(module_weights) - sum(module_weights %% 3 == 1) - 2 * sum(module_weights %% 3 == 2) - 6 * length(module_weights)) / 3
}

source('module_weight_data.R')
original_fuel(module_weights)

# Star 2
library(purrr)

fuel_per_unit_mass <- function (mass) {
  fuel = floor(mass / 3) - 2
  fuel[fuel < 0] = 0
  return(fuel)
}

total_fuel <- function (module_weights) {
  # Calculate # iterations
  # Iterative sequence for max iterations is:
  # x_n = x_n-1 * 3 + 6, x_1 = 9, where n is the number of times we need to calculate the fuel
  # The analytical formula for this sequence...
  # Addition of 6 follows geometric series, otherwise we are multiplyng by 3 each time. The complete formula is:
  # x = 3 ^ (n + 1) - 6 * (1 - 3 ^ (n - 1) / (1 - 3)) 
  # This can be simplified and converted to get a formula for n
  # n = (log(x + 3) - log(4)) / log(3)
  max_iterations = (log(max(module_weights) + 3) - log(4)) / log(3)
  max_iterations[max_iterations < 0] = 0 # Very unlikely but could happen that the weight results in a negative number
  max_iterations = floor(max_iterations)
  max_iterations
  
  # Run purrr::reduce using the fuel_per_unit_mass function max_iterations number of times. Sum the result each time.
  total_fuel = accumulate(1:(max_iterations - 1), ~ fuel_per_unit_mass(..1), .init = fuel_per_unit_mass(module_weights)) %>% reduce(sum)
  return(total_fuel)
}

source('module_weight_data.R')
total_fuel(module_weights)
