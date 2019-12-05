# Star 5

p0 <- list(x = 0, y = 0)

go_left <- function(p, steps) {
  list(
    x = c(p$x, p$x[length(p$x)] - 1:steps), 
    y = c(p$y, rep(p$y[length(p$y)], length(1:steps)))
  )
}

go_right <- function(p, steps) {
  list(
    x = c(p$x, p$x[length(p$x)] + 1:steps), 
    y = c(p$y, rep(p$y[length(p$y)], length(1:steps)))
  )
}

go_up <- function(p, steps) {
  list(
    x = c(p$x, rep(p$x[length(p$x)], length(1:steps))), 
    y = c(p$y, p$y[length(p$y)] + 1:steps)
  )
}

go_down <- function(p, steps) {
  list(
    x = c(p$x, rep(p$x[length(p$x)], length(1:steps))), 
    y = c(p$y, p$y[length(p$y)] - 1:steps)
  )
}

library(stringr)
library(dplyr)

parse_instruction <- function (p, instruction) {
  direction = which(str_detect(instruction, c('L', 'R', 'U', 'D')))
  magnitude = as.numeric(str_extract(instruction, '[:digit:]+'))
  go = list(go_left,go_right, go_up, go_down)
  return((go[[direction]])(p, magnitude))
}

return_intercepts <- function(path1, path2) {
  
}