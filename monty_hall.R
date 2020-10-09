# CHECK the rules: https://en.wikipedia.org/wiki/Monty_Hall_problem
montyhall <- function(strategy_change, nrep) {
  # compute probability of winning for both strategies
  # strategy_change == TRUE means change
  # strategy_change == FALSE means staying
  win <- 0
  for(i in 1: nrep){
    car <- sample(c(1,2,3),1) # production chooses where to put the car
    chosen <- sample(c(1,2,3),1) # participant chooses a door
    if (!strategy_change) {
      win <- win + (car == chosen) # sum one if the car is under chosen door
    }else{
      if (car == chosen) { # open any of the other doors
        open_door <- sample(setdiff(c(1,2,3),chosen),1,prob=c(0.5,0.5)) 
      } else { # open the door with a goat
        open_door <- setdiff(c(1,2,3),c(car,chosen))
      }
      win <- win + (car == setdiff(c(1,2,3),c(chosen,open_door)))
    }
  }
  return(win/nrep)
}

montyhall(TRUE, 10000) # prob of winning changing
montyhall(FALSE, 10000) # prob of winning staying
