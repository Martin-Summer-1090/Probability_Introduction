roll <- function(){
  
  # create a die
  
  die <- 1:6
  
  # roll the dice by making use of sample. We roll two dice, therefore size has to be equal to 2
  # note that we need to set replace to TRUE because both dice should be able to show all possible
  # points
  
  dice <- sample(die, size = 2, replace = TRUE)
  
  # sum up the points of the two dice
  
  sum(dice)
  
}