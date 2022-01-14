roll_die <- function(){
  
  die <- 1:6
  
  sample(die, 1)
  
}

roll <- replicate(10000, roll_die())

qplot(roll, binwidth = 1)

birthday_collisions <- function(n){
  
  1 - prod(365:(365-n+1))/365^n
  
}


b_38 <- sample(1:365, 38, replace = TRUE)

m_38 <- 38 - length(unique(b_38))

sim_38 <- function(){
  
  b_38 <- sample(1:365, 38, replace = TRUE) 
  
  38 - length(unique(b_38))
}

simprobs_38 <- replicate(10000, sim_38())

1- length(simprobs_38[simprobs_38 == 0])/10000

birthday_collisions(38)
