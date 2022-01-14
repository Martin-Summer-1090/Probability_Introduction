roll_die <- function(){
  
  die <- 1:6
  
  sample(die, 1)
  
}

roll <- replicate(10000, roll_die())

qplot(roll, binwidth = 1)

