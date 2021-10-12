stpetersburg <- function(n, cap=10**6)
{
  output <- numeric()
  for(i in 1:n)
  {
    current <- 1
    heads <- 0
    while(current==1)
    {
      current <- rbinom(1, 1, 0.5)
      if (current==1)
      {
        heads <- heads+1
      }
    }
    output[i] <- floor(2**heads/2)*2 #the reason for the seemingly pointless floor and multiplying/dividing by 2
    #is to cover the case of heads=0 neatly in one line without using an if statement
  }
  output[output>cap] <- cap
  #with no cap, this "game" has infinite expected payout. Adding a cap to the payout allows one to see how 
  #dramatically this expected payoff can be curtailed even with high caps
  return(output)
}