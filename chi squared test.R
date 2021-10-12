chisquare.test = function(actual, expected)
{
  chisq.statistic = sum((actual-expected)**2/expected)
  return(pchisq(sum(chisq.statistic), df=length(actual)-1, lower.tail=F))
}