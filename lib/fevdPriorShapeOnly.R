fevdPriorShape <- function (theta, p=6, q=9) 
{
  # Use a beta distribution (with parameters p and q) to describe the prior probability distribution of the shape parameter
  # Default values p = 6 and q = 9 from Martins and Stedinger (2000)
  # Author: Julia Lutz (MET Norway)
  res <- dbeta(-theta[3] + 0.5, shape1=p, shape2=q, log=TRUE)
  return(res)
}
