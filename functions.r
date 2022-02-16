
missing_values <- function(dataset)
{
  for (i in 1:ncol(dataset))
  {
    if (is.numeric(dataset[,i]))
    {
      dataset[,i][is.na(dataset[,i])] = mean(is.na(dataset[,i])==F)
      next
    }
    if (is.factor(dataset[,i]))
    {
      dataset[,i][is.na(dataset[,i])] = tail(names(sort(table(dataset[,i]))), 1)
      next
    }
  }
  return(dataset)
}

# -------------------------------------------

outliers_grubbs <- function(dataset, alpha)
{
  for (i in 1:ncol(dataset))
  {
    if (is.numeric(dataset[,i])==TRUE)
    {
      while (grubbs.test(as.numeric(dataset[,i]))$p.value < alpha)
      {
        p1 <- grubbs.test(as.numeric(dataset[,i]))$alternative
        p2 <- readr::parse_number(p1)
        dataset <- dataset[-which(dataset[,i] %in% p2), ]
      }
    }
  }
  return(dataset)
}

# -------------------------------------------

coefficient_of_variation <- function(k) {
  x <- sd(k)/mean(k)
  return(as.numeric(x))
}

