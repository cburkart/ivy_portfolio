# Backtesting function testing
# Inputs: matrix of holding patterns, matrix of prices, investment amt.
# All held in data frame of dimension m by 2n (rows by columns, n assets to track)
# 
backTest <- function(df, invest=100){
  m <- nrow(df)
  n <- ncol(df)/2
  
  # Create emtpy work matrix to hold calculations
  # Dimensions: m rows, n+1 columns
  # Will make reference to corresponding rows already loaded in incoming dataframe df
  # Rows: c(Shares_1, Shares_2, ... , Shares_n, Value)
  workmat <- data.frame(matrix(-9,nrow=m,ncol=n + 1))
  
  # Initialize the work matrix
  workmat[1,] <- cbind(invest*df[1, 1:n] / df[1, (n + 1):(2 * n)], invest)
  
  for (i in 2:m){
    # Populate value first: previous row shares times respective current prices
    currprice <- as.matrix(df[i,(n+1):(2*n)])
    prevshare <- as.matrix(workmat[i-1, 1:n])
    value <- currprice %*% t(prevshare)
    workmat[i,n + 1] <- value
    # Calculate current share holdings based on value and signal
    workmat[i, 1:(n)] <- value %*% (as.matrix(df[i, 1:n]) / as.matrix(df[i, (n+1):(2*n)]))
  }
  
  output <- xts(workmat[,n+1], order.by=index(df))
  output
}

