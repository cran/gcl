`paired.call` <-
function (cs, df) 
{
    m <- cbind(make.call(cs), as.vector(df[, ncol(df)]))
    colnames(m) <- c("Call made", "Actual Class")[1:ncol(m)]
    return(m)
}
