`fdisc` <-
function (df, memfuns) 
{
    for (n in names(memfuns)) {
        tmp <- sapply(memfuns[[n]], function(fun, x) sapply(x, 
            fun), df[, n])
        if (is.null(dim(tmp))) 
            df[, n] <- which.max(tmp)
        else df[, n] <- apply(tmp, 1, which.max)
    }
    return(df)
}
