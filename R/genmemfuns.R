`genmemfuns` <-
function (df, nlev = 3) 
{
    mktrimf <- function(cuts) sapply(1:(length(cuts) - 2), function(i) {
        f <- makefun(member3, unlist(c(cuts[i:(i + 2)], ramp), 
            use.names = F))
        if (!is.null(opt <- getOption("gcl.decorate")) && opt > 
            1) 
            attr(f, "gcl.ds") <- paste(c("[", cuts[i:(i + 2)], 
                "]"), collapse = " ")
        return(f)
    })
    indata <- as.data.frame(df)
    l <- apply(indata, 2, function(col) mktrimf(c(-Inf, quantile(col, 
        probs = seq(0, 1, 1/(nlev - 1))), Inf)))
    names(l) <- colnames(indata)
    return(l)
}
