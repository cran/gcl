`makerules` <-
function (df, nlev = 3, multi = NULL, cfun = function(a, b, ...) 1, 
    ...) 
{
    mkrules <- function(df, nlev, cfun, ...) {
        memfns <- genmemfuns(as.matrix(df[, -ncol(df)]), nlev)
        ddf <- fdisc(as.matrix(df[, -ncol(df)]), memfns)
        ddf <- cbind(ddf, df[, ncol(df)])
        colnames(ddf) <- colnames(df)
        return(makerulesfromm(genrulesm(ddf, cfun, ...), memfns))
    }
    if (is.null(multi)) 
        return(mkrules(df, nlev, cfun = cfun, ...))
    folds <- makefolds(df, multi)
    r <- c()
    for (i in 1:multi) r <- c(r, mkrules(df[!folds[i, ], ], nlev, 
        cfun = cfun, ...))
    return(r)
}
