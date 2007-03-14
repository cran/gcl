`ci.eval` <-
function (cf, df, ...) 
{
    clsv <- unlist(df[, ncol(df)])
    clss <- unique(clsv)
    if (length(clss) != 2) 
        stop("Cannot compute CI on non-binary outcomes. Aborting.\n", 
            call. = F)
    clss <- sort(clss)
    cs <- cf(df[, -ncol(df)], ...)
    if (is.null(cs)) 
        return(NULL)
    csc <- colnames(cs)
    prev <- sum(clsv == clss[2])/length(clsv)
    csn <- t(apply(cs, 1, pnormalize, c(1 - prev, prev)))
    pred <- csn[, 2]
    act <- clsv == clss[2]
    return(cindex(pred, act)[1])
}
