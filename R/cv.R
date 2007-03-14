`cv` <-
function (df, mb, eval, fold = 2, seed = NULL, q.only = T, cv.verbose = F, 
    ...) 
{
    if (!is.null(seed)) {
        if (cv.verbose) 
            cat("cv: setting seed to:", seed, ".\n")
        set.seed(seed)
    }
    folds <- makefolds(df, fold, ...)
    if (cv.verbose) {
        cat("Class Prevalences in folds:\n")
        clsv <- unlist(df[, ncol(df)])
        clss <- unique(unlist(df[, ncol(df)]))
        prevs <- apply(folds, 1, function(row) sapply(clss, function(cl, 
            r) sum(clsv[r] == cl)/sum(r), row))
        colnames(prevs) <- paste("fold", 1:fold)
        rownames(prevs) <- clss
        print(prevs)
    }
    cq <- apply(folds, 1, function(idx) {
        if (cv.verbose) 
            cat("cv: processing fold...")
        cf <- mb(df[!idx, , drop = F], ...)
        qi <- eval(cf, df[idx, , drop = F])
        if (cv.verbose) 
            cat("cv: done with fold.\n")
        return(list(cf = cf, q = qi))
    })
    if (q.only) 
        return(sapply(cq, function(x) x$q))
    return(cq)
}
