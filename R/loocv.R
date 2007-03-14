`loocv` <-
function (df, mb, eval, seed = NULL, cv.verbose = F, ...) 
{
    if (!is.null(seed)) {
        if (cv.verbose) 
            cat("cv: setting seed to:", seed, ".\n")
        set.seed(seed)
    }
    ps <- c()
    for (i in 1:nrow(df)) {
        cf <- mb(df[-i, , drop = F], ...)
        ps <- rbind(ps, cf(df[i, , drop = F]))
    }
    return(eval(function(x) ps, df))
}
