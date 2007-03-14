`cvcomp` <-
function (df, mb1, mb2, eval, folds = 5, seed = NULL, cv.verbose = F, 
    ...) 
{
    if (!is.null(seed)) {
        if (cv.verbose) 
            cat("cv: setting seed to:", seed, ".\n")
        set.seed(seed)
    }
    else {
        seed <- sample(1:1e+06, 1)
    }
    tcis <- c()
    lcis <- c()
    if (cv.verbose) 
        cat("Doing", folds, "fold CV...\n")
    if (cv.verbose) 
        cat(" seed:", seed, "\n")
    if (cv.verbose) 
        cat(" doing CF1:\n")
    tcis <- cv(df, mb1, eval, fold = folds, seed = seed, cv.verbose = cv.verbose, 
        ...)
    if (cv.verbose) 
        cat(" mean perfomance:", mean(tcis), "\n")
    if (cv.verbose) 
        cat(" doing CF2:\n")
    lcis <- cv(df, mb2, eval, fold = folds, seed = seed, cv.verbose = cv.verbose, 
        ...)
    if (cv.verbose) 
        cat(" mean performance:", mean(lcis), "\n")
    if (cv.verbose) 
        cat("\n")
    if (cv.verbose) 
        cat("Comparing:\n")
    if (cv.verbose) 
        cat(" Mean Performance Difference:", mean(tcis - lcis), 
            "\n")
    tt <- t.test(tcis, lcis, paired = T)
    if (cv.verbose) 
        print(tt)
    if (cv.verbose) 
        cat(" Difference was")
    if (cv.verbose) 
        if (tt$p.value < 0.05) 
            cat(" significant ")
        else cat(" not significant ")
    if (cv.verbose) 
        cat("at a 0.95 confidence level.\n")
    return(list(tt = tt, tcis = tcis, lcis = lcis))
}
