`cv52` <-
function (df, mb1, mb2, eval, seed = NULL, cv.verbose = F, ...) 
{
    if (!is.null(seed)) {
        if (cv.verbose) 
            cat("cv52: setting seed to:", seed, ".\n")
        set.seed(seed)
    }
    tcis2 <- c()
    lcis2 <- c()
    seeds <- sample(1:1e+06, 5)
    for (i in 1:5) {
        if (cv.verbose) 
            cat(" fold", i, "...\n")
        tcis2 <- c(tcis2, cv(df, mb1, eval, fold = 2, seed = seeds[i], 
            cv.verbose = cv.verbose, ...))
        lcis2 <- c(lcis2, cv(df, mb2, eval, fold = 2, seed = seeds[i], 
            cv.verbose = cv.verbose, ...))
    }
    em1 <- matrix(tcis2, byrow = T, ncol = 2)
    em2 <- matrix(lcis2, byrow = T, ncol = 2)
    delta <- em1 - em2
    m <- cbind(em1, em2, delta, apply(delta, 1, mean))
    colnames(m) <- c("CF1 fold 1", "CF1 fold 2", "CF2 fold 1", 
        "CF2 fold 2", "Delta Fold 1", "Delta fold 2", "Mean Delta")
    rownames(m) <- paste("Repetition", 1:5)
    if (cv.verbose) 
        print(m)
    if (cv.verbose) 
        cat(" Mean Performance difference:", mean(tcis2 - lcis2), 
            "\n")
    f <- cv52f(tcis2, lcis2)
    pv <- pf(f, 10, 5)
    if (cv.verbose) 
        cat("5x2 CV F statistic:", f, ", P-value:", pv, "\n")
    if (cv.verbose) 
        cat(" Difference is")
    if (cv.verbose && pv >= 0.95) 
        cat(" significant ")
    else cat(" not significant ")
    if (cv.verbose) 
        cat("at a 0.95 confidence level.\n")
    return(list(f = f, p = pv, m = m, seeds = seeds))
}
