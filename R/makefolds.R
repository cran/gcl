`makefolds` <-
function (df, fold = 10, balance = T, rand = 0.1, ...) 
{
    makefoldsi <- function(df, fold = 10, ...) {
        addf <- 0
        if (!is.null(rand)) 
            addf <- rnorm(1, sd = rand)
        n <- round(nrow(df)/fold + rnorm(1, sd = 0.2))
        avail <- 1:nrow(df)
        set <- rep(F, nrow(df))
        folds <- t(sapply(1:(fold - 1), function(i) {
            inc <- sample(avail, n)
            avail <<- setdiff(avail, inc)
            set[inc] <- T
            return(set)
        }))
        set[avail] <- T
        folds <- rbind(folds, set)
        return(folds)
    }
    if (!balance) 
        return(makefoldsi(df, fold))
    clsv <- unlist(df[, ncol(df)])
    clss <- unique(unlist(df[, ncol(df)]))
    l <- sapply(clss, function(cls) list(makefoldsi(df[clsv == 
        cls, ], fold)))
    folds <- matrix(rep(F, fold * nrow(df)), nrow = fold)
    for (i in 1:length(clss)) folds[, clsv == clss[i]] <- l[[i]]
    return(folds)
}
