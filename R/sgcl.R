`sgcl` <-
function (df, cb = gcl, s.fold = 4, s.verbose = F, s.eval = acc.eval, 
    ...) 
{
    folds <- makefolds(df, fold = 3)
    tdata <- df[folds[1, ] | folds[2, ], ]
    vdata <- df[folds[3, ], ]
    if (s.verbose) 
        cat("sgcl: Starting CV...\n")
    cq <- cv(tdata, cb, eval = s.eval, q.only = F, fold = s.fold, 
        ...)
    if (s.verbose) 
        cat("sgcl: done CV.\n")
    q1 <- nts(cq, "q")
    cfs <- nts(cq, "cf")
    q2 <- sapply(cfs, function(cf, df) s.eval(cf, df), vdata)
    q <- ((q1 + q2)/2)/(1 + abs(q1 - q2))
    return(cfs[[which.max(q)]])
}
