`rpart` <-
function (dt, outcomes, rankf = gainr, inf.lim = 0.5, tcl.verbose = T) 
{
    dt <- as.matrix(dt)
    if (is.null(colnames(dt))) 
        colnames(dt) <- paste("V", 1:ncol(dt), sep = "")
    gains <- apply(dt, 2, rankf, outcomes)
    index <- which.max(gains)
    if (gains[index] == 0 || infa(outcomes) < inf.lim) {
        vals <- unique(outcomes)
        if (length(vals) == 1) 
            return(list(values = vals, counts = nrow(dt)))
        else return(list(values = vals, counts = apply(sapply(vals, 
            "==", outcomes), 2, sum)))
    }
    if (tcl.verbose) 
        cat("Splitting at: ", index, colnames(dt)[index], "gain(r):", 
            gains[index], ", inf in outcome", infa(outcomes), 
            "\n")
    tree <- c()
    li <- 1
    vals <- unique(dt[, index])
    for (v in vals) {
        incl <- dt[, index] == v
        if (sum(incl) == 0) {
            stop()
        }
        tree[[li]] <- list(test = list(attribute = colnames(dt)[index], 
            value = v), subtree = rpart(dt[incl, , drop = F], 
            outcomes[incl], rankf = rankf, inf.lim = inf.lim))
        li <- li + 1
    }
    return(tree)
}
