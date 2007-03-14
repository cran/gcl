`genrulesm` <-
function (indata, cfun = function(a, b, ...) 1, ...) 
{
    dims <- dim(indata)
    rules <- c()
    ants <- c()
    ocs <- c()
    for (i in 1:dims[1]) {
        targets <- indata[, dims[2]] != indata[i, dims[2]]
        if (sum(targets) == 0) 
            next
        cost <- apply(indata[, -ncol(indata)], 2, cfun, indata[, 
            ncol(indata)])
        r <- mdarel(indata[targets, -ncol(indata), drop = F], 
            indata[i, -dims[2]], cost = cost)
        if (is.null(r)) 
            next
        set <- rep(0, dims[2] - 1)
        set[r] <- indata[i, r]
        ants <- c(ants, as.vector(indata[i, dims[2]]))
        ocs <- c(ocs, i)
        rules <- rbind(rules, set)
    }
    if (is.null(dim(rules))) {
        warning("All predictor or prediction values the same.\n", 
            call. = F)
        return(NULL)
    }
    rownames(rules) <- c()
    rules <- as.data.frame(rules)
    rules <- cbind(rules, ants)
    colnames(rules) <- colnames(indata)
    rules[, ncol(rules)] <- indata[ocs, ncol(indata)]
    return(rules)
}
