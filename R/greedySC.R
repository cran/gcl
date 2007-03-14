`greedySC` <-
function (sets, cost = rep(1, nrow(sets)), mit = nrow(sets) * 
    5, mono = T, exact = F) 
{
    u <- rep(T, ncol(sets))
    m <- 1:ncol(sets)
    avail <- 1:nrow(sets)
    sol <- c()
    it <- 0
    while (sum(u) > 0 && length(avail) > 0 && it < mit) {
        it <- it + 1
        if (exact) 
            a <- Inf
        else a <- 0
        b <- 0
        for (i in avail) {
            if (exact) {
                d <- sum(sets[i, !u])/sum(sets[i, u])
                if (is.nan(d)) 
                  d <- Inf
                if (d <= a) {
                  a <- d
                  b <- i
                }
            }
            else {
                d <- sum(sets[i, u])/cost[i]
                if (d >= a) {
                  a <- d
                  b <- i
                }
            }
        }
        sol <- c(sol, b)
        res <- apply(sets[sol, , drop = F], 2, sum) > 0
        if (!mono) 
            u <- rep(T, ncol(sets))
        u[res] <- F
        if (!mono) {
            covers <- apply(sets[sol, , drop = F], 2, sum)
            positives <- covers > 0
            sump <- sum(positives)
            rem <- c()
            for (col in sol) {
                pos <- (covers - sets[col, ]) > 0
                if (sum(pos) > 0 && (sump == 0 || sum(pos[positives]) == 
                  sump)) {
                  rem <- c(rem, col)
                  covers <- covers - sets[col, ]
                }
            }
            if (!is.null(rem)) {
                sol <- setdiff(sol, rem)
                if (length(sol) == 0) 
                  sol <- c()
                avail <- union(avail, rem)
            }
        }
        avail <- setdiff(avail, b)
    }
    return(sol)
}
