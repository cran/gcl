`mdarel` <-
function (tmat, row, cols = 1:length(row), cost = rep(1, length(row))) 
{
    targets <- 1:nrow(tmat)
    sol <- c()
    while (length(targets) > 0 && length(cols) > 0) {
        diffs <- sapply(cols, function(col) sum(tmat[targets, 
            col] != row[col]))/cost[cols]
        best <- cols[which.max(diffs)]
        targets <- targets[tmat[targets, best] == row[best]]
        cols <- cols[cols != best]
        sol <- c(sol, best)
    }
    return(sol)
}
