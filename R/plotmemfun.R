`plotmemfun` <-
function (idx, memfns, x = seq(0, 1, 0.01)) 
{
    fns <- memfns[[idx]]
    lf <- length(fns)
    cols <- rainbow(lf)
    plot(x, sapply(x, fns[[1]]), col = cols[1], type = "l", ylab = "membership")
    for (i in 2:lf) lines(x, sapply(x, fns[[i]]), col = cols[i])
}
