`mcnemar` <-
function (corr1, corr2) 
{
    rr <- sum(corr1 & corr2)
    rw <- sum(corr1 & !corr2)
    wr <- sum(corr2 & !corr1)
    ww <- sum(!corr1 & !corr2)
    if (rw + wr == 0) {
        warning("McNemar: diagonal contingency matrix, Z-statistic not defined.")
        return(NA)
    }
    z = (abs(rw - wr) - 1)/sqrt(rw + wr)
    p = 2 * (1 - pnorm(z))
    return(p = p, z = z)
}
