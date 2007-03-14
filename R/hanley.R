`hanley` <-
function (paref1, paref2, aaref, ctype = "pearson") 
{
    aaref2 <- aaref
    aaref1 <- aaref
    if (length(paref1) != length(aaref1) || length(paref2) != 
        length(aaref2) || length(paref1) != length(aaref2)) {
        warning("C-index difference test: series size difference\n")
        return(NA)
    }
    if (length(paref1) == 0) {
        warning("C-index difference test: empty input, nothing to do\n")
        return(NA)
    }
    ci1 <- cindex(paref1, aaref1)
    ci2 <- cindex(paref2, aaref2)
    auc1 <- ci1[1]
    se1 <- ci1[2]
    auc2 <- ci2[1]
    se2 <- ci2[2]
    zi <- aaref1 == 0
    oi <- !zi
    corr0s <- cor.test(paref1[zi], paref2[zi], method = ctype)$estimate
    corr1s <- cor.test(paref1[oi], paref2[oi], method = ctype)$estimate
    auc <- 0.5 * (auc1 + auc2)
    corr <- 0.5 * (corr0s + corr1s)
    if (is.na(corr)) {
        warning("correlation NA in hanley()\n")
        return(NA)
    }
    r <- hanleyR(corr, auc)
    z = (auc1 - auc2)/sqrt((se1 * se1) + (se2 * se2) - (2 * r * 
        se1 * se2))
    p = 2 * (1 - pnorm(abs(z)))
    return(c(p = p, auc1 = auc1, auc2 = auc2, se1 = se1, se2 = se2, 
        z = z, r = r, corr0s = corr0s, corr1s = corr1s))
}
