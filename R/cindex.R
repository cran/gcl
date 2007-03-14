`cindex` <-
function (preds, acts) 
{
    if (length(preds) != length(acts)) {
        warning("ci: length(preds) != length(act)\n")
        return(NA)
    }
    up <- sort(unique(preds))
    Q1 <- 0
    Q2 <- 0
    W <- 0
    n <- length(acts)
    ones <- sum(acts)
    zeros <- n - ones
    pairs <- ones * zeros
    if (pairs == 0) {
        warning("ci: no pairs\n")
        return(NA)
    }
    obelow <- 0
    zbelow <- 0
    for (l in up) {
        is <- (1:length(preds))[preds == l]
        iones <- sum(acts[is])
        izeros <- length(is) - iones
        oabove <- ones - (obelow + iones)
        zabove <- zeros - (zbelow + izeros)
        w <- iones * (zbelow + 0.5 * izeros)
        W <- W + w
        q2 <- iones * (zbelow * zbelow + zbelow * izeros + (1/3) * 
            izeros * izeros)
        Q2 <- Q2 + q2
        q1 <- izeros * (oabove * oabove + oabove * iones + (1/3) * 
            iones * iones)
        Q1 <- Q1 + q1
        obelow <- obelow + iones
        zbelow <- zbelow + izeros
    }
    cindex <- W/pairs
    csq <- cindex * cindex
    Q1 <- Q1/(pairs * ones)
    Q2 <- Q2/(pairs * zeros)
    n1 <- cindex * (1 - cindex)
    n2 <- (ones - 1) * (Q1 - csq)
    n3 <- (zeros - 1) * (Q2 - csq)
    se <- sqrt((n1 + n2 + n3)/pairs)
    q1 <- cindex/(2 - cindex)
    q2 <- (2 * csq)/(1 + cindex)
    n2 <- (ones - 1) * (q1 - csq)
    n3 <- (zeros - 1) * (q2 - csq)
    seb <- sqrt((n1 + n2 + n3)/pairs)
    m <- pairs/2
    sd <- sqrt((pairs * (ones + zeros + 1))/12)
    z = (W - m)/sd
    if (cindex < 0.5) 
        cindex <- 1 - cindex
    return(c(cindex = cindex, se = se, seb = seb, z = z, m = m, 
        sd = sd))
}
