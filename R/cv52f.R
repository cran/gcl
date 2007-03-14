`cv52f` <-
function (ev1, ev2) 
{
    em1 <- matrix(ev1, byrow = T, ncol = 2)
    em2 <- matrix(ev2, byrow = T, ncol = 2)
    delta <- em1 - em2
    vd <- apply(delta, 1, var)
    f <- sum(delta^2)/(2 * sum(vd))
    return(f)
}
