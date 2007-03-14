`rema` <-
function (t) 
{
    normalize <- function(v) if (sum(v) == 0) 
        v
    else v/sum(v)
    nis <- apply(t(apply(t, 1, normalize)), 1, inf)
    return(sum(t * nis)/sum(t))
}
