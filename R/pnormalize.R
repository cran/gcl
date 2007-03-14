`pnormalize` <-
function (v, p = rep(1, length(v))) 
{
    if (sum(v == 0) == length(v)) 
        return(p/sum(p))
    return(v/sum(v))
}
