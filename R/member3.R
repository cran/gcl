`member3` <-
function (x, v1, v2, v3, ramp = ramp) 
{
    if (x <= v2) {
        if (v1 == -Inf) 
            return(1)
        return(ramp(x, v1, v2))
    }
    if (v3 == Inf) 
        return(1)
    return(1 - ramp(x, v2, v3))
}
