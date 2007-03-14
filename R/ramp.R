`ramp` <-
function (val, v1, v2) 
{
    if (val <= v1) 
        return(0)
    if (val >= v2) 
        return(1)
    return((val - v1) * 1/(v2 - v1))
}
