`gainr` <-
function (a, b) 
{
    ia <- infa(a)
    if (ia == 0) 
        return(0)
    return(gain(a, b)/ia)
}
