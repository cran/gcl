`make.call` <-
function (cs) 
{
    return(colnames(cs)[apply(cs, 1, which.max)])
}
