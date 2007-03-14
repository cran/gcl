`greedy` <-
function (collection, cost = rep(1, ncol(collection))) 
{
    collection <- collection[apply(collection, 1, sum) > 0, , 
        drop = F]
    return(greedySC(t(collection), cost))
}
