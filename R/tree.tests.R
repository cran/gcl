`tree.tests` <-
function (tree) 
{
    if (is.null(tree)) 
        return(list())
    l <- c()
    for (branch in tree) {
        if (is.null(branch$subtree$values)) 
            l <- c(l, tree.tests(branch$subtree))
    }
    l <- c(l, tree[[1]]$test$attribute)
    return(unique(l))
}
