`print.tree` <-
function (tree) 
{
    ptree <- function(tree, level = 0) {
        indent <- function(level) rep("+", level)
        if (is.null(tree)) 
            return(c())
        l <- c()
        for (branch in tree) {
            l <- c(l, indent(level), branch$test$attribute, "=", 
                branch$test$value)
            if (!is.null(branch$subtree$values)) 
                l <- c(l, "[", paste(branch$subtree$values, branch$subtree$counts, 
                  sep = ":", collapse = ","), "]\n")
            else l <- c(l, "\n", ptree(branch$subtree, level + 
                1))
        }
        return(l)
    }
    paste(ptree(tree), collapse = "")
}
