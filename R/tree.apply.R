`tree.apply` <-
function (tree, df, printer = print.tree, efun = function(v, 
    branch, ...) {
    (unlist(v[branch$test$attribute]) == branch$test$value) + 
        0
}, op = "+", ds = mktcl.licensestr(), first.only = F, ...) 
{
    if (is.null(df)) 
        return(paste(ds, printer(tree), sep = "\n"))
    tree.apply.point <- function(v, tree, curv = 1, true.fuzzy = F, 
        ...) {
        if (is.null(names(v))) 
            names(v) <- paste("V", 1:length(v), sep = "")
        l <- list()
        for (branch in tree) {
            amatch <- min(efun(v, branch, ...), curv)
            if (amatch > 0) {
                if (!is.null(branch$subtree$values)) {
                  st <- branch$subtree
                  if (true.fuzzy) 
                    st$counts <- rep(amatch, length(st$counts))
                  else st$counts <- st$counts * amatch
                  if (first.only) 
                    return(list(st))
                  l <- c(l, list(st))
                  next
                }
                val <- tree.apply.point(v, branch$subtree, curv = amatch, 
                  true.fuzzy = true.fuzzy, ...)
                if (!is.null(val)) {
                  if (first.only) 
                    return(val)
                  l <- c(l, val)
                }
            }
        }
        return(l)
    }
    clsv <- c()
    if (is.null(dim(df))) {
        if (class(df) != "list") 
            df <- list(df)
        l <- lapply(df, tree.apply.point, tree, curv = 1, ...)
    }
    else {
        l <- apply(df, 1, tree.apply.point, tree, curv = 1, ...)
    }
    clsv <- unique(unlist((lapply(l, lapply, function(v) v$values))))
    cs <- matrix(0, ncol = length(clsv), nrow = length(l))
    colnames(cs) <- sort(clsv)
    i <- 1
    for (rc in l) {
        sapply(rc, function(cpair, op) cs[i, paste(cpair$values)] <<- op(cs[i, 
            paste(cpair$values)], cpair$counts), op)
        i <- i + 1
    }
    return(cs)
}
