`fclassify` <-
function (df = NULL, rules, mkvm = makevm, ds = "") 
{
    if (is.null(df)) 
        return(ds)
    if (is.null(dim(df))) {
        tmp <- names(df)
        df <- matrix(df, nrow = 1)
        colnames(df) <- tmp
    }
    if (is.null(colnames(df))) 
        stop("Cannot classify data without attribute names, bye.\n", 
            call. = F)
    if (is.null(rules)) {
        warning("Empty rule set encountered, returning 0 memberships.\n", 
            call. = F)
        classes <- sort(unique(unlist(df[, ncol(df)])))
        m <- matrix(0, ncol = length(classes), nrow = nrow(df))
        colnames(m) <- classes
        return(m)
    }
    if (length(unlist(df)) == 0) {
        warning("Empty data set encountered, returning NULL.\n", 
            call. = F)
        return(NULL)
    }
    dovote <- function(mm, clss, rt, ...) {
        ncls <- length(clss)
        votes <- matrix(rep(0, ncol(mm) * ncls), ncol = ncls)
        colnames(votes) <- clss
        for (i in clss) {
            ci <- (1:ncls)[clss == i]
            ri <- rt == i
            if (sum(ri) > 0) {
                votes[, ci] <- pmax(votes[, ci], apply(mm[ri, 
                  , drop = F], 2, max))
            }
        }
        return(votes)
    }
    rt <- sapply(rules, function(v) v[[1]])
    clss <- sort(unique(unlist(rt)))
    return(dovote(mkvm(df, rules), clss, unlist(rt)))
}
