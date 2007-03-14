`makerulesfromm` <-
function (rulem, memfuns) 
{
    if (is.null(rulem)) 
        return(NULL)
    selfun <- function(x, f, ind) f(x[[ind]])
    muxdemux <- function(x, funvector) min(sapply(funvector, 
        function(fun, x) fun(x), x))
    pr <- function(rule, nv) {
        inc <- rule > 0
        inc[length(rule)] <- F
        paste(paste(nv[inc], rule[inc], sep = "=", collapse = " & "), 
            paste(nv[length(rule)], rule[length(rule)], sep = "="), 
            sep = " => ")
    }
    antnames <- function(rule, names) {
        inc <- rule > 0
        inc[length(rule)] <- F
        return(names[inc])
    }
    nams <- colnames(rulem)
    nc <- ncol(rulem)
    opt <- getOption("gcl.decorate")
    if (is.null(opt)) 
        opt <- 0
    rules <- apply(rulem, 1, function(row) {
        rowi <- as.numeric(row[-nc])
        h <- function(i) {
            f <- makefun(selfun, c(memfuns[[nams[i]]][[rowi[[i]]]], 
                nams[i]))
            if (opt > 1) 
                attr(f, "gcl.ds") <- attr(memfuns[[nams[i]]][[rowi[[i]]]], 
                  "gcl.ds")
            return(f)
        }
        flist <- sapply(which(rowi > 0), h)
        l <- list(row[nc], makefun(muxdemux, list(flist)))
        names(l) <- c(nams[nc], "f")
        if (opt > 0) {
            s <- pr(row, nams)
            if (opt > 1) 
                s <- paste(s, paste(sapply(flist, attr, "gcl.ds"), 
                  collapse = ","))
            attr(l$f, "gcl.ds") <- s
            attr(l$f, "gcl.rant") <- antnames(row, nams)
        }
        return(l)
    })
    return(rules)
}
