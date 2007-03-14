`makefun` <-
function (.thefun, .thealist, .moveddd = 1, ...) 
{
    frms <- formals(.thefun)
    frn <- names(frms)
    body <- body(.thefun)
    na <- length(frms)
    ddd <- names(frms)[na] == "..."
    add.ddd <- F
    if (ddd && .moveddd != 0) {
        na <- na - 1
        frms <- frms[1:na]
        frn <- frn[1:na]
        add.ddd <- T
    }
    if (.moveddd != 1) 
        add.ddd <- F
    if (is.null(names(.thealist))) {
        tmp <- rev(rev(names(frms))[1:min(length(.thealist), 
            na)])
        names(.thealist)[1:length(tmp)] <- tmp
    }
    frms[names(.thealist)] <- .thealist
    if (add.ddd) 
        frms <- c(frms, alist(... = ))
    return(as.function(c(frms, body), ...))
}
