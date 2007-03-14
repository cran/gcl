`filterrules` <-
function (rules, df, mono = F, ...) 
{
    rt <- unlist(sapply(rules, function(v) v[[1]]))
    dt <- df[, ncol(df)]
    filter <- function(vm, rt, dt, bc = 10, mono = T) {
        cm <- outer(rt, dt, function(a, b) {
            n <- a == b
            n[!n] <- -1
            return(n)
        })
        vm <- vm * cm
        maxvm <- max(abs(vm))
        classes <- unique(rt)
        for (i in classes) {
            av <- (rt == i) + 0
            nc <- ceiling(bc * (length(rt) - sum(av)))
            vm <- cbind(vm, av * (nc * maxvm))
        }
        return(greedySC(vm, mono = mono))
    }
    keepr <- filter(makevm(df[, -ncol(df)], rules), rt, dt, mono = mono)
    return(rules[keepr])
}
