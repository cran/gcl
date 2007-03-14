`findnv` <-
function (df, mb = gcl, eval = acc.eval, nvals = seq(2, 4, 1), 
    ...) 
{
    means <- sapply(nvals, function(nv, ...) mean(cv(df, mb, 
        eval, nlev = nv, ...)), ...)
    return(nvals[which.max(means)])
}
