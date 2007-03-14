`findff` <-
function (df, mb = gcl, eval = acc.eval, fval = seq(0.1, 0.5, 
    0.1), ...) 
{
    means <- sapply(fval, function(fv, ...) mean(cv(df, mb, eval, 
        filter = fv, ...)), ...)
    return(fval[which.max(means)])
}
