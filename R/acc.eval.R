`acc.eval` <-
function (cf, df, ...) 
{
    cs <- cf(df[, -ncol(df)])
    if (is.null(cs)) 
        return(NA)
    return(sum(apply(paired.call(cs, df), 1, function(v) (v[1] == 
        v[2]) + 0), na.rm = T)/nrow(df))
}
