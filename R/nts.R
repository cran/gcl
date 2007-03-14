`nts` <-
function (l, n = 1) 
sapply(l, function(x, i) x[[i]], n)
