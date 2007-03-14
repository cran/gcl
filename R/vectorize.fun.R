`vectorize.fun` <-
function (fun) 
function(v) sapply(v, fun)
