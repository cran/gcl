`applyrules` <-
function (point, rules) 
sapply(rules, function(r, x) c(r[[1]], r$f(x)), point)
