`inf` <-
function (p) 
sum(-p * sapply(p, securelog2))
