`makevm` <-
function (df, rules) 
apply(df, 1, function(point, rules) sapply(rules, function(r, 
    x) r$f(x), point), rules)
