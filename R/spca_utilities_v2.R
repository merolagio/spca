

#R helpers that do not depend on spca UI objects

list2vec = function(li, uniq = TRUE, sorted = FALSE) {
  a = unlist(li, use.names = FALSE)
  
  if (uniq)
    a = unique(a)
  
  if (sorted)
    a = sort(a)
  
  a
}


vec2list = function(vec) {
  u = sort(unique(vec))
  li = lapply(u, function(x) which(vec == x))
  names(li) = as.character(u)
  li
}


fac2list = function(fac) {
  fac = droplevels(fac)
  u = levels(fac)
  li = lapply(u, function(x) which(fac == x))
  names(li) = u
  li
}


fac2index = function(fac) {
  fac = droplevels(fac)
  vec = as.numeric(fac)
  (seq_along(fac))[order(vec)]
}


list2fac = function(x) {
  if (is.null(names(x)))
    namex = paste0("e", seq_along(x))
  else
    namex = names(x)
  
  ind = unlist(x, use.names = FALSE)
  fa = rep(NA_integer_, max(ind))
  
  for (i in seq_along(x))
    fa[x[[i]]] = i
  
  factor(fa, levels = seq_along(x), labels = namex)
}


vec2fac = function(v) {
  u = sort(unique(v))
  val = match(v, u)
  factor(val, levels = seq_along(u), labels = as.character(u))
}

# Rounds a list prints or return a rounded list @param li a list of numerical
# objects @param d nuber of digits @return rounded list
roundl = function(li, d = 2) {
  lapply(li, round, digits = d)
}
