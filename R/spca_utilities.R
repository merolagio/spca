

list2vec = function(li, uniq = T, sorted = F) {
  a = c(unlist(li))
  if (uniq)
    a = unique(a)
  if (sorted)
    a = sort(a)
  return(a)
}

vec2list = function(vec) {
  u = unique(vec)
  li = vector("list", length(u))
  for (j in seq_along(u)) li[[j]] = which(vec == u[j])
  names(li) = u
  li
}

fac2list = function(fac) {
  u = levels(fac)
  li = vector("list", length(u))
  for (j in seq_along(u)) li[[j]] = which(fac == u[j])
  names(li) = u
  li
}

list2fac = function(x) {
  ca = sapply(x, length)
  if (is.null(names(x)))
    namex = paste0("e", seq_along(x)) else namex = names(x)
  fa = c(unlist(x))
  fa = rep(0, length(fa))
  for (i in seq_along(x)) {
    fa[x[[i]]] = i
  }
  return(factor(fa, labels = namex))
}

vec2fac = function(v) {
  u = unique(v)
  u
  val = rep(0, length(v))
  for (i in seq_along(u)) {
    val[v == u[i]] = i
  }
  factor(val, labels = u)
}

# Rounds a list prints or return a rounded list @param li a list of numerical
# objects @param d nuber of digits @return rounded list
roundl = function(li, d = 2) {
  lapply(li, round, digits = d)
}
