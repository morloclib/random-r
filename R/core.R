morloc_setSeed <- function(seed) {
  set.seed(seed)
  return(NULL)
}

morloc_random_real <- function() {
  runif(1L)
}

morloc_random_int <- function() {
  sample.int(2147483647L, 1L)
}

morloc_random_bool <- function() {
  runif(1L) < 0.5
}

morloc_randomRange_real <- function(lo, hi) {
  runif(1L, min = lo, max = hi)
}

morloc_randomRange_int <- function(lo, hi) {
  sample(lo:hi, 1L)
}

morloc_bernoulli <- function(p) {
  runif(1L) < p
}

morloc_choice <- function(xs) {
  xs[[sample.int(length(xs), 1L)]]
}

morloc_weightedChoice <- function(pairs) {
  weights <- vapply(pairs, function(p) p[[1L]], numeric(1L))
  values <- lapply(pairs, function(p) p[[2L]])
  values[[sample.int(length(values), 1L, prob = weights)]]
}

morloc_sample <- function(n, xs) {
  idx <- sample.int(length(xs), n)
  xs[idx]
}

morloc_sampleWith <- function(n, xs) {
  idx <- sample.int(length(xs), n, replace = TRUE)
  xs[idx]
}

morloc_permute <- function(xs) {
  xs[sample.int(length(xs))]
}
