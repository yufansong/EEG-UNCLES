kmeansKA <- function(X, K, distancemetric = "euclidean", iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong"), trace = FALSE) {
    K <- KA(X, K, distancemetric)
    r = kmeans(X, K, iter.max, nstart, algorithm, trace)$cluster
    return(clustVec2partMat(r))
}