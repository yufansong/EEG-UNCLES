HC <- function(X, K, distancemetric = "euclidean", method = "ward.D2") {
    clusters <- hclust(dist(X, method = distancemetric), method = method)
    clusterCut <- cutree(clusters, K)
    return(clustVec2partMat(clusterCut))
}