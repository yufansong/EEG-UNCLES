SOMs <- function(X, K, topo = "hexagonal", rlen = 100, alpha = c(0.05, 0.01),
    n.hood = "circular") {
    dims = closestToSquareFactors(K)
    som_grid <- kohonen::somgrid(xdim = dims[1], ydim = dims[2], topo = topo)
    som_model <- kohonen::som(X, grid = som_grid, rlen = rlen, alpha = alpha)
    return(clustVec2partMat(som_model$unit.classif))
}