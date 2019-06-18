partMat2clustVec <- function(U, skipValidity = FALSE) {
    M = ncol(U)
    C = rep(0, M)

    if (!skipValidity && !isValidBPM(U)) {
        stop("Invalid binary partition matrix U.")
    }

    for (i in 1:M) {
        if (any(U[, i])) {
            C[i] = which(U[, i])
        }
    }

    return(C)
}