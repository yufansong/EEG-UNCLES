######################################################################
#函数�?:clustDist
#函数功能:找到两个簇之间的距离
#参数:
#   U1:矩阵1(U代表的是将原始数据X聚类之后的结�?)
#   U2:矩阵2
#   X:原始数据
#   criterion:  计算距离的方�? direct_euc     centers_euc    union_std
#返回�?:    D : distance
#   
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################

clustDist <- function(U1, U2, X = matrix(, 0, 0), criterion = "direct_euc") {
    if (criterion == "direct_euc") {
        return(as.matrix(pdist::pdist(U1, U2)))
    } else if (criterion == "centers_euc") {
        # find the centers
        N = ncol(X)
        centres1 = (U1 %*% X) / (rowSums(U1) %*% rep(1, N))         #????X没传入东西，那么N�?0  %*%�?
        centres2 = (U2 %*% X) / (rowSums(U2) %*% rep(1, N))

        # find the distances between the centres
        D = as.matrix(pdist::pdist(centres1, centres2))
        m = max(D[!is.na(D)])
        if (!isempty(m)) {
            D[is.na(D)] = m + 1
        }
        return(D)
    } else if (criterion == "union_std") {
        N = ncol(X)
        K1 = nrow(U1)
        K2 = nrow(U2)
        D = matrix(, K1, K2)

        for (i in 1:K1) {
            for (j in 1:K2) {
                uU = pmax(U1[i,], U2[j,])
                uCentre = (uU %*% X) / (rowSums(uU) %*% rep(1, N))  #聚类所谓的中心到底是什么意思，具体是什么算�??
                dists = as.matrix(pdist::pdist(X, uCentre))         #计算原始数据X，和处理之后的center的距�?
                D[i, j] = (t(dists) %*% uU) / sum(uU)
            }
        }
        return(D)
    } else {
        stop("Unsupported distances")
    }
}