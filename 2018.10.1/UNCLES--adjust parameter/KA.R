######################################################################
#函数�?:KA(Kaufman approach)
#函数功能:Perform Kaufman’s initialisation for k-means clustering
#       该方法在 Kaufman L; Rousseeuw P J ;An introduction to cluster ananlysis; 1990中提�?
#       该方法逐个从数据集中选出代表点，知道取到K个点为止，第一个代表点�?
#       离数据集中心最近的那个点，其余的代表点根据一个启发式的规则从剩余点取�?
#参数:
#   X:矩阵
#   K:K�?
#   distancemetrix:传入dist函数计算方法
#返回�?:    
#   X：从输入开始并没有做什么操作，只是从中计算均值，然后构造出距离矩阵
#      然后挑选最大最小值算出index，然后最后用index挑选x的行输出
#编写者：宋宇�?
#时间�?2018/5/6
########################################################################

KA <- function(X, K, distancemetric = "euclidean") {
    M = nrow(X)
    Dists = as.matrix(dist(X, method = distancemetric))

    ResultInd = rep(0, K)
    Xmean = colMeans(X)
    Dmean = as.matrix(pdist::pdist(X, Xmean))

    ResultInd[1] = which(Dmean == min(Dmean))[1]

    for (k in 1:(K - 1)) {
        D = apply(as.matrix(Dists[, ResultInd[1:k]]), 1, min)
        C = rep(0, M)
        for (i in 1:M) {
            if (all(ResultInd == i)) {
                next
            }
            C[i] = sum(pmax(D - Dists[, i], 0))
        }
        ResultInd[k + 1] = which(C == max(C))[1]
    }
    return(X[ResultInd,])
}