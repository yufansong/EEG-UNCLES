######################################################################
#函数�?:clustVec2partMat
#函数功能: Converts a clustering result vector to a partition matrix
#参数: C:传入的聚类结果，是一个vector
#返回�?: U：把传入的vec变成矩阵
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################

clustVec2partMat <- function(C) {
    labels = unique(C[C != 0])
    if (length(labels) == 0) {
        return(rep(0, length(C)))
    }

    K = max(labels)
    N = length(C)
    U = matrix(FALSE, K, N)                 #每一行代表一种分类数�?(K),N是C的所有非零数�?
    for (i in 1:N) {
        U[labels[labels == C[i]], i] = TRUE #?????
    }

    if (sum(rowSums(U) == 0) > 0) {
        U = U[ - which(rowSums(U) == 0),]   #?????
    }
    return(U)
}