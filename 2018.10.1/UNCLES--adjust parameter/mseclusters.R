######################################################################
#函数�?:mseclusters
#函数功能:Calculate the MSE values for clusters
#       论文中有对应的公式，但是voxel，BOLD signal是什么意思，代码中的dateset和partition有什么关�??代表原始数据和聚类结果矩阵？
#参数:
#   X：原始数据X
#   B：定义见mnplots
#   normalise：矩阵是否进行归一�?
#返回�?:    
#   mse：mse[k,] = colMeans(mseC[[k]])
#   mseC:根据X，B的一些参数计算出来的
#编写者：宋宇�?
#时间�?2018/5/6
########################################################################

mseclusters <- function(X, B, normalise = TRUE) {
    if (!is.list(X) || is.data.frame(X)) {
        X = list(X)
    }
    if (!is.list(B) || is.data.frame(B)) {
        B = list(B)
    }

    # Number of datasets and number of partitions
    Nx = length(X)
    Nb = length(B)
    for (i in 1:Nb) {
        if (is.vector(B[[i]])) {
            B[[i]] = matrix(B[[i]])
        }
    }

    # Number of genes
    M = nrow(B[[1]])
    # Number of clusters
    K = ncol(B[[1]])

    # Check numbers of genes in all datasets and partitions
    for (n in 1:Nx) {
        if (nrow(X[[n]]) != M) {
            stop("Unequal number of genes in datasets and partitions.")     #dataset �? partition的关�?????
        }
    }
    for (n in 1:Nb) {
        if (nrow(B[[n]]) != M) {
            stop("Unequal number of genes in datasets and partitions.")
        }
        if (ncol(B[[n]]) != K) {
            stop('Unequal number of clusters in partitions')
        }
    }

    # Prepare mseC list's matrices
    mseC = list()
    for (k in 1:K) {
        mseC[[k]] = matrix(, Nx, Nb)
    }

    # Prepare other matrices and vectors
    Nk = matrix(, K, Nb)
    Nd = vector(, Nx)
    # Number of genes per cluster
    for (n in 1:Nb) {
        Nk[, n] = colSums(B[[n]])
    }
    # Number of dimensions per dataset
    for (n in 1:Nx) {
        Nd[n] = ncol(X[[n]])
    }

    # Normalise (subtract the mean and divide by the std)
    if (normalise) {
        for (n in 1:Nx) {
            X[[n]] = normaliseMatrix(X[[n]], type = 4)
        }
    }

    # Calculations

    # Find the mse for each cluster in all partitions and datasets
    for (nx in 1:Nx) {
        for (nb in 1:Nb) {
            for (k in 1:K) {
                if (sum(B[[nb]][, k]) == 0) {
                    mseC[[k]][nx, nb] = NaN
                } else if (sum(B[[nb]][, k]) == 1) {
                    mseC[[k]][nx, nb] = 0
                } else {
                    Xlocal = X[[nx]][B[[nb]][, k],]
                    tmp = apply(Xlocal, 2, colMeans(Xlocal), FUN = '-')
                    tmp = sum(tmp ^ 2)
                    mseC[[k]][nx, nb] = tmp / Nd[nx] / Nk[k, nb]
                }
            }
        }
    }

    # Find the datasets-combined mse for each partition for each cluster
    mse = matrix(, K, Nb)
    for (k in 1:K) {
        mse[k,] = colMeans(mseC[[k]])
    }


    return(list(mse = mse, mseC = mseC))
}