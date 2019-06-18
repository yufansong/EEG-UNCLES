######################################################################
#函数�?:generateCoPaM
#函数功能:Generate a Consensus Partition Matrix (CoPaM) from partitions
#参数:
#   U:将原始数据X聚类之后的结果矩�?
#   relabel_technique:传入relabelClusts的参数brute / minmin / minmax / minmin_strict / minmax_strict
#   w:对于每一种聚类结果和聚类方法得出的结果，赋予一个权�?
#   X:原始数据X
#   disCriterion: 传入clusDist函数中用于计算距离的类型：direct_euc     centers_euc    union_std
#   K:聚类的类�?
#   GDM:bool矩阵，感觉是作为共识矩阵的索引进行一系列计算
#返回�?:  CoPaM：共识矩�?
#   
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################

generateCoPaM <- function(U, relabel_technique = "minmin", w = numeric(), X = numeric(), distCriterion = "direct_euc", K = 0, GDM = numeric()) {
    # Helper function (calwmeans)
    calwmeans <- function(ww) {
        wm = rep(0, length(ww))

        for (ii in 1:length(ww)) {
            if (is.list(ww[[ii]])) {
                wm[ii] = mean(calwmeans(ww[[ii]]))
            } else {
                wm[ii] = mean(ww[[ii]])
            }
        }

        return(wm)
    }

    # If U is not a list, make it a list, and find R (number of partitions) anyway
    if (!is.list(U)) {
        if (K == 0) {
            if (length(dim(U)) == 2) {              #dim????????
                K = sum(rowSums(U) > 0)
                U = list(U);
            } else if (length(dim(U)) == 3) {       #dim==3????U是什么，为什么会有三�?
                K = sum(rowSums(U[,, 1]) > 0)
                R = dim(U)[3]
                tmp = U
                U = list()
                r = 1
                for (i in 1:R) {
                    if (sum(rowSums(tmp[, , i]) > 0) == K) {
                        U[[r]] = tmp[,, i]
                        r = r + 1
                    }
                }
                R = r - 1
            }
        }
    } else {
        R = length(U)
    }

    # Fill the GDM with all TRUEs if not provided
    if (isempty(GDM)) {
        Utmp = U[[1]]
        while (is.list(Utmp)) {
            Utmp = Utmp[[1]]
        }
        GDM = matrix(TRUE, ncol(Utmp), R)
    }

    # Format w properly, and fill it if not provided
    if (isempty(w) || (is.character(w) && (tolower(w) == "all" || tolower(w) == "equal"))) {
        w = rep(1, R)                           #w????? 是一个数字还是字�????
    }
    if (length(w) == 1) {
        w = rep(w, R)
    }
    if (!is.list(w)) {
        w = as.list(w)
    }

    wmeans = calwmeans(w)

    ################# WORK!!

    # Random permutation of partitions
    permR = sample(R)
    U = U[permR]
    GDM = GDM[, permR]
    if (!is.matrix(GDM)) {
        GDM = matrix(GDM)
    }
    wmeans = wmeans[permR]

    if (is.list(U[[1]] || length(dim(U[[1]])) > 1)) {
        U[[1]] = generateCoPaM(U[[1]], relabel_technique = relabel_technique, w = w[[1]], X = X, distCriterion = distCriterion, K = K)
    }
    CoPaM = matrix(, nrow(U[[1]]), nrow(GDM))       
    CoPaM[, GDM[, 1]] = U[[1]]                      #U就是共识矩阵，U一共有k个row，代表类�?
    K = nrow(CoPaM)

    if (R == 1)
        return(CoPaM)

    for (r in 2:R) {
        if (is.list(U[[r]] || length(dim(U[[r]])) > 1)) {
            U[[r]] = generateCoPaM(U[[r]], relabel_technique = relabel_technique, w = w[[r]], X = X, distCriterion = distCriterion, K = K)
        }
        if (nrow(U[[r]]) != K) {
            stop("Inequal number of clusters in partitions.")
        }
        U[[r]] = relabelClusts(CoPaM[, GDM[, r]], U[[r]], technique = relabel_technique, X = X, distCriterion = distCriterion)$B    #第一次没有relabel，但是后面的都有relabel，很符合论文中写�?

        CoPaM[, GDM[, r]] = t(apply(CoPaM[, GDM[, r]], 1, "*", GDM[GDM[, r], 1:(r - 1)] %*% as.matrix(wmeans[1:(r - 1)])))  #计算共识矩阵?
        CoPaM[, GDM[, r]] = CoPaM[, GDM[, r]] + wmeans[r] * U[[r]]
        CoPaM[, GDM[, r]] = t(apply(CoPaM[, GDM[, r]], 1, "/", GDM[GDM[, r], 1:r] %*% as.matrix(wmeans[1:r])))
    }

    return(CoPaM)
}
