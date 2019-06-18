######################################################################
#函数�?:relabelClusts
#函数功能:(description in the paper)
#            because clustering is unsupervised, there are no labels 
#        for the clusters in the diﬀerent partitions, i.e. the ith cluster
#        in one partition is not guaranteed to match the ith cluster in another partition. 
#        Relabeling reorders, the clusters in the partitions so that they become aligned.
#        Min–minapproachwasused to perform relabeling. 
#
#参数:
#       ref:???
#       input:???
#       technique:  有几种选择，但是并不是你输入什么就做什么，还会根据你矩阵的Kin参数进行判断
#           brute / minmin / minmax / minmin_strict / minmax_strict
#           -"brute": Brute force relabelling. This is not practical for K > 8. 
#           -"minmin_strict": minmin relabelling 
#           -"minmax_strict": minmax relabelling 
#           -"minmin" (DEFAULT): if (K > 8), minmin relabelling is applied, 
#                                otherwise brute force is applied. 
#           - "minmax": if (K > 8), minmax relabelling is applied, 
#                       otherwise brute force is applied.
#       X:???
#       distCriterion: 
#            传入clusDist函数中用于计算距离的类型：direct_euc     centers_euc    union_std
#返回�?:    
#       ind:???
#       Perm:???
#       B:???
#编写者：宋宇�?
#时间�?2018/5/6
########################################################################

relabelClusts <- function(ref, input, technique = 'minmin', X = numeric(), distCriterion = 'direct_euc') {
    relabel_minmax <- function(ref, B, X, distCriterion) {
        Kref = nrow(ref)
        Kin = nrow(B)

        Perm = rep(0, Kin)

        # Kref x Kin distance matrix (D)
        D = clustDist(ref, B, X, distCriterion)

        # (M1) Greater than the global maximum and (M2) lower than the global minimum
        M1 = rep(1, Kin) * max(D) + 1
        M2 = rep(-1, Kref)

        for (ii in 1:min(Kref, Kin)) {
            mi = apply(D, 2, min)
            ma = max(mi)

            col = which(mi == ma)[1]
            row = which(D[, col] == ma)[1]
            Perm[row] = col
            D[row,] = M1
            D[, col] = M2
        }

        if (Kin > Kref) {
            Perm[Perm == 0] = setdiff(1:Kin, Perm)
        }

        return(Perm)
    }

    relabel_minmin <- function(ref, input, X, distCriterion) {
        Kref = nrow(ref)
        Kin = nrow(B)

        Perm = rep(0, Kin)

        # Kref x Kin distance matrix (D)
        D = clustDist(ref, B, X, distCriterion)

        # (M1) Greater than the global maximum and (M2) lower than the global minimum
        M1 = rep(1, Kin) * max(D) + 1
        M2 = rep(1, Kref) * max(D) + 1

        for (ii in 1:min(Kref, Kin)) {
            mi = apply(D, 2, min)
            ma = min(mi)

            col = which(mi == ma)[1]
            row = which(D[, col] == ma)[1]
            Perm[row] = col
            D[row,] = M1
            D[, col] = M2
        }

        if (Kin > Kref) {
            Perm[Perm == 0] = setdiff(1:Kin, Perm)
        }

        return(Perm)
    }

    relabel_brute <- function(ref, input, X, distCriterion) {
        Kref = nrow(ref)
        Kin = nrow(B)

        # Kref x Kin distance matrix (D)
        D = clustDist(ref, B, X, distCriterion)

        Perms = permutations(Kin);
        PermsSums = vector(, factorial(Kin));

        for (ll in 1:factorial(Kin)) {
            for (ii in 1:min(Kin, Kref)) {
                PermsSums[ll] = PermsSums[ll] + D[ii, Perms[ll, ii]]
            }
        }

        best = which(PermsSums == min(PermsSums))[1]
        return(Perms[best,])
    }

    if (is.vector(ref)) {
        ref = clustVec2partMat(ref)
    } else {
        Kref = nrow(ref)
        M = ncol(ref)
        if (Kref > M) {
            ref = t(ref)
        }
    }

    if (is.vector(input)) {
        Ind = input
        B = clustVec2partMat(Ind)
        Kin = nrow(B)
    } else {
        B = input
        Kin = nrow(input)
        M = ncol(input)
        if (Kin > M) {
            B = t(B)
            Kin = M
        }
    }

    if (technique == 'brute') {
        Perm = relabel_brute(ref, B, X, distCriterion)
    } else if (technique == 'minmin') {
        if (Kin < 8) {
            Perm = relabel_brute(ref, B, X, distCriterion)
        } else {
            Perm = relabel_minmin(ref, B, X, distCriterion)
        }
    } else if (technique == 'minmax') {
        if (Kin < 8) {
            Perm = relabel_brute(ref, B, X, distCriterion)
        } else {
            Perm = relabel_minmax(ref, B, X, distCriterion)
        }
    } else if (technique == 'minmin_strict') {
        Perm = relabel_minmin(ref, B, X, distCriterion)
    } else if (technique == 'minmax_strict') {
        Perm = relabel_minmax(ref, B, X, distCriterion)
    } else {
        stop("Unknown relabelling technique")
    }

    B = B[Perm,]
    if (isValidBPM(B)) {
        Ind = partMat2clustVec(B)
    } else {
        Ind = "Not Binary Partition Matrix"
    }

    return(list(Ind=Ind, B=B, Perm=Perm))
}
