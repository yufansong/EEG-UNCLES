######################################################################
#函数�?:clusterDataset
#函数功能:Cluster a single dataset
#参数:
#   X:dataset
#   K:聚类数目
#   D:函数中没用过，不知道有什么用，在uncle函数中传入的是K的因�?
#   methods:传入的方法变量，list类型
#返回�?:    
#   U：最后的返回指，要用到clustVec2partMat函数，将运算过程中产生的结果向量转变成矩�?
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################


clusterDataset <- function(X, K, D = 0, methods = list(kmeansKA)) {
    if (!is.list(methods)) {
        methods = list(methods)
    }

    U = list()

    for (ms in 1:length(methods)) {
        if (!is.list(methods[[ms]])) {
            methods[[ms]] = list(methods[[ms]])
        }
        # Get the method's function
        meth = methods[[ms]][[1]]

        # Get the name of the output variable if provided
        if (is.null(methods[[ms]]$outputVariable)) {    #outputVariable?????
            outputVariable = NULL
        } else {
            outputVariable = methods[[ms]]$outputVariable
            methods[[ms]]$outputVariable <- NULL
        }

        # Get the parameters and run the method
        if (length(meth) > 1) {
            params = methods[[ms]][2:length(methods[[ms]])]
            res = meth(X, K, params)
        } else {
            params = NULL
            res = meth(X, K)
        }

        # Get the output variable from the result if relevant
        if (!is.null(outputVariable)) {
            res = res[[outputVariable]]
        }

        # Convert the result to a partition matrix if not already so
        if (is.vector(res) && max(res) > 1) {
            res = clustVec2partMat(res)
        }

        # Store the result
        U[[ms]] = res
    }

    return(U)
}