######################################################################
#函数�?:fuzzystretch
#函数功能:把传入的矩阵进行一个拉升的操作，给定一个阈值x0�?0-x0的值减少，x0�?1的值增�?
#        可以给定x0，但是文档中建议缺省，如过缺省，那么使用每列的均�?
#参数:  
#   X：需要处理的数据
#   x0：阈�?
#   拉伸方法�?  x                       y
#      x <x0    (pi*x)/(2*x0)-pi/2      x0+x0*sin(x) 
#      x>=x0    (x-x0)*pi/(2-(1-x0))    x0+(1-x0)*sin(x)
#返回�?:    
#   y:处理结果
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################

fuzzystretch <- function(X, x0 =0.5) {
    M = nrow(X)
    N = ncol(X)
    if (x0 == -1) {
        x0 = rep(0, M)
        for (i in 1:M) {
            xrow = X[i,]
            x0[i] = mean(xrow[xrow > 0])
        }
        x0[x0 == 1] = 0.5
    } else {
        if (length(x0) == 1) {
            x0 = rep(x0, M)
        } else if (length(x0) != M) {
            stop("x0 must be a single value or a vector with elements equal in number to the number of rows of X")
        }
    }

    y = matrix(, M, N)
    for (i in 1:M) {
        xrow = X[i,]
        xt = xrow
        xt[xrow < x0[i]] = (pi * xrow[xrow < x0[i]]) / (2 * x0[i]) - pi / 2
        xt[xrow >= x0[i]] = (xrow[xrow >= x0[i]] - x0[i]) * pi / (2 * (1 - x0[i]))

        yt = rep(0, N)
        yt[xrow < x0[i]] = x0[i] + x0[i] * sin(xt[xrow < x0[i]])
        yt[xrow >= x0[i]] = x0[i] + (1 - x0[i]) * sin(xt[xrow >= x0[i]])

        y[i,] = yt
    }

    return(y)
}