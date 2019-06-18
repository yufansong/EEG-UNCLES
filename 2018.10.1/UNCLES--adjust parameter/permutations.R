######################################################################
#函数�?:permutations
#函数功能:Finds all permutations of numbers 1 to n（找到全排列�?
#参数：n
#返回�?:A（矩阵，每一行代表一个全排列�?    
#编写者：宋宇�?
#时间�?2018/5/6
########################################################################

permutations <- function(n) {
    if (n == 1) {
        return(matrix(1))
    } else {
        sp <- permutations(n - 1)
        p <- nrow(sp)
        A <- matrix(nrow = n * p, ncol = n)
        for (i in 1:n) {
            A[(i - 1) * p + 1:p,] <- cbind(i, sp + (sp >= i))
        }
        return(A)
    }
}