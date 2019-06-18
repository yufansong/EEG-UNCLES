######################################################################
#函数�?:isValidBPM
#函数功能: Checking if a matrix is a valid binary partition matrix (BPM)
#参数: U
#返回�?: True/False    
#编写者：宋宇�?
#时间�?2018/5/6
########################################################################

isValidBPM <- function(U) {
    s1 = colSums(U)
    s2 = rowSums(U)

    if (any(U != 1 && U != 0)) {        #所有的值都必须�?0/1
        return(FALSE)
    }

    if (max(s1) != 1 || max(s2) != 1) { #每一行列中只能有一个True
        return(FALSE)
    }

    if (min(s2) <= 0) {                 #每一行中必须有一个True（为什么列不做要求???�?
        return(FALSE)
    }

    return(TRUE)
}