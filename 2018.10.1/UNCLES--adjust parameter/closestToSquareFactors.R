######################################################################
#函数�?:closestToSquareFactors
#函数功能:
#   Finds the two factors of (n), closest to its square root
#   主要是为了在plot函数中控制图像的行列值，不涉及到核心算法，只是为了排版，用处不大
#参数: n 图像的总数�?
#返回�?: sf 二元组，是行列值，二者相乘等于n
#   
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################

closestToSquareFactors <- function(n) {
    f = factors(n)
    d = abs(f - sqrt(n))
    j = which(d == min(d))
    sf = c(0, 0);

    if (f[j] < sqrt(n)) {
        sf[1] = f[j]
        sf[2] = n / sf[1]
    } else {
        sf[2] = f[j]
        sf[1] = n / sf[2]
    }

    return(sf)
}