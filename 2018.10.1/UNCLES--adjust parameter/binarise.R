######################################################################
#函数�?:binarise
#函数功能:二值化操作
#参数:
#   U：传入要进行操作的矩�?/向量
#   K：要分的类的数目
#   technique：二值化操作的几个选择:（一下的几种二值化方法，刘超学长已经给出具体的公式，不过看代码也能看出来）
#               1 UB/union binarization(default)
#               2 IB/intersection binarization
#               3 MVB/maximum value Binarization  
#               4 VTB/value thresholding binarization
#               5 std thresh??? (TB/top binarization?)
#               6 DTB/difference Thresholding binarization
#   parameter: 阈值，二值化的阈�?
#返回�?:    
#   一个bool类型的，维度和输入的U相同的变�?
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################
binarise <- function(U, K, technique = "DTB", parameter = 0.5) {
    technique = tolower(technique)
    if (technique == "union" || technique == "ub") {
        return(U > 0)
    } else if (technique == "intersection" || technique == "ib") {
        return(U == 1)
    } else if (technique == "max" || technique == "mvb") {
        return(U == matrix(1, K, 1) %*% apply(U, 2, max))
    } else if (technique == "valuethresh" || technique == "value" || technique == "vtb") {
        return(U >= parameter)
    } else if (technique == "stdthresh" || technique == "std") {
        return((matrix(1, K, 1) %*% apply(U, 2, sd) >= parameter) & (U == matrix(1, K, 1) %*% apply(U, 2, max)))
    } else if (technique == "difference" || technique == "dtb") {
        if (is.vector(U) || nrow(U) == 1) {
            diff = rep(1, length(U))
        } else {
            Us = apply(U, 2, sort)
            diff = Us[nrow(Us),] - Us[nrow(Us) - 1,]
        }
        return(((matrix(1, K, 1) %*% diff) >= parameter) & (U == matrix(1, K, 1) %*% apply(U, 2, max)))
    } else if (technique == "top" || technique == "tb") {
        return(U >= (matrix(1, K, 1) %*% apply(U, 2, max) - parameter))
    } else {
        stop("Unknown binarisation technique")
    }
}
