######################################################################
#å½æ°å?:closestToSquareFactors
#å½æ°åè½:
#   Finds the two factors of (n), closest to its square root
#   ä¸»è¦æ¯ä¸ºäºå¨plotå½æ°ä¸­æ§å¶å¾åçè¡åå¼ï¼ä¸æ¶åå°æ ¸å¿ç®æ³ï¼åªæ¯ä¸ºäºæçï¼ç¨å¤ä¸å¤§
#åæ°: n å¾åçæ»æ°ç?
#è¿åå?: sf äºåç»ï¼æ¯è¡åå¼ï¼äºèç¸ä¹ç­äºn
#   
#ç¼åèï¼å®å®å?
#æ¶é´ï¼?2018/5/5
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