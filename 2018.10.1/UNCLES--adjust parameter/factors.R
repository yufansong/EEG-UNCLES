######################################################################
#函数�?:factors
#函数功能:Find the factors of a number. If (primesonly) was true, 
#        it gives prime factors only. Otherwise, it gives all factors
#参数:    n: 找到n的因�?
#返回�?:  r：返回所有的因子，或者只返回素数因子
#        该函数作用不大， 也只是在最后的画图中用到，和核心算法无�?
#编写者：宋宇�?
#时间�?2018/5/5
########################################################################


factors <- function(n, primeonly = FALSE) {
    nextPow2 <- function(n) {
        if (length(n) > 1) {
            r = n;
            for (i in 1:length(n)) {
                r[i] = nextPow2(n[i])
            }
        } else {
            r = 0
            while (n > 1) {
                r = r + 1
                n = n / 2
            }
        }
        return(r)
    }
    sievePrimes <- function(n) {
        if (length(n) > 1) {
            r = as.list(n);
            for (i in 1:length(n)) {
                r[[i]] = sievePrimes(n[i])
            }
            return(r)
        } else {
            n <- as.integer(n)
            if (n < 2)
                return(NULL)
            if (n == 2)
                return(2)
            if (n == 3)
                return(c(2, 3))
            if (n > 1e6)
                stop("n too large")
            primes <- rep(TRUE, n)
            primes[1] <- FALSE
            last.prime <- 2L
            for (i in last.prime:floor(sqrt(n))) {
                primes[seq.int(2L * last.prime, n, last.prime)] <- FALSE
                last.prime <- last.prime + min(which(primes[(last.prime + 1):n]))
            }
            return(which(primes))
        }
    }
    primefactors <- function(n) {
        if (length(n) > 1) {
            f = as.list(n)
            for (i in 1:length(n)) {
                f[[i]] = primefactors(n[i])
            }
            return(f)
        } else {
            if (n < 4) {
                return(n)
            }

            f = numeric()

            p = sievePrimes(floor(sqrt(n)))

            while (n > 1) {
                d = which(n %% p == 0)
                if (isempty(d)) {
                    return(sort(c(n, f)))
                }
                p = p[d]
                f = c(f, p)
                n = n / prod(p)
            }

            return(sort(f))
        }
    }
    getcondvects <- function(i) {
        g = 2
        i2 = 2 ^ i
        condvects = matrix(FALSE, i2, i)
        for (m in 1:i) {
            m2 = 2 ^ m
            m3 = (m2 / 2) - 1
            i3 = i - m + 1
            for (g in seq(g, i2, m2)) {
                for (k in 0:m3) {
                    condvects[g + k, i3] = TRUE
                }
            }
            g = m2 + 1
        }

        return(condvects)
    }

    if (n != floor(n)) {
        stop("n must be an integer")
    }

    f = primefactors(n)

    if (primeonly) {
        return(f)
    }

    allfactors = t(apply(getcondvects(length(f)), 1, "*", f))
    allfactors[allfactors == 0] = 1
    allfactors = unique(apply(allfactors, 1, prod))

    return(sort(allfactors))
}