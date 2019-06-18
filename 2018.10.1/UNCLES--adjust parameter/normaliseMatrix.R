normaliseMatrix <- function(X, type) {
    # Nested functions


    if (length(type) > 1) {
        for (i in type) {
            X = normaliseMatrix(X, i)
        }
        return(X)
    }

    if (type == 0) {
        #1: Do nothing
        X = X
    } else if (type == 1) {
        #1: Divide by the mean
        X = X / rowMeans(X)
    } else if (type == 2) {
        #2: Divide by the first value
        X = X / X[, 1]
    } else if (type == 3) {
        #3: Take log2
        X[X <= 0] = NaN
        X = log2(X)
        ind1 = which(is.na(rowSums(X)))
        X[ind1,] = fixnans(X[ind1,])
    } else if (type == 4) {
        #4: Subtract the mean and divide by the std
        X = X - rowMeans(X);
        ConstGenesIndices = apply(X, 1, sd) == 0
        X = X / apply(X, 1, sd);
        X[ConstGenesIndices,] = 0
    } else if (type == 5) {
        #5: Divide by the sum
        X = X / rowSums(X)
    } else if (type == 6) {
        #6: Subtract the mean
        X = X - rowMeans(X)
    } else if (type == 7) {
        #7: Divide by the maximum
        X = X / apply(X, 1, max)
    } else if (type == 8) {
        #8: (2 to the power X)
        X = 2 ^ X
    } else if (type == 9) {
        #9: Subtract the min
        X = X - apply(X, 1, min)
    } else if (type == 10) {
        #10: Rank: 1 for lowest, then 2, 3, ...; average on ties
        rfunc <- function(x) rank(x, ties.method = "average")
        X = t(apply(X, 1, rfunc))
    } else if (type == 11) {
        #11: Rank: 1 for lowest, then 2, 3, ...; arbitrary order on ties
        rfunc <- function(x) rank(x, ties.method = "random")
        X = t(apply(X, 1, rfunc))
    } else if (type == 12) {
        #12: Normalise to the [0 1] range
        X = X - apply(X, 1, min)
        X = X / apply(X, 1, max)
    } else if (type == 101) {
        #101: quantile
        X_rank <- apply(X, 2, rank, ties.method = "min")
        X_sorted <- apply(X, 2, sort)
        X_mean <- apply(X_sorted, 1, mean)

        index_to_mean <- function(my_index, my_mean) {
            return(my_mean[my_index])
        }

        X <- apply(X_rank, 2, index_to_mean, my_mean = X_mean)
    } else {
        stop("Unsupported type");
    }

    return(X);
}
