signalghost <- function(x, blocks, circular=FALSE, ...) {
# Author: Guillaume Filion.
# Date: August 1, 2011.

    if (!is.vector(x) && !is.matrix(x)) {
        stop ("x has to be a vector or a matrix");
    }

    if (is.vector(x)) {
        size <- length;
    }
    else {
        size <- nrow;
    }

    if (missing(blocks)) {
        blocks <- rep(0, size(x));
    }

    if (length(blocks) != size(x)) {
        stop ("x and blocks must have the same size");
    }

    for (block in unique(blocks)) {
        indices <- which(blocks == block);
        n <- length(indices);
        N <- ceiling(log2(n))
        ghost  <- .ghostpermute(N, ...);
        if (circular) {
            m <- sample.int(n=length(ghost), size=1);
            ghost <- c(ghost[m:length(ghost)], ghost)[1:length(ghost)];
        }
        zero <- sample(1:2^N, size=2^N-n, replace=FALSE)
        one <- rep(1, 2^N)
        one[zero] <- 0
        i <- cumsum(one)
        i[zero] <- NA
        i <- i[ghost]
        i <- i[!is.na(i)]
        if (is.vector(x)) {
            x[indices] <- x[indices[i]];
        }
        else {
            x[indices,] <- x[indices[i],];
        }
    }

    return (x);

}
