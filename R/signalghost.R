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
        ghost  <- .ghostpermute(ceiling(log2(n)), ...);
        if (circular) {
            m <- sample.int(n=length(ghost), size=1);
            ghost <- c(ghost[m:length(ghost)], ghost)[1:length(ghost)];
        }
        if (is.vector(x)) {
            x[indices] <- x[indices[ghost[ghost <= n]]];
        }
        else {
            x[indices,] <- x[indices[ghost[ghost <= n]],];
        }
    }

    return (x);

}
