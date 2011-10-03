.ghostpermute = function(n, minlvl = 1, maxlvl = n) {
# Author: Guillaume Filion.
# Date: April 12 2011.
# Generates a random permutation by hierarchic
# swapping.

# In the heuristic example below, n=4, minlvl=2, maxlvl=2.

    current <- seq(from=1, to=2^(n-maxlvl), by=1)
    # Example: current = (1,2,3,4)

    i <- maxlvl
    while (i >= minlvl) {
        current <- 2 * rep(current-1, each=2)
        # Example: current = (0,0,2,2,4,4,6,6)
        random <- runif(2^(n-i), min=0, max=1) > 0.5
        # Example: random = (1,1,0,1)
        zeroone <- as.vector(rbind(random, 1-random))
        # Example: zeroone = (1,0,1,0,0,1,1,0)
        current <- 1 + current + zeroone
        # Example: current = (2,1,4,3,5,6,8,7)

        i <- i-1
    }

    if (i > 1) {
        # Now i = minlvl - 1.
        current <- 2^i * rep(current-1, each=2^i)
        # Example: current = (2,2,0,0,6,6,4,4,8,8,10,10,14,14,12,12)
        current <- 1 + current + rep(0:(2^i-1), times=2^(n-i))
        # Example: current = (3,4,1,2,7,8,5,6,9,10,11,12,15,16,13,14)
    }

    return(current)

}
