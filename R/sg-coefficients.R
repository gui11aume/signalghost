.ghostweightsmatrix <- function(n) {

    # R. is the matrix R_{2^n-1} from definition 3.
    R. <- matrix(0, ncol=2^n-1, nrow=2^n-1);

    # Define the convenience function phi_m(.) (see formula 4).
    phi <- function(m) {
        # Return a vector of length 2^n-1 where element l
        # is the value of phi_m(l). So phi(2^k) returns
        # all values for phi_{2^k}(l).
        return (c(1:m, (m-1):0, rep(0, 2^n))[1:2^n-1]);
    }

    for (L in 1:(2^n-1)) {
       for (k in 0:(n-1)) {
           R.[L,] <- R.[L,] + phi(2^k)[L] * phi(2^k) / 8^k;
       }
       R.[L,] <- R.[L,] * 2^(n-1) / (2^n - L)
    }

    # R is the matrix R_{2^n} from definition 3.
    R <- matrix(0, nrow=2^n, ncol=2^n);
    R[2:2^n,2:2^n] <- R.;
    R[1,1] <- 1;

    return(R);

}
