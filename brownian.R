# Brownian motion
m = 1000    # number of time steps
n = 5       # number of paths

Z = matrix(rnorm(n*m), nrow=n, ncol=m) # need n*m std. normals
dt = 1/m # going from 0 to 1 in time steps of dt = 1/m
dW = sqrt(dt) * Z  # small increments of Brownian motion

# compute a running sum of the increments along the rows
W = t(apply(dW, 1, cumsum))    # simulated paths
  # note: the apply() function transposes the matrix
  # from n x m to m x n, so we use t() to 
  # transpose it back to n x m

ti = matrix(rep(seq(0,1,length=m), each=n), 
            nrow=n, ncol=m) # time index (x axis)

matplot(t(ti), t(W), type="l", main="Brownian Motion",
        xlab="t", ylab="W(t)")
  # note: matplot plots each *column* as a series
  # transpose the matrices to m x n