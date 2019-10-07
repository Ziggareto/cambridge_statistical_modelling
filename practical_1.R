# 1.
# Z ~ N(0, 1). Want E(Z| Z >=1) and E(Z^6)
z_vec <- rnorm(10000)
mean(z_vec[z_vec >= 1])
mean(z_vec^6)

# 2.
# upper 5% point of a chi squared 6 distribution
qchisq(0.95, 6)

# 3.
# Solve the eqation M(a, b, c, d) = c(9, 13, 11, 27) 
# i.e. Mx = b
M <- matrix(c(3, 2, 6, 6, 4, -1, 2, 6, -2, 7, -1, -2, 1, -2, 1, 5), 4, 4)
b <- c(9, 13, 11, 27)
solve(M, b)