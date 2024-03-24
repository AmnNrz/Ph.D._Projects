library(MASS)

# Assuming A and B are your matrices
A <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2)
B <- A %*% matrix(c(1,2,3,3), nrow=2, ncol=2) # Example P matrix for demonstration

# Transpose A and B
A_t <- t(A)
B_t <- t(B)

# Solve for X using the pseudo-inverse of A_t
A_pinv <- ginv(A_t) # Using ginv() from the MASS package for pseudo-inverse
X_solution <- A_pinv %*% B_t

# Check the solution
X_solution
