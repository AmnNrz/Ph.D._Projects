# Define the matrix A
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2, byrow=TRUE)

# Calculate A transpose
At <- t(A)

# Compute the Moore-Penrose pseudoinverse of At * A
AtA_inv <- solve(At %*% A)

# Calculate the projection matrix P
P <- A %*% AtA_inv %*% At

# Print the projection matrix P
print(P)








# Define the matrix A
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2, byrow=TRUE)

# Compute the SVD of A
svd_result <- svd(A)

# U matrix (left singular vectors)
U <- svd_result$u

# Sigma (diagonal matrix of singular values)
Sigma <- svd_result$d

# Compute the rank of A based on non-zero singular values
r <- sum(Sigma > 1e-10)  # Threshold to avoid numerical issues

# Compute the projection matrix P using the first r columns of U
P <- U[, 1:r] %*% t(U[, 1:r])

# Print the projection matrix P
print(P)

