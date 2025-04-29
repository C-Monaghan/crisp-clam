normalise <- function(x) {
  # Normalizes a matrix so that each row sums to 1.
  # Arguments:
  #   - x: The input matrix to normalize.
  # Returns:
  #   - A normalized matrix where each row sums to 1.
  x / rowSums(x)
}
