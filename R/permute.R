#' Permutations of n objects
#'
#' Generate a full list of permutations of n objects
#' @param x A vector containing n items (numeric/alphanumeric)
#' @return Array of permuted objects, one permutation per line
#' @examples
#' p1 <- permute(1:4)
#' p2 <- permute(letters[1:5])
#' @export
permute <- function(x) {
    if (length(x) == 1) {
        return(x)
    } else {
        res <- matrix(nrow = 0, ncol = length(x))
        for (i in seq_along(x)) {
            res <- rbind(res, cbind(x[i], Recall(x[-i])))
        }
        return(res)
    }
}
