# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])

  # Drop count column
  x[[countcol]] <- NULL

  # Get the rows from x
  x[idx, ]
}


longify_xtab <- function(x) {
  nm <- names(x)
  # Convert to table
  x_tab <- as.table(as.matrix(x))
  # Just in case there are now rownames, required for conversion
  rownames(x_tab) <- nm
  # Use appropriate method to get a df
  x_df <- as.data.frame(x_tab)

  # Restructure df in a painful and unsightly way
  data.frame(lapply(x_df[seq_len(ncol(x_df) - 1)], function(col) {
    rep(col, x_df$Freq)
  }))
}

# m
# a  b  c  d  e  f
# A 11 16 13 10 16 17
# B  7 19  9 13 12 12
# C 14 21 25 12 35  24
# class(m)

# [1] "matrix" "array"

# nm <- names(m)
# x_tab <- as.table(as.matrix(m))
# x_tab
# rownames(x_tab) <- nm
# x_tab
# x_df <- as.data.frame(x_tab)
# x_df
# contTables(x_df, rows=Var1, cols=Var2, counts=Freq)
# countsToCases(x_df)
