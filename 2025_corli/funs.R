require("dtw")
require("dplyr")
require("tibble")
require("rlang")

alignement_des_textes <- function(df1, df2, col1, col2, suffix_y = "_y") {
  # Allow bare names or strings
  col1_sym <- ensym(col1)
  col2_sym <- ensym(col2)
  col1_name <- as_string(col1_sym)
  col2_name <- as_string(col2_sym)
  
  # Extract the sequences to align
  seq1 <- dplyr::pull(df1, !!col1_sym)
  seq2 <- dplyr::pull(df2, !!col2_sym)
  
  # Build joint vocabulary and numeric encoding
  all_vals <- unique(c(seq1, seq2))
  seq1_num <- match(seq1, all_vals)
  seq2_num <- match(seq2, all_vals)
  
  # Distance matrix: 0 for match, 1 for mismatch
  dist_mat <- outer(seq1_num, seq2_num, FUN = function(x, y) as.numeric(x != y))
  
  # DTW alignment
  alignment <- dtw::dtw(
    dist_mat,
    step.pattern = dtw::symmetric2,
    keep.internals = TRUE
  )
  
  idx1 <- alignment$index1
  idx2 <- alignment$index2
  
  # Subset rows in alignment order
  df1_aligned <- df1[idx1, , drop = FALSE]
  df2_aligned <- df2[idx2, , drop = FALSE]
  
  # Rename all columns of df2 with suffix_y
  df2_aligned <- df2_aligned |>
    rename_with(~ paste0(.x, suffix_y))
  
  # Combine, including index columns
  combined <- tibble(
    id   = idx1,
    id_y = idx2
  ) |>
    bind_cols(df1_aligned, df2_aligned)
  
  # Keep only rows where the aligning columns are equal
  combined |>
    filter(
      .data[[col1_name]] ==
        .data[[paste0(col2_name, suffix_y)]]
    )
}