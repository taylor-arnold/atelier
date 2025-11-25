input  <- "atelier_exercices_solutions.Rmd"
output <- "atelier_exercices.Rmd"

lines <- readLines(input)

out <- c()
i <- 1
n <- length(lines)

while (i <= n) {
  
  # Detect start of R code block
  if (grepl("^```\\{r", lines[i])) {
    block_start <- lines[i]
    i <- i + 1
    block_lines <- c()
    
    # Collect until end of block
    while (i <= n && !grepl("^```$", lines[i])) {
      block_lines <- c(block_lines, lines[i])
      i <- i + 1
    }
    
    # Now lines[i] should be ``` (end of block)
    block_end <- lines[i]
    
    # Process block
    if (length(block_lines) > 0 && grepl("^# *keep", block_lines[1])) {
      # Keep: drop the first line (# keep)
      block_lines <- block_lines[-1]
      if (length(block_lines) == 0) block_lines <- ""  # avoid empty block
    } else {
      # No keep: replace with a single empty line
      block_lines <- ""
    }
    
    # Append processed block to output
    out <- c(out, block_start, block_lines, block_end)
    
  } else {
    # Non-code lines are copied as-is
    out <- c(out, lines[i])
  }
  
  i <- i + 1
}

writeLines(out, output)
