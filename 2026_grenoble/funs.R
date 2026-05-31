verifier_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!suppressWarnings(require(pkg, character.only = TRUE))) {
      message("Installing ", pkg, "...")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      message("Loaded ", pkg)
    }
  }
}

to_sparse <- function (data, row, column, value, ...)
{
  row_col <- rlang::quo_name(rlang::enquo(row))
  column_col <- rlang::quo_name(rlang::enquo(column))
  value_col <- rlang::enquo(value)
  if (rlang::quo_is_missing(value_col)) {
      value_col <- 1
  }
  data <- ungroup(data)
  # data <- distinct(data, !!sym(row_col), !!sym(column_col),
  #     .keep_all = TRUE)
  row_names <- data[[row_col]]
  col_names <- data[[column_col]]
  if (is.numeric(value_col)) {
      values <- value_col
  }
  else {
      value_col <- rlang::quo_name(value_col)
      values <- data[[value_col]]
  }
  if (is.factor(row_names)) {
      row_u <- levels(row_names)
      i <- as.integer(row_names)
  }
  else {
      row_u <- unique(row_names)
      i <- match(row_names, row_u)
  }
  if (is.factor(col_names)) {
      col_u <- levels(col_names)
      j <- as.integer(col_names)
  }
  else {
      col_u <- unique(col_names)
      j <- match(col_names, col_u)
  }
  ret <- Matrix::sparseMatrix(
    i = i, j = j, x = values, dimnames = list(row_u, col_u), ...)
  ret
}

oai_chat <- function(
  msg, base_url, model_name, temperature = 0.7, api_key = NULL
) {
  body <- list(
    model = model_name,
    messages = list(
      list(role = "user", content = msg)
    ),
    temperature = temperature
  )
  
  # Build headers
  headers <- c("Content-Type" = "application/json")
  if (!is.null(api_key)) {
    headers <- c(headers, Authorization = paste("Bearer", api_key))
  }
  
  resp <- httr::POST(
    sprintf("%s/v1/chat/completions", base_url),
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  
  return(parsed$choices[[1]]$message$content)
}

oai_transcriptions <- function(
  file, 
  base_url = "https://api.openai.com", 
  model_name = "whisper-1", 
  language = "en", 
  api_key = NULL,
  timestamp_granularities = "word"
) {

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") stop("API key not provided and OPENAI_API_KEY env var is empty.")
  }

  url <- paste0(base_url, "/v1/audio/transcriptions")

  res <- httr::POST(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", api_key)
    ),
    body = list(
      file = httr::upload_file(file),
      model = model_name,
      response_format = "verbose_json",
      language = language,
      `timestamp_granularities[]` = timestamp_granularities
    ),
    encode = "multipart"
  )

  httr::stop_for_status(res)
  json_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  out <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE)

  return(out)
}

oai_embeddings <- function(
  input, 
  base_url = "https://api.openai.com", 
  model_name = "text-embedding-3-large", 
  api_key = NULL
) {

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") stop("API key not provided and OPENAI_API_KEY env var is empty.")
  }
  url <- paste0(base_url, "/v1/embeddings")

  res <- httr::POST(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(list(
      model = model_name,
      input = input
    ), auto_unbox = TRUE)
  )

  httr::stop_for_status(res)

  json_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  embedding_result <- jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
  out <- unlist(embedding_result$data[[1]]$embedding)
  return(out)
}

alignement_des_textes <- function(df1, df2, col1, col2, suffix_y = "_y") {
  col1_sym <- rlang::ensym(col1)
  col2_sym <- rlang::ensym(col2)
  col1_name <- rlang::as_string(col1_sym)
  col2_name <- rlang::as_string(col2_sym)
  
  seq1 <- dplyr::pull(df1, rlang::as_string(col1_sym))
  seq2 <- dplyr::pull(df2, rlang::as_string(col2_sym))
  
  all_vals <- unique(c(seq1, seq2))
  seq1_num <- match(seq1, all_vals)
  seq2_num <- match(seq2, all_vals)
  
  dist_mat <- outer(seq1_num, seq2_num, FUN = function(x, y) as.numeric(x != y))
  
  alignment <- dtw::dtw(
    dist_mat,
    step.pattern = dtw::symmetric2,
    keep.internals = TRUE
  )
  
  idx1 <- alignment$index1
  idx2 <- alignment$index2
  
  df1_aligned <- df1[idx1, , drop = FALSE]
  df2_aligned <- df2[idx2, , drop = FALSE]
  
  df2_aligned <- df2_aligned |>
    dplyr::rename_with(~ paste0(.x, suffix_y))
  
  combined <- tibble::tibble(
    id   = idx1,
    id_y = idx2
  ) |>
    dplyr::bind_cols(df1_aligned, df2_aligned)
  
  combined |>
    dplyr::filter(
      .data[[col1_name]] ==
        .data[[paste0(col2_name, suffix_y)]]
    )
}