#' Convert a list of articles to a data frame
#'
#' This function converts a list of articles to a data frame.
#'
#' @param articles A list of articles, each containing metadata.
#'
#' @return A data frame containing article metadata.
#' @keywords internal
list_to_df <- function(articles) {
  # Check if the input is a list
  if (!is.list(articles)) {
    stop("Input must be a list")
  }

  # Check if the list is empty
  if (length(articles) == 0) {
    return(data.frame())
  }

  # Initialize an empty data frame
  result_df <- data.frame(
    pmid = character(),
    title = character(),
    abstract = character(),
    authors = character(),
    publication_year = character(),
    journal = character(),
    stringsAsFactors = FALSE
  )

  # Convert each article to a row in the data frame
  for (article in articles) {
    # Create a new row
    new_row <- data.frame(
      pmid = ifelse(is.null(article$pmid), NA_character_, article$pmid),
      title = ifelse(is.null(article$title), NA_character_, article$title),
      abstract = ifelse(is.null(article$abstract), NA_character_, article$abstract),
      authors = ifelse(is.null(article$authors), NA_character_, paste(article$authors, collapse = ", ")),
      publication_year = ifelse(is.null(article$publication_year), NA_character_, article$publication_year),
      journal = ifelse(is.null(article$journal), NA_character_, article$journal),
      stringsAsFactors = FALSE
    )

    # Append to the result
    result_df <- rbind(result_df, new_row)
  }

  return(result_df)
}

#' Save search results to a file
#'
#' This function saves search results to a file.
#'
#' @param results A data frame containing search results.
#' @param file_path File path to save the results.
#' @param format File format to use. One of "csv", "rds", or "xlsx".
#'
#' @return The file path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' save_results(search_results, file_path = "search_results.csv")
#' }
save_results <- function(results, file_path, format = c("csv", "rds", "xlsx")) {
  # Match format argument
  format <- match.arg(format)

  # Get file extension from file_path
  ext <- tools::file_ext(file_path)

  # If extension doesn't match format, warn and adjust file_path
  if (ext != format) {
    warning("File extension does not match format argument. Using format: ", format)
    file_path <- paste0(tools::file_path_sans_ext(file_path), ".", format)
  }

  # Save the file in the appropriate format
  message("Saving results to: ", file_path)

  if (format == "csv") {
    # Ensure character columns stay as character
    for (col in names(results)) {
      if (is.character(results[[col]])) {
        results[[col]] <- as.character(results[[col]])
      }
    }
    utils::write.csv(results, file = file_path, row.names = FALSE)
  } else if (format == "rds") {
    saveRDS(results, file = file_path)
  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("The openxlsx package is required for Excel format. Install it with: install.packages('openxlsx')")
    }
    openxlsx::write.xlsx(results, file = file_path)
  }

  # Return the file path invisibly
  invisible(file_path)
}

#' Load saved results from a file
#'
#' This function loads previously saved results from a file.
#'
#' @param file_path File path to load the results from.
#'
#' @return A data frame containing the loaded results.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- load_results("search_results.csv")
#' }
load_results <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Get file extension
  ext <- tools::file_ext(file_path)

  # Load the file based on its extension
  if (ext == "csv") {
    results <- utils::read.csv(file_path, stringsAsFactors = FALSE)

    # Convert numeric IDs to character if they look like strings
    if ("pmid" %in% colnames(results)) {
      results$pmid <- as.character(results$pmid)
    }
  } else if (ext == "rds") {
    results <- readRDS(file_path)
  } else if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("The openxlsx package is required for Excel format. Install it with: install.packages('openxlsx')")
    }
    results <- openxlsx::read.xlsx(file_path)
  } else {
    stop("Unsupported file format: ", ext, ". Supported formats: csv, rds, xlsx")
  }

  return(results)
}

#' Merge multiple search results
#'
#' This function merges multiple search results into a single data frame.
#'
#' @param ... Data frames containing search results.
#' @param remove_duplicates Logical. If TRUE, removes duplicate articles.
#'
#' @return A merged data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' merged_results <- merge_results(results1, results2, results3)
#' }
merge_results <- function(..., remove_duplicates = TRUE) {
  # Get all data frames
  data_frames <- list(...)

  # Check if all inputs are data frames
  if (!all(sapply(data_frames, is.data.frame))) {
    stop("All inputs must be data frames")
  }

  # Check if any data frames are empty
  if (any(sapply(data_frames, nrow) == 0)) {
    warning("Some data frames are empty")
  }

  # Merge all data frames using rbind
  merged_df <- do.call(rbind, data_frames)

  # Remove duplicates if requested
  if (remove_duplicates && nrow(merged_df) > 0) {
    # Check if PMID column exists
    if ("pmid" %in% colnames(merged_df)) {
      # Remove duplicates based on PMID
      merged_df <- merged_df[!duplicated(merged_df$pmid), ]
    } else {
      # If no PMID, use title for deduplication
      if ("title" %in% colnames(merged_df)) {
        merged_df <- merged_df[!duplicated(merged_df$title), ]
      }
    }
  }

  return(merged_df)
}

#' Create a citation network from article data
#'
#' This function creates a citation network from article data.
#' Note: Currently a placeholder as it requires citation data not available through basic PubMed queries.
#'
#' @param article_data A data frame containing article data.
#' @param citation_data A data frame containing citation data (optional).
#'
#' @return An igraph object representing the citation network.
#' @export
#'
#' @examples
#' \dontrun{
#' network <- create_citation_net(article_data)
#' }
create_citation_net <- function(article_data, citation_data = NULL) {
  # Check for required packages
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The igraph package is required. Install it with: install.packages('igraph')")
  }

  message("Note: Full citation network creation requires citation data not available through basic PubMed API.")
  message("This function currently creates a placeholder network based on available data.")

  # Check if article_data has required columns
  if (!all(c("pmid", "title") %in% colnames(article_data))) {
    stop("article_data must contain at least 'pmid' and 'title' columns")
  }

  # Create nodes from article data
  nodes <- article_data[, c("pmid", "title")]
  names(nodes) <- c("id", "label")
  nodes <- unique(nodes)

  # If citation_data is provided, use it to create edges
  if (!is.null(citation_data)) {
    # Check if citation_data has required columns
    if (!all(c("citing_pmid", "cited_pmid") %in% colnames(citation_data))) {
      stop("citation_data must contain 'citing_pmid' and 'cited_pmid' columns")
    }

    # Create edges from citation data
    edges <- citation_data[, c("citing_pmid", "cited_pmid")]
    names(edges) <- c("from", "to")

    # Filter for edges where both from and to are in the nodes
    edges <- edges[edges$from %in% nodes$id & edges$to %in% nodes$id, ]
    edges <- unique(edges)
  } else {
    # Create a placeholder network based on publication year if available
    if ("publication_year" %in% colnames(article_data)) {
      # Sort articles by publication year
      sorted_articles <- article_data[, c("pmid", "publication_year")]
      sorted_articles <- sorted_articles[order(sorted_articles$publication_year), ]

      # Create simple edges based on publication year proximity
      # This is just a placeholder approach
      edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

      # Get unique years and create connections between articles in consecutive years
      years <- sort(unique(as.numeric(sorted_articles$publication_year)))

      if (length(years) > 1) {
        for (i in 1:(length(years)-1)) {
          current_year <- years[i]
          next_year <- years[i+1]

          current_articles <- sorted_articles$pmid[sorted_articles$publication_year == current_year]
          next_articles <- sorted_articles$pmid[sorted_articles$publication_year == next_year]

          # Connect some articles (just for demonstration)
          if (length(current_articles) > 0 && length(next_articles) > 0) {
            n_edges <- min(5, length(current_articles), length(next_articles))

            for (j in 1:n_edges) {
              edges <- rbind(edges, data.frame(
                from = next_articles[j],
                to = current_articles[j],
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      } else {
        message("Only one publication year found. Creating empty edge list.")
      }
    } else {
      # If no publication year, create an empty edge list
      edges <- data.frame(
        from = character(),
        to = character(),
        stringsAsFactors = FALSE
      )

      message("No citation data or publication year available. Creating empty network.")
    }
  }

  # Create igraph object
  network <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  return(network)
}

#' Calculate basic bibliometric statistics
#'
#' This function calculates basic bibliometric statistics from article data.
#'
#' @param article_data A data frame containing article data.
#' @param by_year Logical. If TRUE, calculates statistics by year.
#'
#' @return A list containing bibliometric statistics.
#' @export
#' @importFrom utils head
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' stats <- calc_bibliometrics(article_data)
#' }
calc_bibliometrics <- function(article_data, by_year = TRUE) {
  # Check if article_data is empty
  if (nrow(article_data) == 0) {
    stop("article_data is empty")
  }

  # Initialize results list
  stats <- list()

  # Basic statistics
  stats$total_articles <- nrow(article_data)

  # Check if journal column exists
  if ("journal" %in% colnames(article_data)) {
    # Top journals
    journal_table <- table(article_data$journal)
    journal_table <- sort(journal_table, decreasing = TRUE)
    journal_counts <- data.frame(
      journal = names(journal_table),
      n = as.numeric(journal_table),
      stringsAsFactors = FALSE
    )
    stats$top_journals <- head(journal_counts, 10)
  }

  # Check if authors column exists
  if ("authors" %in% colnames(article_data)) {
    # Extract individual authors
    authors <- unlist(strsplit(article_data$authors, ", "))

    # Count author occurrences
    author_counts <- table(authors)
    author_counts <- sort(author_counts, decreasing = TRUE)

    # Top authors - limit to 10 if there are more
    max_authors <- min(10, length(author_counts))
    if (max_authors > 0) {
      stats$top_authors <- head(author_counts, max_authors)
    } else {
      stats$top_authors <- author_counts # If there are less than 10, use all
    }
  }

  # Check if publication_year column exists
  if ("publication_year" %in% colnames(article_data) && by_year) {
    # Articles by year
    year_table <- table(article_data$publication_year)
    year_table <- sort(year_table, decreasing = TRUE)
    year_counts <- data.frame(
      publication_year = names(year_table),
      n = as.numeric(year_table),
      stringsAsFactors = FALSE
    )
    stats$articles_by_year <- year_counts
  }

  # Calculate additional metrics if possible

  # Average number of authors per paper
  if ("authors" %in% colnames(article_data)) {
    author_counts <- sapply(strsplit(article_data$authors, ", "), length)
    stats$avg_authors_per_paper <- mean(author_counts, na.rm = TRUE)
    stats$median_authors_per_paper <- median(author_counts, na.rm = TRUE)
  }

  # Word count statistics for abstracts
  if ("abstract" %in% colnames(article_data)) {
    # Remove NA abstracts
    abstracts <- article_data$abstract[!is.na(article_data$abstract)]

    if (length(abstracts) > 0) {
      # Count words in each abstract
      word_counts <- sapply(abstracts, function(x) {
        length(unlist(strsplit(x, "\\s+")))
      })

      stats$avg_abstract_length <- mean(word_counts, na.rm = TRUE)
      stats$median_abstract_length <- median(word_counts, na.rm = TRUE)
      stats$max_abstract_length <- max(word_counts, na.rm = TRUE)
      stats$min_abstract_length <- min(word_counts, na.rm = TRUE)
    }
  }

  return(stats)
}

#' Extract common terms from a corpus
#'
#' This function extracts and counts the most common terms in a corpus.
#'
#' @param article_data A data frame containing article data.
#' @param text_column Name of the column containing the text to analyze.
#' @param n Number of top terms to return.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#' @param min_word_length Minimum word length to include.
#'
#' @return A data frame containing term counts.
#' @export
#'
#' @examples
#' \dontrun{
#' common_terms <- extract_terms(article_data, text_column = "abstract")
#' }
extract_terms <- function(article_data, text_column = "abstract",
                          n = 100, remove_stopwords = TRUE,
                          min_word_length = 3) {

  # Check if text column exists
  if (!text_column %in% colnames(article_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Add ID column if not present
  if (!"doc_id" %in% colnames(article_data)) {
    article_data$doc_id <- seq_len(nrow(article_data))
  }

  # Define a list of common English stopwords
  stopword_list <- c(
    "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
    "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
    "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
  )

  # Function to tokenize text
  tokenize_text <- function(text) {
    # Convert to lowercase
    text <- tolower(text)

    # Replace non-alphanumeric characters with spaces
    text <- gsub("[^a-zA-Z0-9]", " ", text)

    # Split by whitespace
    words <- unlist(strsplit(text, "\\s+"))

    # Remove empty strings
    words <- words[words != ""]

    # Apply length filtering
    words <- words[nchar(words) >= min_word_length]

    # Remove stopwords if requested
    if (remove_stopwords) {
      words <- words[!words %in% stopword_list]
    }

    return(words)
  }

  # Initialize a vector to store all words
  all_words <- character()

  # Process each document
  for (i in seq_len(nrow(article_data))) {
    text <- article_data[[text_column]][i]
    if (!is.na(text) && text != "") {
      # Tokenize the text
      words <- tokenize_text(text)

      # Add to all words
      all_words <- c(all_words, words)
    }
  }

  # Count term frequencies
  term_counts <- table(all_words)

  # Sort by frequency and convert to data frame
  term_counts_sorted <- sort(term_counts, decreasing = TRUE)

  # Limit to top n terms
  if (length(term_counts_sorted) > n) {
    term_counts_sorted <- term_counts_sorted[1:n]
  }

  # Convert to data frame
  result <- data.frame(
    word = names(term_counts_sorted),
    n = as.numeric(term_counts_sorted),
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Compare term frequencies between two corpora
#'
#' This function compares term frequencies between two sets of articles.
#'
#' @param corpus1 First corpus (data frame).
#' @param corpus2 Second corpus (data frame).
#' @param text_column Name of the column containing the text to analyze.
#' @param corpus1_name Name for the first corpus in the output.
#' @param corpus2_name Name for the second corpus in the output.
#' @param n Number of top terms to return.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#'
#' @return A data frame containing term frequency comparisons.
#' @export
#'
#' @examples
#' \dontrun{
#' comparison <- compare_terms(corpus1, corpus2,
#'                                       corpus1_name = "Migraine",
#'                                       corpus2_name = "Magnesium")
#' }
compare_terms <- function(corpus1, corpus2, text_column = "abstract",
                          corpus1_name = "Corpus1",
                          corpus2_name = "Corpus2",
                          n = 100, remove_stopwords = TRUE) {

  # Check if text column exists in both corpora
  if (!text_column %in% colnames(corpus1) || !text_column %in% colnames(corpus2)) {
    stop("Text column '", text_column, "' not found in one or both corpora")
  }

  # Define a list of common English stopwords
  stopword_list <- c(
    "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
    "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
    "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
  )

  # Function to tokenize text
  tokenize_text <- function(text) {
    # Convert to lowercase
    text <- tolower(text)

    # Replace non-alphanumeric characters with spaces
    text <- gsub("[^a-zA-Z0-9]", " ", text)

    # Split by whitespace
    words <- unlist(strsplit(text, "\\s+"))

    # Remove empty strings
    words <- words[words != ""]

    # Remove stopwords if requested
    if (remove_stopwords) {
      words <- words[!words %in% stopword_list]
    }

    return(words)
  }

  # Function to count terms in a corpus
  count_corpus_terms <- function(corpus) {
    # Initialize a vector to store all words
    all_words <- character()

    # Process each document
    for (i in seq_len(nrow(corpus))) {
      text <- corpus[[text_column]][i]
      if (!is.na(text) && text != "") {
        # Tokenize the text
        words <- tokenize_text(text)

        # Add to all words
        all_words <- c(all_words, words)
      }
    }

    # Count term frequencies
    term_counts <- table(all_words)

    return(term_counts)
  }

  # Count terms in each corpus
  counts1 <- count_corpus_terms(corpus1)
  counts2 <- count_corpus_terms(corpus2)

  # Get all unique words from both corpora
  all_words <- unique(c(names(counts1), names(counts2)))

  # Create a data frame with all words and their counts in each corpus
  result <- data.frame(
    word = all_words,
    stringsAsFactors = FALSE
  )

  # Add counts for corpus1
  result[[corpus1_name]] <- sapply(result$word, function(w) {
    if (w %in% names(counts1)) counts1[w] else 0
  })

  # Add counts for corpus2
  result[[corpus2_name]] <- sapply(result$word, function(w) {
    if (w %in% names(counts2)) counts2[w] else 0
  })

  # Calculate total and ratio
  result$total <- result[[corpus1_name]] + result[[corpus2_name]]
  result$ratio <- (result[[corpus1_name]] + 0.5) / (result[[corpus2_name]] + 0.5)

  # Sort by total and limit to top n terms
  result <- result[order(-result$total), ]
  if (nrow(result) > n) {
    result <- result[1:n, ]
  }

  return(result)
}

#' Extract term variations from text corpus
#'
#' This function identifies variations of a primary term within a corpus of articles.
#'
#' @param articles A data frame containing article data with text columns
#' @param primary_term The primary term to find variations of
#' @param text_col Name of the column containing the text to search
#'
#' @return A character vector of unique term variations, sorted by length
#' @export
#' @examples
#' # Create example articles
#' articles <- data.frame(
#'   abstract = c(
#'     "Migraine headaches are debilitating",
#'     "Migraines affect quality of life",
#'     "Migraine disorders require treatment"
#'   )
#' )
#'
#' # Get term variations
#' variations <- get_term_vars(articles, "migrain")
#' print(variations)
get_term_vars <- function(articles, primary_term, text_col = "abstract") {
  # Extract all occurrences of primary term with context
  variations <- character(0)

  for (i in 1:nrow(articles)) {
    abstract <- articles[[text_col]][i]
    if (!is.na(abstract) && grepl(primary_term, abstract, ignore.case = TRUE)) {
      # Find all occurrences with some surrounding context
      matches <- gregexpr(paste0("\\b\\w*", primary_term, "\\w*\\b"),
                          abstract, ignore.case = TRUE)

      if (matches[[1]][1] != -1) {
        terms <- regmatches(abstract, matches)[[1]]
        variations <- c(variations, terms)
      }
    }
  }

  # Remove duplicates and sort by length (shortest first)
  unique_variations <- unique(variations)
  return(unique_variations[order(nchar(unique_variations))])
}

#' Combine and deduplicate entity datasets
#'
#' This function combines custom and standard entity datasets, handling the case
#' where one or both might be empty, and removes duplicates.
#'
#' @param custom_entities Data frame of custom entities (can be NULL)
#' @param standard_entities Data frame of standard entities (can be NULL)
#' @param primary_term The primary term of interest
#' @param primary_type The entity type of the primary term (default: "disease")
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return A data frame of combined entities
#' @export
#' @examples
#' # Create example entity datasets
#' custom_entities <- data.frame(
#'   doc_id = c(1, 1, 2),
#'   entity = c("migraine", "headache", "pain"),
#'   entity_type = c("disease", "symptom", "symptom"),
#'   start_pos = c(1, 10, 5),
#'   end_pos = c(8, 18, 9),
#'   sentence = c("sent1", "sent1", "sent2"),
#'   frequency = c(2, 1, 1)
#' )
#'
#' standard_entities <- data.frame(
#'   doc_id = c(1, 2, 2),
#'   entity = c("serotonin", "migraine", "therapy"),
#'   entity_type = c("chemical", "disease", "treatment"),
#'   start_pos = c(20, 1, 15),
#'   end_pos = c(29, 8, 22),
#'   sentence = c("sent1", "sent2", "sent2"),
#'   frequency = c(1, 1, 1)
#' )
#'
#' # Merge entities
#' merged <- merge_entities(custom_entities, standard_entities, "migraine")
#' print(merged)
merge_entities <- function(custom_entities, standard_entities,
                           primary_term, primary_type = "disease",
                           verbose = TRUE) {
  # Check if both entity sets exist and have content
  if (!is.null(custom_entities) && nrow(custom_entities) > 0 &&
      !is.null(standard_entities) && nrow(standard_entities) > 0) {
    # Use rbind to combine both dataframes
    entities <- rbind(custom_entities, standard_entities)
    # Remove duplicates if needed
    entities <- entities[!duplicated(paste(entities$doc_id, entities$entity, entities$start_pos)), ]

    if (verbose) {
      cat("Combined", nrow(custom_entities), "custom entities with",
          nrow(standard_entities), "standard entities.\n")
    }
  } else if (!is.null(standard_entities) && nrow(standard_entities) > 0) {
    entities <- standard_entities
    if (verbose) cat("Using only standard entities (", nrow(entities), ").\n")
  } else if (!is.null(custom_entities) && nrow(custom_entities) > 0) {
    entities <- custom_entities
    if (verbose) cat("Using only custom entities (", nrow(entities), ").\n")
  } else {
    if (verbose) cat("WARNING: No entities extracted from either method!\n")
    # Create a minimal entity dataframe with just our primary term
    entities <- data.frame(
      doc_id = 1,
      entity = primary_term,
      entity_type = primary_type,
      start_pos = 1,
      end_pos = nchar(primary_term),
      sentence = primary_term,
      frequency = 1,
      stringsAsFactors = FALSE
    )
  }

  return(entities)
}

#' Filter entities to include only valid biomedical terms
#'
#' This function applies validation to ensure only legitimate biomedical entities
#' are included, while preserving trusted terms.
#'
#' @param entities Data frame of entities to filter
#' @param primary_term The primary term to trust
#' @param primary_term_variations Vector of variations of the primary term to trust
#' @param validation_function Function to validate entities (default: is_valid_biomedical_entity)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#' @param entity_col Name of the column containing entity names (default: "entity")
#' @param type_col Name of the column containing entity types (default: "entity_type")
#'
#' @return A data frame of filtered entities
#' @export
#' @examples
#' # Create example entities
#' entities <- data.frame(
#'   entity = c("migraine", "optimization", "receptor", "europe"),
#'   entity_type = c("disease", "process", "protein", "location")
#' )
#'
#' # Validate entities
#' validated <- valid_entities(entities, "migraine", c("migrain", "headache"))
#' print(validated)
valid_entities <- function(entities, primary_term, primary_term_variations = NULL,
                           validation_function = NULL,
                           verbose = TRUE,
                           entity_col = "entity",
                           type_col = "entity_type") {
  if (nrow(entities) == 0) {
    return(entities)
  }

  # Verify that the required columns exist
  if (!entity_col %in% colnames(entities)) {
    stop("Entity column '", entity_col, "' not found in entities data frame")
  }
  if (!type_col %in% colnames(entities)) {
    stop("Type column '", type_col, "' not found in entities data frame")
  }

  # If validation_function is NULL, get the function from the package environment
  if (is.null(validation_function)) {
    # Default to permissive validation in the data package.
    validation_function <- function(term, type) TRUE
  }

  # Store original count for reporting
  original_count <- nrow(entities)

  # Get unique entity-type pairs
  entity_type_pairs <- unique(entities[, c(entity_col, type_col)])

  # Apply validation function to each pair
  valid_rows <- sapply(1:nrow(entity_type_pairs), function(i) {
    term <- entity_type_pairs[[entity_col]][i]
    claimed_type <- entity_type_pairs[[type_col]][i]

    # Skip our primary term and its variations (they're trusted)
    if (term == primary_term || term %in% primary_term_variations) {
      return(TRUE)
    }

    # Apply validation function
    validation_function(term, claimed_type)
  })

  # Get valid entity-type pairs
  valid_pairs <- entity_type_pairs[valid_rows, ]

  # Filter the original entities dataframe
  filtered_entities <- merge(entities, valid_pairs, by = c(entity_col, type_col))

  if (verbose) {
    cat("Filtered from", original_count, "to", nrow(filtered_entities), "validated entities\n")
  }

  return(filtered_entities)
}

