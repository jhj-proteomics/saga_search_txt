library(stringr)

# Function to search for keywords in a single text file and extract variations
search_keywords_in_txt <- function(txt_path, keywords) {
  # Read text from file
  text <- tolower(readLines(txt_path, warn = FALSE))
  
  # Combine text lines into a single string
  combined_text <- paste(text, collapse = " ")
  
  # Create a list to store keyword occurrences and variations
  keyword_variations <- list()
  
  # Search for each keyword and extract occurrences
  for (keyword in keywords) {
    pattern <- paste0("\\b(", keyword, "\\w*)")
    matches <- str_extract_all(combined_text, pattern)[[1]]
    if (length(matches) > 0) {
      variations_count <- table(matches)
      keyword_variations[[keyword]] <- as.list(variations_count)
    }
  }
  
  return(keyword_variations)
}

# Function to search through multiple text files and gather results
search_keywords_in_txts <- function(txt_folder, keywords) {
  # List all text files in the folder
  txt_files <- list.files(txt_folder, pattern = "\\.txt$", full.names = TRUE)
  
  # Create a list to store results
  results <- list()
  
  # Loop through each text file and search for keywords
  for (txt_file in txt_files) {
    keyword_variations <- search_keywords_in_txt(txt_file, keywords)
    if (length(keyword_variations) > 0) {
      results[[basename(txt_file)]] <- keyword_variations
    }
  }
  
  return(results)
}

# Define the folder containing your text files
txt_folder <- "C:/Users/skl448/Desktop/phd/data/sagas"

# Define the keywords to search for
keywords <- c("goat", "sheep", "wool")

# Search for keywords in the text files
results <- search_keywords_in_txts(txt_folder, keywords)

# Filter results to exclude text files without the keyword
filtered_results <- results[lengths(results) > 0]

# Print the filtered results
print(filtered_results)

# Create a summary of keyword counts per text file
keyword_summary <- data.frame(file = character(), variation = character(), count = integer(), stringsAsFactors = FALSE)

for (saga in names(filtered_results)) {
  for (keyword in names(filtered_results[[saga]])) {
    for (variation in names(filtered_results[[saga]][[keyword]])) {
      count <- filtered_results[[saga]][[keyword]][[variation]]
      keyword_summary <- rbind(keyword_summary, data.frame(file = saga, variation = variation, count = count, stringsAsFactors = FALSE))
    }
  }
}

# Print the keyword summary
print(keyword_summary)
