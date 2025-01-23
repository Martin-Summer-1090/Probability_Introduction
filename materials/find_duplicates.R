# Path to your .qmd file
file_path <- "04-lecture4.qmd"

# Read the file
lines <- readLines(file_path)

# Extract chunk labels
chunk_labels <- gsub("^```\\{r ([^,\\}]+).*", "\\1", grep("^```\\{r ", lines, value = TRUE))

# Find duplicates
duplicates <- chunk_labels[duplicated(chunk_labels)]

# Output duplicates
if (length(duplicates) > 0) {
  cat("Duplicate chunk labels found:\n")
  print(unique(duplicates))
} else {
  cat("No duplicate chunk labels found.\n")
}
