# Debug: Explore Microalgae R Object Loading
# Purpose: Diagnose issues with loading the R object
# Date: 2026-05-02

library(tidyverse)

# Check if file exists
file_path <- "data-raw/MicroAlgae_Collection_NeonData.Robj"

cat("Checking file...\n")
cat("  File path:", file_path, "\n")
cat("  File exists:", file.exists(file_path), "\n")

if (file.exists(file_path)) {
  cat("  File size:", file.size(file_path), "bytes\n")
  cat("  File info:\n")
  print(file.info(file_path))

  cat("\n\nAttempting to load...\n")

  # Try loading
  tryCatch(
    {
      load(file_path)
      cat("✓ File loaded successfully\n\n")

      # List what was loaded
      cat("Objects in workspace after loading:\n")
      print(ls())

      # Check each object
      for (obj in ls()) {
        cat("\n", obj, ":\n")
        cat("  Class:", class(get(obj)), "\n")
        if (is.data.frame(get(obj))) {
          cat("  Dimensions:", dim(get(obj)), "\n")
          cat("  Columns:", paste(colnames(get(obj))[1:min(5, ncol(get(obj)))], collapse=", "), "...\n")
        }
      }
    },
    error = function(e) {
      cat("✗ Error loading file:\n")
      cat("  ", e$message, "\n")
    }
  )
} else {
  cat("✗ File not found at:", file_path, "\n\n")

  cat("Available files in data-raw/:\n")
  files <- list.files("data-raw/", pattern = "Algae|algae|Micro", full.names = FALSE)
  if (length(files) > 0) {
    print(files)
  } else {
    cat("  No files matching pattern 'Algae/algae/Micro'\n")
    cat("\n  All files in data-raw/:\n")
    print(list.files("data-raw/"))
  }
}
