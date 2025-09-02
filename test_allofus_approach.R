# Test Script: AllofUs-Native Data Extraction
#
# This script demonstrates the new AllofUs-native approach for data extraction
# that replaces the raw SQL queries.

library(pheprobAoU)

cat("=== Testing AllofUs-Native PheProb Data Extraction ===\n\n")

# Test diabetes concepts
diabetes_concepts <- c(201826, 4329847, 9201)

cat("Testing concept IDs:", paste(diabetes_concepts, collapse = ", "), "\n\n")

# Test the main function (which should auto-detect environment and use appropriate method)
cat("Testing main function with environment auto-detection...\n")

tryCatch({
  # This should automatically use AllofUs-native approach in cloud, legacy approach locally
  diabetes_scores <- calculate_pheprob(
    concept_ids = diabetes_concepts,
    progress = TRUE
  )
  
  cat("✅ SUCCESS! Got data for", nrow(diabetes_scores), "persons\n")
  cat("First few rows:\n")
  print(head(diabetes_scores))
  
}, error = function(e) {
  cat("❌ FAILED:", e$message, "\n")
  
  # If in AllofUs environment, provide debugging information
  if (pheprobAoU:::is_aou_environment()) {
    cat("\n=== Debugging Information ===\n")
    cat("Environment detected: AllofUs cloud\n")
    cat("Connection status:", pheprobAoU::is_aou_connected(), "\n")
    
    # Test individual components
    cat("\nTesting AllofUs table access...\n")
    tryCatch({
      tables <- allofus::aou_tables()
      cat("✅ Can access table list:", nrow(tables), "tables found\n")
    }, error = function(e2) {
      cat("❌ Cannot access table list:", e2$message, "\n")
    })
    
    cat("\nTesting simple dplyr query...\n")
    tryCatch({
      con <- allofus::aou_connect()
      result <- dplyr::tbl(con, "person") %>% 
        dplyr::count() %>% 
        dplyr::collect()
      cat("✅ Simple dplyr query works: person table has", result$n, "rows\n")
    }, error = function(e3) {
      cat("❌ Simple dplyr query failed:", e3$message, "\n")
    })
  }
})

cat("\n=== Test Complete ===\n")
