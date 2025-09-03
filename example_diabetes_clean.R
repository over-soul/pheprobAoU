# Example: Calculate PheProb for Diabetes (Clean - No Validation Errors)
# Now that existence verification is disabled by default, this will run without errors

library(pheprobAoU)

# Define diabetes-related concepts
diabetes_concepts <- c(4193704, 201826)

# Calculate phenotype probabilities - no more validation error messages!
diabetes_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  progress = TRUE
)

# The analysis will proceed without any validation error messages
print(diabetes_scores)

# If you ever want to enable existence checking (e.g., for new/untested concepts):
# diabetes_scores <- calculate_pheprob(
#   concept_ids = diabetes_concepts,
#   check_concept_existence = TRUE,  # Only use if you want database validation
#   progress = TRUE
# )
