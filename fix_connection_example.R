# Load required libraries
library(allofus)
library(pheprobAoU)

# Step 1: Establish connection to All of Us database
aou_connect()

# Step 2: Verify connection is working
cat("Checking connection...\n")
workspaces <- aou_ls_workspaces()
if (length(workspaces) > 0) {
  cat("✅ Connected to All of Us successfully!\n")
  cat("Available workspaces:", length(workspaces), "\n")
} else {
  cat("❌ Connection failed or no workspaces available\n")
}

# Step 3: Now run your PheProb calculation
diabetes_concepts <- c(201826, 4329847, 9201)

cat("Running PheProb calculation...\n")
diabetes_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  progress = TRUE
)

print(diabetes_scores)
summary(diabetes_scores)
