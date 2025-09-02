# Demo: Multiple Phenotypes Mode in pheprobAoU
# This script demonstrates the new functionality for handling multiple phenotypes

# Note: This demo shows expected output without requiring the package to be installed
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# =============================================================================
# PROBLEM: Mixed unrelated concepts (DON'T DO THIS)
# =============================================================================

mixed_concepts <- c(
  201826,   # Type 2 diabetes
  313217,   # Atrial fibrillation  
  432870,   # Depression
  4030518   # Chronic kidney disease
)

# This would produce a meaningless pheprob_score:
# What is a pheprob_score of 0.73 for this mix? It's not interpretable!

# =============================================================================
# SOLUTION: Multiple Phenotypes Mode (DO THIS INSTEAD)
# =============================================================================

# Define coherent phenotypes
phenotypes <- list(
  diabetes = c(201826, 4329847, 9201),           # Diabetes-related concepts
  cardiovascular = c(314866, 313217, 316866),    # CVD-related concepts  
  mental_health = c(4152280, 4226263, 436073),   # Mental health concepts
  chronic_kidney = c(4030518, 192359, 4030319)   # CKD-related concepts
)

# DEMO OUTPUT: What the results would look like
# =============================================================================

cat("=== MULTIPLE PHENOTYPES ANALYSIS ===\n\n")

cat("1. Input validation:\n")
cat("✓ All phenotypes have valid names\n")
cat("✓ diabetes: 3 concepts\n") 
cat("✓ cardiovascular: 3 concepts\n")
cat("✓ mental_health: 3 concepts\n")  
cat("✓ chronic_kidney: 3 concepts\n\n")

cat("2. Processing each phenotype separately:\n")
cat("ℹ Processing diabetes phenotype...\n")
cat("✓ diabetes: 8,234 persons, mean score = 0.342\n")
cat("ℹ Processing cardiovascular phenotype...\n") 
cat("✓ cardiovascular: 8,234 persons, mean score = 0.198\n")
cat("ℹ Processing mental_health phenotype...\n")
cat("✓ mental_health: 8,234 persons, mean score = 0.127\n")
cat("ℹ Processing chronic_kidney phenotype...\n")
cat("✓ chronic_kidney: 8,234 persons, mean score = 0.089\n\n")

# Create demo results data frame (what the actual function would return)
set.seed(123)
n_persons <- 10

demo_results <- data.frame(
  person_id = sample(100000:999999, n_persons),
  diabetes_prob = round(rbeta(n_persons, 2, 5), 3),        # Most people low risk
  cardiovascular_prob = round(rbeta(n_persons, 1.5, 6), 3), # Even lower base rate
  mental_health_prob = round(rbeta(n_persons, 1, 8), 3),    # Lower base rate
  chronic_kidney_prob = round(rbeta(n_persons, 1, 10), 3)   # Lowest base rate
)

cat("3. Results (wide format):\n")
print(demo_results)
cat("\n")

# =============================================================================
# MEANINGFUL ANALYSIS NOW POSSIBLE
# =============================================================================

cat("=== MEANINGFUL ANALYSIS ===\n\n")

# 1. Individual phenotype summaries
cat("1. Phenotype prevalence (high probability > 0.8):\n")
high_prob_summary <- demo_results %>%
  summarise(
    diabetes_high = sum(diabetes_prob > 0.8),
    cvd_high = sum(cardiovascular_prob > 0.8), 
    mental_high = sum(mental_health_prob > 0.8),
    kidney_high = sum(chronic_kidney_prob > 0.8)
  )
print(high_prob_summary)
cat("\n")

# 2. Comorbidity analysis
cat("2. Comorbidity patterns:\n")
comorbidity <- demo_results %>%
  mutate(
    high_diabetes = diabetes_prob > 0.5,
    high_cvd = cardiovascular_prob > 0.5,
    high_mental = mental_health_prob > 0.5,
    high_kidney = chronic_kidney_prob > 0.5
  ) %>%
  summarise(
    diabetes_only = sum(high_diabetes & !high_cvd & !high_mental & !high_kidney),
    cvd_only = sum(!high_diabetes & high_cvd & !high_mental & !high_kidney),
    diabetes_cvd = sum(high_diabetes & high_cvd),
    no_conditions = sum(!high_diabetes & !high_cvd & !high_mental & !high_kidney)
  )
print(comorbidity)
cat("\n")

# 3. Risk stratification
cat("3. Population risk stratification:\n")
risk_strata <- demo_results %>%
  mutate(
    overall_risk = case_when(
      diabetes_prob > 0.8 | cardiovascular_prob > 0.8 ~ "High Risk",
      diabetes_prob > 0.5 | cardiovascular_prob > 0.5 ~ "Moderate Risk",
      mental_health_prob > 0.5 | chronic_kidney_prob > 0.5 ~ "Specialized Care",
      TRUE ~ "Low Risk"
    )
  ) %>%
  count(overall_risk)
print(risk_strata)
cat("\n")

# =============================================================================
# LONG FORMAT FOR ADVANCED ANALYSIS  
# =============================================================================

cat("=== LONG FORMAT OUTPUT ===\n\n")

# Convert to long format (what output_format = "long" would produce)
demo_long <- demo_results %>%
  pivot_longer(
    cols = ends_with("_prob"),
    names_to = "phenotype_name", 
    values_to = "pheprob_score"
  ) %>%
  mutate(
    phenotype_name = str_remove(phenotype_name, "_prob$")
  )

cat("Long format sample:\n")
print(head(demo_long, 12))
cat("\n")

# Analysis by phenotype in long format
cat("Summary by phenotype:\n")
phenotype_summary <- demo_long %>%
  group_by(phenotype_name) %>%
  summarise(
    mean_score = round(mean(pheprob_score), 3),
    median_score = round(median(pheprob_score), 3),
    high_prob_cases = sum(pheprob_score > 0.8),
    .groups = "drop"
  )
print(phenotype_summary)
cat("\n")

# =============================================================================
# VALIDATION AND WARNINGS
# =============================================================================

cat("=== PHENOTYPE COHERENCE VALIDATION ===\n\n")

# What the validation function would show
cat("Phenotype coherence validation results:\n")
validation_demo <- data.frame(
  phenotype_name = c("diabetes", "cardiovascular", "mental_health", "chronic_kidney"),
  n_concepts = c(3, 3, 3, 3),
  n_valid_concepts = c(3, 3, 3, 3),
  n_warnings = c(0, 0, 0, 0)
)
print(validation_demo)
cat("✓ All phenotypes passed coherence validation\n\n")

# Example of warning for mixed concepts
cat("Example warning for problematic mixed concepts:\n")
cat("⚠ mixed_example: Concepts span 4 domains (Condition, Procedure, Drug, Observation).\n")
cat("⚠ Consider reviewing concept selection.\n\n")

# =============================================================================
# COMPARISON: BEFORE VS AFTER
# =============================================================================

cat("=== BEFORE vs AFTER COMPARISON ===\n\n")

cat("BEFORE (mixing unrelated concepts):\n")
cat("❌ Single pheprob_score = 0.73 (meaningless)\n")
cat("❌ Cannot interpret what this represents\n") 
cat("❌ No way to analyze specific conditions\n")
cat("❌ Poor clinical utility\n\n")

cat("AFTER (multiple phenotypes mode):\n")  
cat("✅ diabetes_prob = 0.89 (89% probability of diabetes)\n")
cat("✅ cardiovascular_prob = 0.45 (45% probability of CVD)\n")
cat("✅ mental_health_prob = 0.12 (12% probability of mental health condition)\n")
cat("✅ chronic_kidney_prob = 0.23 (23% probability of CKD)\n")
cat("✅ Each score is clinically interpretable\n")
cat("✅ Enables comorbidity analysis\n")
cat("✅ Supports personalized risk stratification\n")
cat("✅ High clinical utility\n\n")

cat("=== DEMO COMPLETE ===\n")
cat("The multiple phenotypes mode solves the conceptual problem\n") 
cat("of mixing unrelated medical concepts and provides clinically\n")
cat("meaningful, interpretable results for research and care.\n")
