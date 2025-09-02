# pheprobAoU: Calculate PheProb using All of Us EHR Data

[![R-CMD-check](https://github.com/yourusername/pheprobAoU/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/pheprobAoU/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/yourusername/pheprobAoU/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yourusername/pheprobAoU?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/pheprobAoU)](https://CRAN.R-project.org/package=pheprobAoU)

## Overview

`pheprobAoU` is an R package for calculating PheProb (phenotype probabilities) using electronic health record (EHR) data from the All of Us Research Program. The package provides a comprehensive toolkit for extracting clinical features and computing probabilistic phenotype predictions based on OMOP concept IDs.

PheProb scores represent the likelihood that a participant exhibits a particular phenotype based on their clinical history. This package supports multiple scoring methods, from simple binary presence/absence to sophisticated temporal and composite scoring approaches.

## Key Features

- **Seamless All of Us Integration**: Direct interface with the `allofus` R package for EHR data access
- **Multiple Scoring Methods**: Binary, weighted, temporal, and composite PheProb calculations
- **Flexible Input/Output**: Support for various data formats and export options
- **Comprehensive Validation**: Robust input validation and error handling
- **Batch Processing**: Efficient processing of large datasets with progress indicators
- **Extensive Documentation**: Complete function documentation with real-world examples

## Installation

### Prerequisites

Before installing `pheprobAoU`, ensure you have:

1. **R 4.0.0 or higher**
2. **All of Us Research Program access** with appropriate data use agreements
3. **Active All of Us workbench environment** (if running in the cloud)

### Installation Methods

#### From GitHub (Development Version)

```r
# Install devtools if you haven't already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install pheprobAoU
devtools::install_github("yourusername/pheprobAoU")
```

#### From CRAN (When Available)

```r
install.packages("pheprobAoU")
```

#### Dependencies

The package will automatically install required dependencies:

- `allofus` (>= 1.0.0) - All of Us R interface
- `dplyr` (>= 1.1.0) - Data manipulation
- `tibble` (>= 3.2.0) - Modern data frames
- `tidyr` (>= 1.3.0) - Data tidying
- `cli` (>= 3.6.0) - Command line interface
- `glue` (>= 1.6.0) - String interpolation

## Quick Start

### Basic Usage

```r
library(pheprobAoU)

# Define diabetes-related OMOP concept IDs
diabetes_concepts <- c(
  201826,   # Type 2 diabetes mellitus
  4329847,  # Diabetes mellitus
  9201      # Diabetes complication
)

# Calculate simple binary PheProb scores
diabetes_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "simple"
)

# View results
head(diabetes_scores)
```

### Advanced Example with Custom Parameters

```r
# Calculate weighted PheProb scores with custom weights and date filtering
advanced_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "weighted",
  weights = c("201826" = 2.0, "4329847" = 1.5, "9201" = 1.0),
  domains = c("condition", "procedure", "drug"),
  date_range = list(
    start = as.Date("2020-01-01"), 
    end = Sys.Date()
  ),
  normalization = "minmax",
  output_format = "wide",
  output_file = "diabetes_pheprob_scores.csv"
)
```

## Core Functions

### `calculate_pheprob()`

The main function for PheProb calculation with extensive customization options:

```r
calculate_pheprob(
  concept_ids,                    # Required: OMOP concept IDs
  person_ids = NULL,              # Optional: specific person IDs
  method = "composite",           # Scoring method
  domains = c("condition", "procedure", "drug", "measurement", "observation"),
  date_range = NULL,              # Temporal filtering
  weights = NULL,                 # Custom concept weights
  normalization = "minmax",       # Score normalization
  output_format = "wide",         # Output format
  output_file = NULL,             # Export file
  batch_size = 10000,            # Batch processing size
  progress = TRUE                 # Progress indicators
)
```

### `calculate_multiple_pheprobs()`

**NEW!** Calculate separate PheProb scores for multiple unrelated phenotypes:

```r
# Define multiple phenotypes (avoids meaningless mixing of unrelated concepts)
phenotypes <- list(
  diabetes = c(201826, 4329847, 9201),
  cardiovascular = c(314866, 313217, 316866),
  mental_health = c(4152280, 4226263)
)

# Get separate probability for each phenotype
multi_scores <- calculate_multiple_pheprobs(
  phenotype_concepts = phenotypes,
  method = "composite"
)

head(multi_scores)
# person_id diabetes_prob cardiovascular_prob mental_health_prob
#    123456          0.89                0.45               0.12
#    234567          0.23                0.91               0.67
```

### `validate_concept_ids()`

Validate OMOP concept IDs before analysis:

```r
validation_result <- validate_concept_ids(
  concept_ids = c(201826, 4329847, 9999999),  # Include invalid ID
  check_existence = TRUE
)

print(validation_result$summary)
```

### `validate_phenotype_coherence()`

**NEW!** Validate that phenotype definitions are clinically coherent:

```r
phenotypes <- list(
  diabetes = c(201826, 4329847, 9201),
  mixed_concepts = c(201826, 313217, 432870)  # Problematic mixing
)

validation <- validate_phenotype_coherence(phenotypes)
# Warns about mixed domains and unrelated concepts
```

### `extract_ehr_features()`

Extract raw EHR features for custom analysis:

```r
features <- extract_ehr_features(
  concept_ids = diabetes_concepts,
  domains = c("condition", "procedure"),
  date_range = list(start = as.Date("2018-01-01"), end = Sys.Date())
)

# Create custom feature matrix
feature_matrix <- create_feature_matrix(
  features, 
  feature_type = "log_count"
)
```

## Scoring Methods

### Simple Binary Scoring

Assigns 1 for concept presence, 0 for absence:

```r
binary_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "simple"
)
```

### Weighted Frequency Scoring

Uses log-transformed occurrence counts:

```r
weighted_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "weighted"
)
```

### Temporal Scoring

Considers recency of clinical events:

```r
temporal_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "temporal"
)
```

### Composite Scoring

Combines multiple approaches:

```r
composite_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "composite"
)
```

## Real-World Examples

### Multiple Phenotypes Analysis (Recommended Approach)

For analyzing multiple unrelated conditions, use the new multiple phenotypes mode:

```r
# Define separate phenotypes instead of mixing concepts
research_phenotypes <- list(
  type2_diabetes = c(201826, 4329847, 9201, 4193704),
  cardiovascular = c(314866, 313217, 316866, 4329847),
  mental_health = c(4152280, 4226263, 436073, 4059317),
  chronic_kidney = c(4030518, 192359, 4030319, 4030320)
)

# Calculate separate probabilities
multi_phenotype_scores <- calculate_multiple_pheprobs(
  phenotype_concepts = research_phenotypes,
  method = "composite",
  output_format = "wide"
)

# Result: Clean separation of phenotype probabilities
# person_id type2_diabetes_prob cardiovascular_prob mental_health_prob chronic_kidney_prob
#    123456               0.91                0.67               0.23                0.12
#    234567               0.34                0.89               0.78                0.45

# Analyze comorbidity patterns
comorbidity_analysis <- multi_phenotype_scores %>%
  mutate(
    high_diabetes = type2_diabetes_prob > 0.8,
    high_cvd = cardiovascular_prob > 0.8,
    comorbid_diabetes_cvd = high_diabetes & high_cvd
  ) %>%
  summarise(
    n_diabetes = sum(high_diabetes),
    n_cvd = sum(high_cvd), 
    n_comorbid = sum(comorbid_diabetes_cvd)
  )
```

### Cardiovascular Disease Phenotyping

```r
# Cardiovascular disease concepts
cvd_concepts <- c(
  314866,   # Myocardial infarction
  313217,   # Atrial fibrillation  
  316866,   # Hypertensive disease
  4329847   # Heart failure
)

# Calculate comprehensive CVD risk scores
cvd_scores <- calculate_pheprob(
  concept_ids = cvd_concepts,
  method = "composite",
  domains = c("condition", "procedure", "drug"),
  weights = c(
    "314866" = 3.0,   # MI weighted heavily
    "313217" = 2.0,   # AFib moderately weighted
    "316866" = 1.5,   # HTN baseline weight
    "4329847" = 2.5   # HF weighted heavily
  ),
  normalization = "minmax",
  output_file = "cvd_risk_scores.csv"
)

# Identify high-risk patients
high_risk_patients <- cvd_scores[cvd_scores$pheprob_score > 0.8, ]
```

### Mental Health Phenotyping

```r
# Depression and anxiety concepts
mental_health_concepts <- c(
  4152280,  # Major depressive disorder
  4226263,  # Anxiety disorder
  436073,   # Bipolar disorder
  4059317   # PTSD
)

mental_health_scores <- calculate_pheprob(
  concept_ids = mental_health_concepts,
  method = "temporal",  # Recent events more important
  date_range = list(start = as.Date("2019-01-01"), end = Sys.Date()),
  normalization = "zscore"
)
```

## Data Export and Integration

### Export Options

```r
# Export to CSV
calculate_pheprob(
  concept_ids = diabetes_concepts,
  output_file = "results.csv"
)

# Export to RDS with metadata
calculate_pheprob(
  concept_ids = diabetes_concepts,
  output_file = "results.rds"
)

# Manual export with custom options
scores <- calculate_pheprob(diabetes_concepts)
export_features(scores, "custom_results.csv", include_metadata = TRUE)
```

### Integration with Analysis Workflows

```r
# Calculate scores and integrate with analysis pipeline
scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "composite",
  output_format = "matrix"
)

# Use with machine learning
library(randomForest)
# scores matrix can be used directly as features
model <- randomForest(outcome ~ ., data = combined_data)

# Use with survival analysis  
library(survival)
cox_model <- coxph(Surv(time, event) ~ scores[, 1] + scores[, 2], data = survival_data)
```

## Performance and Scalability

### Batch Processing

For large datasets, the package automatically uses batch processing:

```r
# Process 100,000+ patients efficiently
large_scale_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  batch_size = 5000,     # Adjust based on memory
  progress = TRUE        # Monitor progress
)
```

### Memory Management

```r
# For very large analyses, process in chunks
person_chunks <- split(all_person_ids, ceiling(seq_along(all_person_ids) / 10000))

results <- list()
for (i in seq_along(person_chunks)) {
  chunk_scores <- calculate_pheprob(
    concept_ids = diabetes_concepts,
    person_ids = person_chunks[[i]],
    progress = FALSE
  )
  
  # Save intermediate results
  saveRDS(chunk_scores, paste0("chunk_", i, "_scores.rds"))
  results[[i]] <- chunk_scores
}

# Combine results
final_scores <- do.call(rbind, results)
```

## Error Handling and Troubleshooting

### Common Issues

1. **Database Connection Errors**
```r
# Ensure you're connected to All of Us
if (!allofus::aou_ls_workspaces()) {
  stop("Not connected to All of Us workspace")
}
```

2. **Invalid Concept IDs**
```r
# Always validate concept IDs first
validation <- validate_concept_ids(your_concepts)
if (validation$summary$valid_concept_ids == 0) {
  stop("No valid concept IDs found")
}
```

3. **Memory Issues with Large Datasets**
```r
# Reduce batch size for memory-constrained environments
scores <- calculate_pheprob(
  concept_ids = your_concepts,
  batch_size = 1000  # Smaller batches
)
```

## Best Practices

### Concept Selection

1. **Use validated concept sets** when available
2. **Include both specific and general concepts** for comprehensive phenotyping
3. **Consider concept hierarchies** in OMOP vocabulary

```r
# Example: Include both specific and general diabetes concepts
diabetes_concepts <- c(
  201826,   # Type 2 diabetes mellitus (specific)
  4329847,  # Diabetes mellitus (general)
  9201,     # Diabetes complication (related)
  4229881   # Diabetic nephropathy (specific complication)
)
```

### Weight Assignment

1. **Higher weights for more specific concepts**
2. **Consider clinical importance**
3. **Validate weights with domain experts**

```r
# Evidence-based weighting
weights <- c(
  "201826" = 2.0,   # Specific diagnosis
  "4329847" = 1.0,  # General diagnosis  
  "9201" = 1.5,     # Complication
  "4229881" = 2.5   # Specific severe complication
)
```

### Temporal Considerations

```r
# Focus on recent events for active conditions
recent_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "temporal",
  date_range = list(start = as.Date("2020-01-01"), end = Sys.Date())
)

# Use full history for lifetime conditions
lifetime_scores <- calculate_pheprob(
  concept_ids = diabetes_concepts,
  method = "composite"
  # No date_range = use all available data
)
```

## Citation

If you use `pheprobAoU` in your research, please cite:

```
[Your Name] et al. (2024). pheprobAoU: Calculate PheProb using All of Us EHR Data. 
R package version 0.1.0. https://github.com/yourusername/pheprobAoU
```

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup

```r
# Clone the repository
git clone https://github.com/yourusername/pheprobAoU.git
cd pheprobAoU

# Install development dependencies
devtools::install_dev_deps()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## License

This package is licensed under the MIT License. See [LICENSE](LICENSE) file for details.

## Support

- **Documentation**: Full function documentation available via `?function_name`
- **Issues**: Report bugs and request features on [GitHub Issues](https://github.com/yourusername/pheprobAoU/issues)
- **Discussions**: Ask questions on [GitHub Discussions](https://github.com/yourusername/pheprobAoU/discussions)

## Acknowledgments

- All of Us Research Program participants and staff
- OMOP Common Data Model community
- R package development community

---

**Note**: This package requires appropriate data use agreements and ethical approvals for use with All of Us Research Program data. Ensure compliance with all applicable regulations and institutional policies before use.
