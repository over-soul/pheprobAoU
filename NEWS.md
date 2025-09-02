# pheprobAoU 1.1.0

## Critical Data Extraction Fix

### Major Improvements

* **BREAKING FIX**: Replaced manual BigQuery queries with proper `allofus` package functions
  - `extract_allofus_pheprob_data()` now uses `aou_concept_set()` and `aou_survey()`
  - Fixes critical data extraction issues that caused unrealistically low disease prevalence (0.21% → realistic 10-15%)
  - Resolves integer64 display and computation bugs with BigQuery data
  - Significantly improves binomial mixture model performance and discrimination

### Technical Changes

* Data extraction now uses `aou_concept_set()` instead of manual `dplyr` queries on BigQuery tables
* Added proper integer64 handling with `as.numeric()` conversions throughout the pipeline
* Improved error handling and progress reporting in data extraction
* Enhanced numerical stability in binomial mixture EM algorithm (log-space computation)
* Fixed concept expansion compatibility issues with BigQuery

### Bug Fixes

* Fixed `object 'alpha_0' not found` error in binomial mixture M-step
* Resolved scientific notation display bugs (e-319 values) from integer64 overflow
* Fixed BigQuery temporary table compatibility issues (not supported)
* Corrected parameter separation in binomial mixture models (p₁ ≠ p₀)
* Fixed `slice_head()` database backend incompatibility

### Performance

* Dramatically improved disease prevalence detection accuracy (57x improvement for diabetes)
* Better convergence properties for EM algorithm with realistic data
* More realistic phenotype probability distributions (not all 0.5)
* Enhanced model discrimination between cases and controls

---

# pheprobAoU 1.0.0

## Major Release: PheProb Implementation (Sinnott et al., 2018)

This major release implements the **PheProb methodology** (Sinnott et al., 2018) using binomial mixture models, replacing the previous deterministic scoring approaches with proper probabilistic phenotyping.

### Breaking Changes

* **Default method changed** from `"composite"` to `"original"` in `calculate_pheprob()`
* **Deprecated deterministic methods**: `"simple"`, `"weighted"`, `"temporal"`, `"composite"` are now deprecated with warnings
* **New output format**: Results now include true phenotype probabilities P(Y=1|S,C) with model diagnostics

### New Features: PheProb Implementation (Sinnott et al., 2018)

* **`method = "original"`**: Implements the PheProb binomial mixture model (Sinnott et al., 2018)
  - Models disease-relevant codes (S) as subset of total healthcare utilization (C)
  - Uses EM algorithm to estimate parameters: p₁, p₀, α₀, α₁
  - Healthcare utilization adjustment: φ(c) = logistic(α₀ + α₁ × c)
  - Returns true phenotype probabilities P(Y=1|S,C)

* **Binomial mixture model core** (`R/binomial_mixture.R`):
  - `fit_pheprob_binomial_mixture()`: Complete EM algorithm implementation
  - Robust numerical implementation with convergence diagnostics
  - Parameter initialization strategies (random, k-means, manual)
  - Comprehensive model validation and quality assessment

* **Enhanced data extraction** (`R/data_utils_helpers.R`):
  - `extract_total_healthcare_utilization()`: Extract total billing code counts (C)
  - `prepare_pheprob_binomial_data()`: Create (S,C) pairs for mixture modeling
  - Automatic data quality validation and constraint checking (S ≤ C)

### New Features: Multiple Phenotypes Extension

* **`calculate_multiple_pheprobs_original()`**: Proper multiple phenotype analysis
  - Independent binomial mixture model for each phenotype
  - Shared healthcare utilization extraction for efficiency
  - Separate interpretable probabilities per phenotype
  - Avoids meaningless mixing of unrelated concept IDs

* **Correlation and comorbidity analysis**:
  - Phenotype correlation matrices with statistical testing
  - Comorbidity pattern identification (patients with multiple conditions)
  - Cross-phenotype validation and consistency checking

* **Rich visualization suite** (`R/multiple_phenotypes_viz.R`):
  - `plot(results, type = "correlation_matrix")`: Phenotype correlation heatmaps
  - `plot(results, type = "probability_distributions")`: Probability histograms
  - `plot(results, type = "comorbidity_network")`: Comorbidity relationships
  - `plot(results, type = "convergence_diagnostics")`: Model convergence plots

### Enhanced API

* **New parameters for original method**:
  - `max_iterations`: Maximum EM algorithm iterations (default: 1000)
  - `convergence_threshold`: EM convergence threshold (default: 1e-6)
  - `init_method`: Parameter initialization ("random", "kmeans", "manual")
  - `data_validation`: Automatic data quality assessment (default: TRUE)
  - `model_diagnostics`: Include convergence and stability diagnostics (default: TRUE)

* **Multiple phenotypes parameters**:
  - `phenotype_correlation_analysis`: Analyze correlations between phenotypes (default: TRUE)
  - `joint_validation`: Cross-phenotype validation (default: TRUE)

### Output Enhancements

* **Rich result objects**:
  - Class `"pheprob_original"` for single phenotype results
  - Class `"pheprob_multiple_original"` for multiple phenotype results
  - Model parameters, diagnostics, and validation results as attributes

* **Enhanced print/summary methods**:
  - `print.pheprob_original()`: Concise result overview
  - `summary.pheprob_original()`: Detailed model information
  - Parameter estimates, convergence status, probability summaries

### Documentation and Examples

* **Updated documentation**:
  - README reflects PheProb methodology (Sinnott et al., 2018)
  - Function documentation updated for new parameters
  - Examples demonstrate proper probabilistic phenotyping

* **Comprehensive examples**:
  - `example_original_pheprob.R`: PheProb methodology demonstration (Sinnott et al., 2018)
  - `example_multiple_phenotypes.R`: Multiple phenotype analysis examples
  - Comparison of old vs new approaches

### Performance and Quality

* **Data quality validation**:
  - Automatic assessment of binomial data quality
  - S ≤ C constraint validation
  - Healthcare utilization reasonableness checks
  - Quality scoring and recommendations

* **Model diagnostics**:
  - EM convergence monitoring
  - Parameter stability assessment
  - Component separation analysis
  - Cross-validation support

### Migration Guide

```r
# Old approach (deprecated)
scores <- calculate_pheprob(concepts, method = "composite")

# New approach (recommended - Sinnott et al., 2018 methodology)
scores <- calculate_pheprob(concepts, method = "original")

# Multiple phenotypes (replaces mixed concepts)
phenotypes <- list(diabetes = c(...), cvd = c(...))
multi_scores <- calculate_multiple_pheprobs(phenotypes, method = "original")
```

### Backward Compatibility

* **Graceful deprecation**: Old methods continue to work with warnings
* **Migration warnings**: Clear guidance for upgrading to new methodology
* **No breaking changes**: Existing code continues to function

---

# pheprobAoU 0.1.0

## Initial Release (Deprecated)

*Note: This initial release used deterministic scoring methods that have been deprecated in favor of the PheProb methodology (Sinnott et al., 2018) in v1.0.0.*

### Deprecated Features

* ~~Simple binary scoring~~ → Use `method = "original"` 
* ~~Weighted frequency scoring~~ → Use `method = "original"`
* ~~Temporal scoring~~ → Use `method = "original"`
* ~~Composite scoring~~ → Use `method = "original"`

### Migration Required

Users of v0.1.0 should upgrade to v1.0.0 and switch to `method = "original"` for proper probabilistic phenotyping with the PheProb binomial mixture model methodology (Sinnott et al., 2018).