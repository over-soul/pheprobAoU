# Implementation Summary: Multiple Phenotypes Mode

## Problem Addressed

The user identified a critical design flaw: **`pheprob_score` only makes sense when all concept IDs relate to the same phenotype**. When unrelated concept IDs are mixed (e.g., diabetes + depression + heart disease), the resulting single probability score is meaningless.

## Solution Implemented: Option 1 - Multiple Phenotypes Mode

### New Functions Added

1. **`calculate_multiple_pheprobs()`** - Main function for analyzing multiple separate phenotypes
2. **`validate_phenotype_coherence()`** - Validates that phenotype definitions are clinically coherent

### Key Features

#### Input Structure
```r
# Define separate phenotypes instead of mixing concepts
phenotypes <- list(
  diabetes = c(201826, 4329847, 9201),
  cardiovascular = c(314866, 313217, 316866),
  mental_health = c(4152280, 4226263, 436073),
  chronic_kidney = c(4030518, 192359, 4030319)
)
```

#### Output Formats

**Wide Format (default):**
```r
# person_id diabetes_prob cardiovascular_prob mental_health_prob chronic_kidney_prob
#    123456          0.89                0.45               0.12                0.23
#    234567          0.23                0.91               0.67                0.08
```

**Long Format:**
```r
# person_id phenotype_name  pheprob_score
#    123456 diabetes                 0.89
#    123456 cardiovascular           0.45
#    123456 mental_health            0.12
```

#### Advanced Features

1. **Custom Weights per Phenotype:**
```r
phenotype_weights <- list(
  diabetes = c("201826" = 2.0, "4329847" = 1.5),
  cardiovascular = c("314866" = 3.0, "313217" = 2.0)
)
```

2. **Comprehensive Validation:**
- Phenotype name validation (no spaces, special characters)
- Domain coherence checking
- Automatic warnings for mixed concepts

3. **Robust Error Handling:**
- Individual phenotype failures don't crash entire analysis
- Informative progress indicators
- Detailed summary statistics

### Clinical Benefits

#### Before (Problematic)
❌ `pheprob_score = 0.73` for mixed concepts - **meaningless**
❌ Cannot interpret what the score represents
❌ No way to analyze specific conditions
❌ Poor clinical utility

#### After (Solution)
✅ `diabetes_prob = 0.89` - **89% probability of diabetes**
✅ `cardiovascular_prob = 0.45` - **45% probability of CVD**
✅ Each score is clinically interpretable
✅ Enables comorbidity analysis
✅ Supports personalized risk stratification
✅ High clinical utility

### Research Applications

1. **Comorbidity Analysis:**
```r
comorbidity_patterns <- multi_scores %>%
  mutate(
    high_diabetes = diabetes_prob > 0.8,
    high_cvd = cardiovascular_prob > 0.8,
    diabetes_cvd_comorbid = high_diabetes & high_cvd
  )
```

2. **Population Stratification:**
```r
risk_strata <- multi_scores %>%
  mutate(
    overall_risk = case_when(
      diabetes_prob > 0.8 | cardiovascular_prob > 0.8 ~ "High Risk",
      diabetes_prob > 0.5 | cardiovascular_prob > 0.5 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    )
  )
```

3. **Phenotype-Specific Analysis:**
- Each phenotype can be analyzed independently
- Different scoring methods per phenotype
- Custom weights based on clinical evidence

## Files Modified/Added

### Core Implementation
- **`R/calculate_pheprob.R`** - Added ~400 lines for multiple phenotypes functions
- **`NAMESPACE`** - Added exports and imports for new functions

### Testing
- **`tests/testthat/test-multiple-phenotypes.R`** - Comprehensive tests for new functionality

### Documentation
- **`vignettes/phenotyping-with-pheprobAoU.Rmd`** - Added extensive multiple phenotypes section
- **`README.md`** - Updated with new function examples and best practices

### Demonstration
- **`demo_multiple_phenotypes.R`** - Complete working demo showing output

## Backward Compatibility

✅ **Fully backward compatible** - All existing `calculate_pheprob()` functionality unchanged
✅ **Additive enhancement** - New functions supplement rather than replace existing ones
✅ **Clear migration path** - Users can gradually adopt multiple phenotypes mode

## Best Practices Established

### ✅ DO: Use Multiple Phenotypes Mode
```r
phenotypes <- list(
  diabetes = c(201826, 4329847, 9201),
  cvd = c(314866, 313217, 316866)
)
multi_scores <- calculate_multiple_pheprobs(phenotypes)
```

### ❌ DON'T: Mix Unrelated Concepts
```r
# Avoid this - meaningless result
mixed_concepts <- c(201826, 313217, 432870, 4030518)
calculate_pheprob(mixed_concepts)  # pheprob_score is uninterpretable
```

## Impact

This implementation **solves a fundamental conceptual problem** in the original package design and provides:

1. **Clinical Meaningfulness** - Each probability score is interpretable
2. **Research Utility** - Enables sophisticated comorbidity and risk analyses
3. **Methodological Rigor** - Prevents misuse of mixing unrelated concepts
4. **Practical Flexibility** - Supports diverse research questions and use cases

The multiple phenotypes mode transforms pheprobAoU from a tool that could produce misleading results to one that provides clinically meaningful, interpretable phenotype probabilities suitable for high-quality research.
