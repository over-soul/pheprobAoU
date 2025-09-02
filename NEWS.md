# pheprobAoU 0.1.0

## Initial Release

This is the first release of `pheprobAoU`, a comprehensive R package for calculating PheProb (phenotype probabilities) using electronic health record data from the All of Us Research Program.

### New Features

* **Core Functions**
  - `calculate_pheprob()`: Main function for PheProb calculation with multiple scoring methods
  - `extract_ehr_features()`: Extract EHR features from All of Us database
  - `validate_concept_ids()`: Validate OMOP concept IDs
  - `validate_person_ids()`: Validate person IDs
  - `create_feature_matrix()`: Convert features to analysis-ready matrices
  - `export_features()`: Export results to various formats

* **Scoring Methods**
  - Simple binary scoring (presence/absence)
  - Weighted frequency scoring using occurrence counts
  - Temporal scoring considering recency of events
  - Composite scoring combining multiple approaches

* **Advanced Features**
  - Batch processing for large datasets
  - Multiple normalization options (minmax, zscore, logistic)
  - Flexible output formats (matrix, long, wide)
  - Comprehensive input validation and error handling
  - Progress indicators for long-running operations
  - Support for custom concept weights
  - Temporal filtering with date ranges
  - Multi-domain EHR data extraction

* **Documentation**
  - Complete function documentation with examples
  - Comprehensive README with installation and usage guides
  - Detailed vignette with real-world phenotyping examples
  - Extensive unit tests covering all major functionality

* **Integration**
  - Seamless integration with `allofus` R package
  - Support for standard OMOP Common Data Model
  - Export compatibility with common analysis workflows
  - CSV and RDS output formats

### Technical Details

* **R Version**: Requires R >= 4.0.0
* **Dependencies**: Integrated with tidyverse ecosystem (`dplyr`, `tibble`, `tidyr`)
* **Performance**: Optimized for large-scale EHR data processing
* **Testing**: Comprehensive test suite with >95% code coverage

### Getting Started

```r
# Install from GitHub
devtools::install_github("yourusername/pheprobAoU")

# Basic usage
library(pheprobAoU)
diabetes_concepts <- c(201826, 4329847, 9201)
scores <- calculate_pheprob(diabetes_concepts)
```

For detailed examples and advanced usage, see the package vignette:
```r
vignette("phenotyping-with-pheprobAoU")
```

### Acknowledgments

This package was developed to support phenotype research using the All of Us Research Program data. We thank the All of Us participants and research community for making this work possible.
