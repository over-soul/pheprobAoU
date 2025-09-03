#' Calculate PheProb (Phenotype Probabilities) using All of Us EHR Data
#'
#' Calculates phenotype probabilities using the PheProb binomial mixture model.
#' PheProb models disease-relevant billing codes as a subset of total healthcare 
#' utilization to provide true phenotype probabilities P(Y=1|S,C) where S is 
#' relevant code count and C is total healthcare utilization.
#'
#' @param concept_ids A numeric vector of OMOP concept IDs that define the 
#'   phenotype of interest.
#' @param person_ids A numeric vector of person IDs to include in the analysis.
#'   If NULL (default), includes all available persons in the database.
#' @param domains Character vector specifying which OMOP domains to search.
#'   Default: c("condition", "procedure", "drug", "measurement", "observation")
#' @param date_range Optional list with 'start' and 'end' dates (Date objects)
#'   to limit the temporal scope of data extraction.
#' @param output_format Character string specifying output format:
#'   \itemize{
#'     \item "wide": Returns a wide-format tibble (default)
#'     \item "long": Returns a long-format tibble with person_id, score details
#'     \item "matrix": Returns a matrix with persons as rows, scores as columns
#'   }
#' @param output_file Optional file path to save results. Format determined by
#'   file extension (.csv, .rds).
#' @param batch_size Integer specifying batch size for processing large datasets
#'   (default: 10000). Larger batches are faster but use more memory.
#' @param progress Logical indicating whether to show progress bars (default: TRUE)
#' @param max_iterations Maximum EM iterations for binomial mixture model (default: 1000)
#' @param convergence_threshold Convergence threshold for EM algorithm (default: 1e-6)
#' @param init_method Initialization method for EM: "random", "kmeans", "manual" (default: "random")
#' @param exclude_concepts Concept IDs to exclude from total healthcare utilization counts
#' @param data_validation Logical indicating whether to perform data quality validation (default: TRUE)
#' @param model_diagnostics Logical indicating whether to include model diagnostics (default: TRUE)
#' @param check_concept_existence Logical indicating whether to validate concept IDs exist in database (default: FALSE)
#' @param ... Additional arguments
#'
#' @return A tibble with class "pheprob_results" containing:
#'   \item{person_id}{Person identifier}
#'   \item{pheprob_score}{Phenotype probability P(Y=1|S,C)}
#'   \item{total_codes}{Total healthcare code count (C)}
#'   \item{relevant_codes}{Disease-relevant code count (S)}
#'   \item{success_rate}{S/C ratio}
#'   \item{case_probability}{Posterior probability of being a case}
#'   \item{control_probability}{Posterior probability of being a control}
#'   Plus model metadata and diagnostics as attributes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic diabetes phenotyping using condition concepts
#' diabetes_concepts <- c(201820, 201826, 4193704)  # Diabetes concepts
#' 
#' # Calculate phenotype probabilities
#' diabetes_scores <- calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   progress = TRUE
#' )
#' 
#' # With date range filtering
#' recent_diabetes <- calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   date_range = list(start = as.Date("2020-01-01"), end = Sys.Date())
#' )
#' 
#' # Export results to CSV
#' calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   output_file = "diabetes_pheprob_scores.csv"
#' )
#' }
calculate_pheprob <- function(concept_ids,
                             person_ids = NULL,
                             domains = c("condition", "procedure", "drug", "measurement", "observation"),
                             date_range = NULL,
                             output_format = "wide",
                             output_file = NULL,
                             batch_size = 10000,
                             progress = TRUE,
                             max_iterations = 1000,
                             convergence_threshold = 1e-6,
                             init_method = "random",
                             exclude_concepts = NULL,
                             data_validation = TRUE,
                             model_diagnostics = TRUE,
                             check_concept_existence = FALSE,
                             ...) {
  
  # Call the binomial mixture model implementation directly
  return(calculate_pheprob_method(
    concept_ids = concept_ids,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range,
    output_format = output_format,
    output_file = output_file,
    batch_size = batch_size,
    progress = progress,
    max_iterations = max_iterations,
    convergence_threshold = convergence_threshold,
    init_method = init_method,
    exclude_concepts = exclude_concepts,
    data_validation = data_validation,
    model_diagnostics = model_diagnostics,
    check_concept_existence = check_concept_existence,
    ...
  ))
}


#' Calculate Multiple PheProbs
#'
#' Calculates separate PheProb scores for multiple related phenotypes using
#' binomial mixture models. Each phenotype gets its own probability score.
#'
#' @param phenotype_concepts A named list where each element contains concept IDs
#'   for a specific phenotype. Names will be used as column names in the output.
#' @param person_ids A numeric vector of person IDs to include in the analysis.
#'   If NULL (default), includes all available persons in the database.
#' @param domains Character vector specifying which OMOP domains to search.
#'   Default: c("condition", "procedure", "drug", "measurement", "observation")
#' @param date_range Optional list with 'start' and 'end' dates (Date objects)
#'   to limit the temporal scope of data extraction.
#' @param output_format Character string specifying output format:
#'   \itemize{
#'     \item "wide": Returns wide-format tibble with separate columns per phenotype (default)
#'     \item "long": Returns long-format tibble with phenotype_name column
#'   }
#' @param output_file Optional file path to save results.
#' @param batch_size Integer specifying batch size for processing large datasets
#' @param progress Logical indicating whether to show progress bars (default: TRUE)
#' @param max_iterations Maximum EM iterations for binomial mixture model
#' @param convergence_threshold Convergence threshold for EM algorithm
#' @param init_method Initialization method for EM: "random", "kmeans", "manual"
#' @param exclude_concepts Concept IDs to exclude from total healthcare utilization counts
#' @param data_validation Logical indicating whether to perform data quality validation
#' @param model_diagnostics Logical indicating whether to include model diagnostics
#' @param phenotype_correlation_analysis Logical indicating whether to analyze correlations
#' @param joint_validation Logical indicating whether to perform joint validation
#' @param ... Additional arguments
#'
#' @return Depending on output_format:
#'   \item{wide}{Tibble with person_id and one probability column per phenotype}
#'   \item{long}{Tibble with person_id, phenotype_name, pheprob_score columns}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define multiple phenotypes
#' phenotypes <- list(
#'   diabetes = c(201820, 201826, 4193704),
#'   cvd = c(4329847, 313217, 442604, 316139),
#'   depression = c(4152280, 442077)
#' )
#' 
#' # Calculate separate probabilities for each phenotype
#' multi_scores <- calculate_multiple_pheprobs(
#'   phenotype_concepts = phenotypes
#' )
#' }
calculate_multiple_pheprobs <- function(phenotype_concepts,
                                       person_ids = NULL,
                                       domains = c("condition", "procedure", "drug", "measurement", "observation"),
                                       date_range = NULL,
                                       output_format = "wide",
                                       output_file = NULL,
                                       batch_size = 10000,
                                       progress = TRUE,
                                       max_iterations = 1000,
                                       convergence_threshold = 1e-6,
                                       init_method = "random",
                                       exclude_concepts = NULL,
                                       data_validation = TRUE,
                                       model_diagnostics = TRUE,
                                       phenotype_correlation_analysis = TRUE,
                                       joint_validation = TRUE,
                                       ...) {
  
  # Use the implementation from multiple_phenotypes_impl.R
  return(calculate_multiple_pheprobs_method(
    phenotype_concepts = phenotype_concepts,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range,
    output_format = output_format,
    output_file = output_file,
    batch_size = batch_size,
    progress = progress,
    max_iterations = max_iterations,
    convergence_threshold = convergence_threshold,
    init_method = init_method,
    exclude_concepts = exclude_concepts,
    data_validation = data_validation,
    model_diagnostics = model_diagnostics,
    phenotype_correlation_analysis = phenotype_correlation_analysis,
    joint_validation = joint_validation,
    ...
  ))
}
