#' Binomial Mixture Model for PheProb (Sinnott et al., 2018)
#'
#' This file implements the PheProb methodology using binomial mixture models
#' as described in Sinnott et al. (2018). The model assumes 2 latent classes (cases and controls)
#' and models disease-relevant code counts as binomial draws from total healthcare utilization.
#'
#' @name binomial_mixture
NULL

#' Fit Binomial Mixture Model for PheProb
#'
#' Implements the original PheProb algorithm using a binomial mixture model with
#' healthcare utilization adjustment. The model assumes 2 latent classes (cases Y=1, 
#' controls Y=0) where disease-relevant code counts S follow binomial distributions
#' conditional on total code counts C.
#'
#' @param S Integer vector of disease-relevant billing code counts
#' @param C Integer vector of total billing code counts (must be >= S)
#' @param max_iterations Maximum number of EM iterations (default: 1000)
#' @param convergence_threshold Convergence threshold for log-likelihood (default: 1e-6)
#' @param verbose Logical indicating whether to print progress (default: TRUE)
#' @param init_method Initialization method: "random", "kmeans", or "manual" (default: "random")
#' @param regularization Small value added to prevent numerical issues (default: 1e-8)
#'
#' @return A list containing:
#'   \item{parameters}{List with p_1, p_0, alpha_0, alpha_1}
#'   \item{phenotype_probabilities}{Vector of P(Y=1|S,C) for each patient}
#'   \item{posterior_probabilities}{Matrix of component probabilities}
#'   \item{log_likelihood}{Final log-likelihood}
#'   \item{log_likelihood_history}{Vector of log-likelihood values by iteration}
#'   \item{n_iterations}{Number of iterations until convergence}
#'   \item{convergence}{Logical indicating convergence}
#'   \item{model_diagnostics}{List with convergence diagnostics}
#'
#' @details
#' The model specification is:
#' \deqn{P(S = s | C = c, Y = y) = \binom{c}{s} p_y^s (1-p_y)^{c-s}}
#' \deqn{\phi(c) = \text{logistic}(\alpha_0 + \alpha_1 \cdot c)}
#' \deqn{P(Y = 1 | S = s, C = c) = \frac{\phi(c) \cdot P(S|C,Y=1)}{\phi(c) \cdot P(S|C,Y=1) + (1-\phi(c)) \cdot P(S|C,Y=0)}}
#'
#' @examples
#' \dontrun{
#' # Simulate data
#' n <- 1000
#' C <- rpois(n, lambda = 20) + 1  # Total codes
#' true_status <- rbinom(n, 1, 0.3)  # True case status
#' S <- rbinom(n, C, ifelse(true_status == 1, 0.4, 0.1))  # Relevant codes
#' 
#' # Fit model
#' model <- fit_pheprob_binomial_mixture(S, C)
#' 
#' # Extract probabilities
#' probs <- model$phenotype_probabilities
#' }
#'
#' @export
fit_pheprob_binomial_mixture <- function(S, 
                                        C,
                                        max_iterations = 1000,
                                        convergence_threshold = 1e-6,
                                        verbose = TRUE,
                                        init_method = "random",
                                        regularization = 1e-8) {
  
  # Input validation
  validate_binomial_mixture_inputs(S, C)
  
  n <- length(S)
  
  if (verbose) {
    cli::cli_alert_info("Fitting binomial mixture model with {n} patients")
    cli::cli_alert_info("Total codes range: {min(C)} - {max(C)}")
    cli::cli_alert_info("Relevant codes range: {min(S)} - {max(S)}")
  }
  
  # Initialize parameters
  init_params <- initialize_binomial_mixture_parameters(S, C, method = init_method)
  p_1 <- init_params$p_1
  p_0 <- init_params$p_0
  alpha_0 <- init_params$alpha_0
  alpha_1 <- init_params$alpha_1
  
  # Storage for iteration history
  log_likelihood_history <- numeric(max_iterations)
  parameter_history <- list()
  
  # Start EM iterations
  if (verbose) {
    cli::cli_progress_bar("EM iterations", total = max_iterations)
  }
  
  for (iter in 1:max_iterations) {
    
    # E-step: Calculate posterior probabilities
    e_step_result <- binomial_mixture_e_step(S, C, p_1, p_0, alpha_0, alpha_1)
    gamma <- e_step_result$posterior_probabilities
    log_likelihood <- e_step_result$log_likelihood
    
    # Store history
    log_likelihood_history[iter] <- log_likelihood
    parameter_history[[iter]] <- list(p_1 = p_1, p_0 = p_0, alpha_0 = alpha_0, alpha_1 = alpha_1)
    
    # M-step: Update parameters
    m_step_result <- binomial_mixture_m_step(S, C, gamma, regularization)
    p_1 <- m_step_result$p_1
    p_0 <- m_step_result$p_0
    alpha_0 <- m_step_result$alpha_0
    alpha_1 <- m_step_result$alpha_1
    
    # Check convergence
    if (iter > 1) {
      improvement <- log_likelihood_history[iter] - log_likelihood_history[iter-1]
      
      if (abs(improvement) < convergence_threshold) {
        if (verbose) {
          cli::cli_progress_done()
          cli::cli_alert_success("EM converged at iteration {iter}")
        }
        converged <- TRUE
        break
      }
      
      # Check for likelihood decrease (numerical issues)
      if (improvement < -1e-6) {
        cli::cli_alert_warning("Log-likelihood decreased at iteration {iter} (improvement: {round(improvement, 8)})")
      }
    }
    
    if (verbose && iter %% 10 == 0) {
      cli::cli_progress_update(set = iter)
    }
    
    converged <- FALSE
  }
  
  if (!converged && verbose) {
    cli::cli_progress_done()
    cli::cli_alert_warning("EM did not converge within {max_iterations} iterations")
  }
  
  # Final E-step for final probabilities
  final_e_step <- binomial_mixture_e_step(S, C, p_1, p_0, alpha_0, alpha_1)
  
  # Model diagnostics
  diagnostics <- calculate_binomial_mixture_diagnostics(
    S, C, gamma, p_1, p_0, alpha_0, alpha_1, log_likelihood_history[1:iter]
  )
  
  # Return results
  result <- list(
    parameters = list(
      p_1 = p_1,
      p_0 = p_0,
      alpha_0 = alpha_0,
      alpha_1 = alpha_1
    ),
    phenotype_probabilities = final_e_step$phenotype_probabilities,
    posterior_probabilities = final_e_step$posterior_probabilities,
    log_likelihood = log_likelihood,
    log_likelihood_history = log_likelihood_history[1:iter],
    n_iterations = iter,
    convergence = converged,
    model_diagnostics = diagnostics,
    parameter_history = parameter_history[1:iter]
  )
  
  class(result) <- "pheprob_binomial_mixture"
  
  if (verbose) {
    print_binomial_mixture_summary(result)
  }
  
  return(result)
}

#' E-step for Binomial Mixture Model
#'
#' Calculates posterior probabilities and log-likelihood for the E-step
#' of the EM algorithm in the binomial mixture model.
#'
#' @param S Vector of disease-relevant code counts
#' @param C Vector of total code counts
#' @param p_1 Success probability for cases
#' @param p_0 Success probability for controls
#' @param alpha_0 Intercept for healthcare utilization effect
#' @param alpha_1 Slope for healthcare utilization effect
#'
#' @return List with posterior_probabilities, phenotype_probabilities, log_likelihood
#'
#' @keywords internal
binomial_mixture_e_step <- function(S, C, p_1, p_0, alpha_0, alpha_1) {
  
  n <- length(S)
  
  # Calculate prior probabilities (healthcare utilization effect)
  phi <- plogis(alpha_0 + alpha_1 * C)  # P(Y = 1 | C)
  
  # Calculate log binomial likelihoods to prevent overflow
  # Add small regularization to prevent log(0)
  log_lik_case <- dbinom(S, size = C, prob = pmax(p_1, 1e-10), log = TRUE)
  log_lik_control <- dbinom(S, size = C, prob = pmax(p_0, 1e-10), log = TRUE)
  
  # Calculate log weighted likelihoods
  log_weighted_lik_case <- log(pmax(phi, 1e-300)) + log_lik_case
  log_weighted_lik_control <- log(pmax(1 - phi, 1e-300)) + log_lik_control
  
  # Use log-sum-exp trick for numerical stability
  max_log_lik <- pmax(log_weighted_lik_case, log_weighted_lik_control)
  log_marginal_likelihood <- max_log_lik + log(
    exp(log_weighted_lik_case - max_log_lik) + 
    exp(log_weighted_lik_control - max_log_lik)
  )
  
  # Posterior probabilities P(Y = y | S, C) - the "responsibilities"
  gamma_case <- exp(log_weighted_lik_case - log_marginal_likelihood)  # P(Y = 1 | S, C)
  gamma_control <- exp(log_weighted_lik_control - log_marginal_likelihood)  # P(Y = 0 | S, C)
  
  # Handle any remaining numerical issues
  gamma_case[is.nan(gamma_case) | is.infinite(gamma_case)] <- 0.5
  gamma_control[is.nan(gamma_control) | is.infinite(gamma_control)] <- 0.5
  
  # Ensure probabilities sum to 1
  total_gamma <- gamma_case + gamma_control
  gamma_case <- gamma_case / total_gamma
  gamma_control <- gamma_control / total_gamma
  
  # Log-likelihood
  log_likelihood <- sum(log_marginal_likelihood)
  
  return(list(
    posterior_probabilities = cbind(control = gamma_control, case = gamma_case),
    phenotype_probabilities = gamma_case,  # This is what we want: P(Y = 1 | S, C)
    log_likelihood = log_likelihood,
    component_log_likelihoods = list(case = log_lik_case, control = log_lik_control),
    prior_probabilities = phi
  ))
}

#' M-step for Binomial Mixture Model
#'
#' Updates parameters in the M-step of the EM algorithm for the binomial mixture model.
#'
#' @param S Vector of disease-relevant code counts
#' @param C Vector of total code counts  
#' @param gamma Matrix of posterior probabilities from E-step
#' @param regularization Small value for numerical stability
#'
#' @return List with updated parameters p_1, p_0, alpha_0, alpha_1
#'
#' @keywords internal
binomial_mixture_m_step <- function(S, C, gamma, regularization = 1e-8) {
  
  # Extract responsibilities
  gamma_case <- gamma[, "case"]
  gamma_control <- gamma[, "control"]
  
  # Update p_1 (success probability for cases)
  # Weighted average of success rates
  weighted_successes_case <- sum(gamma_case * S)
  weighted_trials_case <- sum(gamma_case * C)
  p_1 <- weighted_successes_case / (weighted_trials_case + regularization)
  
  # Update p_0 (success probability for controls)
  weighted_successes_control <- sum(gamma_control * S)
  weighted_trials_control <- sum(gamma_control * C)
  p_0 <- weighted_successes_control / (weighted_trials_control + regularization)
  
  # Ensure probabilities are in valid range
  p_1 <- pmax(pmin(p_1, 1 - regularization), regularization)
  p_0 <- pmax(pmin(p_0, 1 - regularization), regularization)
  
  # Update alpha_0, alpha_1 using weighted logistic regression
  # phi(c) = logistic(alpha_0 + alpha_1 * c)
  # We want to fit: gamma_case ~ logistic(alpha_0 + alpha_1 * C)
  
  # Initialize variables to ensure they're always defined
  alpha_0 <- 0
  alpha_1 <- 0
  
  tryCatch({
    # Weighted logistic regression
    logit_fit <- glm(gamma_case ~ C, 
                    family = binomial(), 
                    weights = rep(1, length(C)),  # Could use gamma weights here
                    start = c(0, 0))
    
    alpha_0 <- as.numeric(coef(logit_fit)[1])
    alpha_1 <- as.numeric(coef(logit_fit)[2])
    
    # Handle NA coefficients
    if (is.na(alpha_0)) alpha_0 <- 0
    if (is.na(alpha_1)) alpha_1 <- 0
    
  }, error = function(e) {
    # Fallback: simple logistic regression without weights
    cli::cli_alert_warning("Weighted logistic regression failed, using simple approach")
    
    # Simple update based on correlation
    mean_gamma <- mean(gamma_case)
    alpha_0 <- qlogis(pmax(pmin(mean_gamma, 1 - regularization), regularization))
    
    # Simple correlation-based update for alpha_1
    cor_gamma_C <- cor(gamma_case, C)
    alpha_1 <- cor_gamma_C * 0.1  # Conservative scaling
  })
  
  return(list(
    p_1 = p_1,
    p_0 = p_0,
    alpha_0 = alpha_0,
    alpha_1 = alpha_1
  ))
}

#' Initialize Parameters for Binomial Mixture Model
#'
#' Provides sensible initial parameter values for the EM algorithm.
#'
#' @param S Vector of disease-relevant code counts
#' @param C Vector of total code counts
#' @param method Initialization method
#'
#' @return List with initial parameters
#'
#' @keywords internal
initialize_binomial_mixture_parameters <- function(S, C, method = "random") {
  
  n <- length(S)
  overall_rate <- sum(S) / sum(C)
  
  if (method == "random") {
    # Random initialization around empirical rate
    p_1 <- overall_rate + runif(1, 0.1, 0.3)  # Cases have higher rate
    p_0 <- overall_rate - runif(1, 0.05, 0.15)  # Controls have lower rate
    alpha_0 <- rnorm(1, 0, 0.5)
    alpha_1 <- rnorm(1, 0, 0.1)
    
  } else if (method == "kmeans") {
    # Use k-means on success rates to initialize
    success_rates <- S / pmax(C, 1)
    kmeans_result <- stats::kmeans(success_rates, centers = 2, nstart = 10)
    
    # Assign higher rate to cases, lower to controls
    centers <- sort(kmeans_result$centers)
    p_0 <- centers[1]
    p_1 <- centers[2]
    
    # Initialize alpha based on cluster assignments
    initial_labels <- kmeans_result$cluster - 1  # Convert to 0/1
    logit_init <- glm(initial_labels ~ C, family = binomial())
    alpha_0 <- as.numeric(coef(logit_init)[1])
    alpha_1 <- as.numeric(coef(logit_init)[2])
    
  } else if (method == "manual") {
    # Conservative manual initialization
    p_1 <- min(overall_rate * 2, 0.8)  # Cases: higher rate
    p_0 <- max(overall_rate * 0.5, 0.05)  # Controls: lower rate
    alpha_0 <- qlogis(0.3)  # 30% prior case probability
    alpha_1 <- 0  # No initial healthcare utilization effect
    
  } else {
    cli::cli_abort("Unknown initialization method: {method}")
  }
  
  # Ensure valid probability ranges
  p_1 <- pmax(pmin(p_1, 0.99), 0.01)
  p_0 <- pmax(pmin(p_0, 0.99), 0.01)
  
  # Ensure p_1 > p_0 (cases should have higher success rate)
  if (p_1 <= p_0) {
    temp <- p_1
    p_1 <- p_0 + 0.1
    p_0 <- temp
    p_1 <- pmin(p_1, 0.99)
  }
  
  return(list(
    p_1 = p_1,
    p_0 = p_0,
    alpha_0 = alpha_0,
    alpha_1 = alpha_1
  ))
}

#' Validate Inputs for Binomial Mixture Model
#'
#' Performs input validation for the binomial mixture model.
#'
#' @param S Vector of disease-relevant code counts
#' @param C Vector of total code counts
#'
#' @keywords internal
validate_binomial_mixture_inputs <- function(S, C) {
  
  # Check basic requirements
  if (!is.numeric(S) || !is.numeric(C)) {
    cli::cli_abort("S and C must be numeric vectors")
  }
  
  if (length(S) != length(C)) {
    cli::cli_abort("S and C must have the same length")
  }
  
  if (length(S) == 0) {
    cli::cli_abort("S and C cannot be empty")
  }
  
  # Check for non-negative integers
  if (any(S < 0) || any(C < 0)) {
    cli::cli_abort("S and C must be non-negative")
  }
  
  if (any(S != round(S)) || any(C != round(C))) {
    cli::cli_abort("S and C must be integers (code counts)")
  }
  
  # Check constraint: S <= C
  if (any(S > C)) {
    n_violations <- sum(S > C)
    cli::cli_abort("Disease-relevant codes (S) cannot exceed total codes (C). Found {n_violations} violations.")
  }
  
  # Check for sufficient variation
  if (var(S) == 0) {
    cli::cli_alert_warning("No variation in disease-relevant code counts")
  }
  
  if (var(C) == 0) {
    cli::cli_alert_warning("No variation in total code counts")
  }
  
  # Check for extreme values
  if (any(C == 0)) {
    n_zero <- sum(C == 0)
    cli::cli_alert_warning("Found {n_zero} patients with zero total codes")
  }
  
  # Data quality warnings
  mean_rate <- mean(S / pmax(C, 1))
  if (mean_rate < 0.01) {
    cli::cli_alert_warning("Very low disease-relevant code rate ({round(mean_rate, 4)}). Check concept definitions.")
  }
  
  if (mean_rate > 0.8) {
    cli::cli_alert_warning("Very high disease-relevant code rate ({round(mean_rate, 4)}). Check concept definitions.")
  }
}

#' Calculate Model Diagnostics for Binomial Mixture
#'
#' Computes various diagnostic metrics for the fitted binomial mixture model.
#'
#' @param S Vector of disease-relevant code counts
#' @param C Vector of total code counts
#' @param gamma Posterior probabilities matrix
#' @param p_1 Estimated success probability for cases
#' @param p_0 Estimated success probability for controls
#' @param alpha_0 Estimated intercept
#' @param alpha_1 Estimated healthcare utilization effect
#' @param ll_history Log-likelihood history
#'
#' @return List with diagnostic information
#'
#' @keywords internal
calculate_binomial_mixture_diagnostics <- function(S, C, gamma, p_1, p_0, alpha_0, alpha_1, ll_history) {
  
  n <- length(S)
  
  # Parameter separation
  param_separation <- abs(p_1 - p_0)
  
  # Component sizes
  case_proportion <- mean(gamma[, "case"])
  control_proportion <- mean(gamma[, "control"])
  
  # Convergence diagnostics
  if (length(ll_history) > 1) {
    ll_improvements <- diff(ll_history)
    final_improvement <- ll_improvements[length(ll_improvements)]
    convergence_rate <- mean(ll_improvements[ll_improvements > 0])
  } else {
    final_improvement <- NA
    convergence_rate <- NA
  }
  
  # Model fit diagnostics
  # Effective sample sizes for each component
  eff_n_case <- sum(gamma[, "case"])
  eff_n_control <- sum(gamma[, "control"])
  
  # Healthcare utilization effect
  phi <- plogis(alpha_0 + alpha_1 * C)
  utilization_effect_strength <- cor(phi, C)
  
  # Information criteria
  n_params <- 4  # p_1, p_0, alpha_0, alpha_1
  final_ll <- ll_history[length(ll_history)]
  AIC <- -2 * final_ll + 2 * n_params
  BIC <- -2 * final_ll + log(n) * n_params
  
  return(list(
    parameter_separation = param_separation,
    component_proportions = c(control = control_proportion, case = case_proportion),
    effective_sample_sizes = c(control = eff_n_control, case = eff_n_case),
    convergence_diagnostics = list(
      final_improvement = final_improvement,
      convergence_rate = convergence_rate,
      ll_history_length = length(ll_history)
    ),
    utilization_effect = list(
      alpha_0 = alpha_0,
      alpha_1 = alpha_1,
      correlation_phi_C = utilization_effect_strength
    ),
    model_selection = list(
      log_likelihood = final_ll,
      AIC = AIC,
      BIC = BIC,
      n_parameters = n_params
    )
  ))
}

#' Print Summary for Binomial Mixture Model
#'
#' Prints a summary of the fitted binomial mixture model results.
#'
#' @param model_result Result from fit_pheprob_binomial_mixture
#'
#' @keywords internal
print_binomial_mixture_summary <- function(model_result) {
  
  cli::cli_h2("Binomial Mixture Model Summary")
  
  # Parameters
  cli::cli_alert_info("Model Parameters:")
  cli::cli_text("  p_1 (cases): {round(model_result$parameters$p_1, 4)}")
  cli::cli_text("  p_0 (controls): {round(model_result$parameters$p_0, 4)}")
  cli::cli_text("  α_0 (intercept): {round(model_result$parameters$alpha_0, 4)}")
  cli::cli_text("  α_1 (utilization): {round(model_result$parameters$alpha_1, 4)}")
  
  # Component information
  diag <- model_result$model_diagnostics
  cli::cli_text("")
  cli::cli_alert_info("Component Information:")
  cli::cli_text("  Estimated case proportion: {round(diag$component_proportions['case'], 3)}")
  cli::cli_text("  Parameter separation: {round(diag$parameter_separation, 4)}")
  
  # Model fit
  cli::cli_text("")
  cli::cli_alert_info("Model Fit:")
  cli::cli_text("  Log-likelihood: {round(model_result$log_likelihood, 2)}")
  cli::cli_text("  BIC: {round(diag$model_selection$BIC, 2)}")
  cli::cli_text("  Converged: {model_result$convergence} ({model_result$n_iterations} iterations)")
  
  # Phenotype probabilities summary
  probs <- model_result$phenotype_probabilities
  cli::cli_text("")
  cli::cli_alert_info("Phenotype Probabilities:")
  cli::cli_text("  Mean: {round(mean(probs), 3)}")
  cli::cli_text("  Range: {round(min(probs), 3)} - {round(max(probs), 3)}")
  cli::cli_text("  High confidence cases (>0.8): {sum(probs > 0.8)}")
  cli::cli_text("  High confidence controls (<0.2): {sum(probs < 0.2)}")
}
