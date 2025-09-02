#' Package Initialization
#'
#' This file contains package initialization functions that run when the package is loaded.

#' Detect All of Us Environment
#'
#' Checks if we're running in an All of Us Research Program environment
#'
#' @return Logical indicating if in All of Us environment
#' @keywords internal
is_aou_environment <- function() {
  # Check for All of Us specific environment variables or characteristics
  aou_indicators <- c(
    "AOU_WORKSPACE",
    "WORKSPACE_NAMESPACE", 
    "WORKSPACE_NAME",
    "GOOGLE_CLOUD_PROJECT"
  )
  
  # Check if any All of Us indicators are present
  any_aou_vars <- any(sapply(aou_indicators, function(x) nzchar(Sys.getenv(x))))
  
  # Also check if allofus package functions are available and working
  allofus_available <- requireNamespace("allofus", quietly = TRUE)
  
  return(any_aou_vars && allofus_available)
}

#' Check and Establish All of Us Connection
#'
#' Automatically attempts to establish All of Us database connection when package loads,
#' but only in All of Us environments.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  
  packageStartupMessage("📊 pheprobAoU: PheProb Implementation for All of Us EHR Data")
  
  # Check if auto-connection is enabled (default: TRUE)
  auto_connect <- getOption("pheprobAoU.auto_connect", TRUE)
  
  # Check if we're in an All of Us environment
  in_aou_env <- is_aou_environment()
  
  if (auto_connect && in_aou_env) {
    # We're in All of Us environment - attempt auto-connection
    tryCatch({
      
      # Check if already connected
      if (requireNamespace("allofus", quietly = TRUE)) {
        # Try to check existing connection
        connection_check <- tryCatch({
          allofus::aou_sql("SELECT 1 LIMIT 1")
          TRUE
        }, error = function(e) FALSE)
        
        if (connection_check) {
          packageStartupMessage("✅ Already connected to All of Us Research Program")
        } else {
          # Attempt connection
          packageStartupMessage("🔌 Connecting to All of Us Research Program...")
          allofus::aou_connect()
          
          # Verify connection with a simple query
          connection_verified <- tryCatch({
            allofus::aou_sql("SELECT 1 LIMIT 1")
            TRUE
          }, error = function(e) FALSE)
          
          if (connection_verified) {
            packageStartupMessage("✅ Successfully connected to All of Us Research Program")
          } else {
            packageStartupMessage("⚠️  Connection attempted but verification failed")
          }
        }
      }
      
    }, error = function(e) {
      packageStartupMessage("❌ Failed to auto-connect to All of Us Research Program")
      packageStartupMessage("💡 You can connect manually with: allofus::aou_connect()")
    })
    
  } else if (auto_connect && !in_aou_env) {
    # Local environment - explain the situation
    packageStartupMessage("ℹ️  Running in local environment (not All of Us cloud)")
    packageStartupMessage("ℹ️  Database connection only available in All of Us Research Workbench")
    packageStartupMessage("💡 For local development, use mock data or simulation functions")
    
  } else {
    # Auto-connection disabled
    if (in_aou_env) {
      packageStartupMessage("ℹ️  Auto-connection disabled. Connect manually with: allofus::aou_connect()")
    } else {
      packageStartupMessage("ℹ️  Local environment - use connect_aou() in All of Us cloud environment")
    }
  }
  
  packageStartupMessage("📖 For help: ?calculate_pheprob or vignette('phenotyping-with-pheprobAoU')")
}

#' Package Unload
#'
#' Clean up when package is unloaded
#'
#' @keywords internal  
.onUnload <- function(libpath) {
  # Any cleanup needed when package is unloaded
}
