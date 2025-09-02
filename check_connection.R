# Check if you're already connected
library(allofus)

# Test connection
tryCatch({
  workspaces <- aou_ls_workspaces()
  if (length(workspaces) > 0) {
    cat("✅ Already connected to All of Us\n")
    cat("Available workspaces:\n")
    print(workspaces)
  } else {
    cat("❌ Not connected. Run aou_connect() first.\n")
  }
}, error = function(e) {
  cat("❌ Connection error:", e$message, "\n")
  cat("Run aou_connect() to establish connection.\n")
})
