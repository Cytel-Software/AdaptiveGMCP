# ============================================================================
# AdaptGMCP Package Rebuild and Install Script
# ============================================================================
# Usage: Source this file from RStudio or R console:
#   source("rebuild_package.R")
# Then run any of the functions below:
#   reinstall()  - Remove old version and reinstall
#   load_pkg()   - Load package for development
#   check_pkg()  - Run R CMD check
#   test_pkg()   - Run tests
#   etc.
# ============================================================================

library(devtools)

# Remove old version and reinstall package
reinstall <- function() {
  message("==== Rebuilding and Reinstalling AdaptGMCP ====\n")

  # Check if package is installed
  if ("AdaptGMCP" %in% rownames(installed.packages())) {
    message("Removing old version...")
    remove.packages("AdaptGMCP", lib = .libPaths()[1])
    message("✓ Old version removed\n")
  } else {
    message("No previous version found\n")
  }

  # Install package
  message("Installing package...")
  devtools::install(upgrade = FALSE)
  message("✓ Package installed successfully!\n")

  message("To load the package, run: load_pkg()")
}

# Load package for development (without installation)
load_pkg <- function() {
  message("==== Loading AdaptGMCP for Development ====\n")
  devtools::load_all()
  message("✓ Package loaded!")
}

# Run R CMD check
check_pkg <- function() {
  message("==== Running R CMD Check ====\n")
  devtools::check()
}

# Build package tarball
build_pkg <- function() {
  message("==== Building Package ====\n")
  devtools::build()
}

# Run tests
test_pkg <- function() {
  message("==== Running Tests ====\n")
  devtools::test()
}

# Generate documentation
document_pkg <- function() {
  message("==== Generating Documentation ====\n")
  devtools::document()
  message("✓ Documentation updated!")
}

# Run lintr for code quality
lint_pkg <- function() {
  message("==== Running Code Quality Checks ====\n")
  if (!require("lintr", quietly = TRUE)) {
    message("lintr package not installed. Install with: install.packages('lintr')")
    return(invisible(NULL))
  }
  lintr::lint_package()
}

# Install package dependencies
install_deps <- function() {
  message("==== Installing Dependencies ====\n")
  devtools::install_deps(dependencies = TRUE)
  message("✓ Dependencies installed!")
}

# ============================================================================
# renv Management Functions
# ============================================================================

# Check renv status
renv_status <- function() {
  message("==== Checking renv Status ====\n")
  renv::status()
}

# Update lockfile to match current installed packages
renv_snapshot <- function() {
  message("==== Updating renv.lock (Snapshot) ====\n")
  message("This will update renv.lock to match your currently installed packages.\n")
  renv::snapshot()
  message("✓ Lockfile updated! Don't forget to commit renv.lock to git.")
}

# Restore packages to match lockfile
renv_restore <- function() {
  message("==== Restoring Packages from renv.lock ====\n")
  message("This will install/update packages to match renv.lock.\n")
  renv::restore()
  message("✓ Packages restored!")
}

# Update all packages to latest versions
renv_update <- function() {
  message("==== Updating All Packages ====\n")
  message("This will update packages to their latest versions.\n")
  renv::update()
  message("\nRun renv_snapshot() to save these changes to renv.lock")
}

# Clean unused packages
renv_clean <- function() {
  message("==== Cleaning Unused Packages ====\n")
  message("⚠️  Warning: This will remove packages not used in your code.")
  message("Development packages like devtools, testthat may be removed.\n")
  response <- readline(prompt = "Continue? (y/n): ")
  if (tolower(response) == "y") {
    renv::clean()
    message("✓ Cleanup complete!")
  } else {
    message("Cancelled.")
  }
}

# Check for package inconsistencies and offer fixes
renv_hygiene <- function() {
  message("==== renv Hygiene Check ====\n")

  status <- tryCatch({
    capture.output(renv::status())
  }, error = function(e) {
    message("Error checking renv status: ", e$message)
    return(NULL)
  })

  if (is.null(status)) return(invisible(NULL))

  # Check if everything is in sync
  if (any(grepl("up to date", status, ignore.case = TRUE))) {
    message("✓ All packages are in sync!\n")
    return(invisible(NULL))
  }

  # Show status
  cat(paste(status, collapse = "\n"), "\n\n")

  # Offer solutions
  message("Available actions:")
  message("  1. renv_restore()  - Restore packages to match renv.lock")
  message("  2. renv_snapshot() - Update renv.lock to match current packages")
  message("  3. renv_status()   - Show detailed status\n")
}

# Install a new package through renv
renv_install <- function(package) {
  if (missing(package)) {
    message("Usage: renv_install('package_name')")
    return(invisible(NULL))
  }
  message(paste0("==== Installing ", package, " via renv ====\n"))
  renv::install(package)
  message("\nRun renv_snapshot() to save this to renv.lock")
}

# Complete renv workflow after changes
renv_sync <- function() {
  message("==== Syncing renv Environment ====\n")
  message("Checking status...\n")
  renv::status()
  message("\nUpdating lockfile...\n")
  renv::snapshot()
  message("✓ Environment synced! Remember to commit renv.lock to git.")
}

# Complete workflow: document, check, and reinstall
full_rebuild <- function() {
  message("==== Running Full Rebuild Workflow ====\n")
  document_pkg()
  message("\n")
  check_pkg()
  message("\n")
  reinstall()
}

# Quick reinstall (most common use case)
quick_reinstall <- function() {
  message("==== Quick Reinstall ====\n")
  if ("AdaptGMCP" %in% rownames(installed.packages())) {
    remove.packages("AdaptGMCP", lib = .libPaths()[1])
  }
  devtools::install(upgrade = FALSE)
  devtools::load_all()
  message("✓ Done! Package reinstalled and loaded.")
}

# Display available functions
help_rebuild <- function() {
  cat("
╔═══════════════════════════════════════════════════════════════╗
║         AdaptGMCP Package Rebuild Functions                   ║
╠═══════════════════════════════════════════════════════════════╣
║ PACKAGE MANAGEMENT:                                           ║
║ reinstall()       - Remove old version and reinstall          ║
║ quick_reinstall() - Fast reinstall and load (recommended)     ║
║ load_pkg()        - Load package for development              ║
║ check_pkg()       - Run R CMD check                           ║
║ build_pkg()       - Build package tarball                     ║
║ test_pkg()        - Run tests                                 ║
║ document_pkg()    - Update documentation                      ║
║ lint_pkg()        - Run code quality checks                   ║
║ install_deps()    - Install package dependencies              ║
║ full_rebuild()    - Complete workflow (doc + check + install) ║
║                                                               ║
║ RENV MANAGEMENT:                                              ║
║ renv_hygiene()    - Check for inconsistencies (recommended)   ║
║ renv_status()     - Show renv status                          ║
║ renv_snapshot()   - Update renv.lock to current packages      ║
║ renv_restore()    - Restore packages from renv.lock           ║
║ renv_update()     - Update all packages to latest versions    ║
║ renv_install(pkg) - Install package via renv                  ║
║ renv_sync()       - Complete renv sync workflow               ║
║ renv_clean()      - Remove unused packages (careful!)         ║
║                                                               ║
║ help_rebuild()    - Show this help message                    ║
╚═══════════════════════════════════════════════════════════════╝
")
}

# Display help on load
message("\n✓ AdaptGMCP rebuild script loaded!")
message("Run help_rebuild() to see available functions\n")
message("Quick start: quick_reinstall()\n")
