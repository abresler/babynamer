library(dplyr)

# Count exported functions
namespace_lines <- readLines("NAMESPACE")
exports <- sum(grepl("^export\\(", namespace_lines))

# Count internal functions
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
all_funcs <- 0

for (f in r_files) {
  lines <- readLines(f)
  func_defs <- sum(grepl("<-\\s*function\\(|=\\s*function\\(", lines))
  all_funcs <- all_funcs + func_defs
}

internal_funcs <- all_funcs - exports

# Test coverage
test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$")
test_count <- length(test_files)

# Documentation coverage
man_files <- list.files("man", pattern = "\\.Rd$")
doc_coverage <- length(man_files) / max(exports, 1)

# Dictionary size
data_dir <- "data"
dict_size_mb <- 0
if (dir.exists(data_dir)) {
  data_files <- list.files(data_dir, pattern = "\\.rda$|\\.RData$", full.names = TRUE)
  if (length(data_files) > 0) {
    dict_size_mb <- sum(file.size(data_files)) / 1024 / 1024
  }
}

manifest <- list(
  package = "babynamer",
  cleanup_id = format(Sys.time(), "%Y%m%d-%H%M%S"),
  started_at = as.character(Sys.time()),
  before = list(
    functions_exported = exports,
    functions_internal = internal_funcs,
    test_files = test_count,
    doc_coverage = round(doc_coverage, 2),
    dictionary_size_mb = round(dict_size_mb, 2)
  )
)

cat("📊 HEALTH SCAN RESULTS\n")
cat("======================\n")
cat("Functions exported:", exports, "\n")
cat("Functions internal:", internal_funcs, "\n")
cat("Test files:", test_count, "\n")
cat("Doc coverage:", round(doc_coverage * 100), "%\n")
cat("Dictionary size:", round(dict_size_mb, 2), "MB\n\n")

jsonlite::write_json(manifest, "CLEANUP_MANIFEST.json", pretty = TRUE, auto_unbox = TRUE)
cat("✅ Manifest created\n")
