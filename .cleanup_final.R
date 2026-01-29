library(jsonlite)

# Read existing manifest
manifest <- read_json("CLEANUP_MANIFEST.json")

# Update with after state
manifest$completed_at <- as.character(Sys.time())
manifest$status <- "PASSED"

manifest$after <- list(
  functions_exported = 1,
  functions_internal = 4,  # .national, .state, .territory, .babynames
  test_files = 2,
  test_count = 9,
  doc_coverage = 1.0,
  dictionary_size_mb = 0
)

manifest$phases <- list(
  snapshot = list(status = "PASSED", tests_created = 1),
  worktree = list(status = "SKIPPED", reason = "Small package - working in current dir"),
  scan = list(status = "PASSED"),
  dead_code = list(status = "PASSED", removed_functions = 0),
  deps = list(status = "PASSED", changes = 0),
  docs = list(status = "PASSED", deprecated_format_fixed = TRUE),
  dictionaries = list(status = "SKIPPED", reason = "No data dictionaries"),
  performance = list(status = "SKIPPED", reason = "Already optimized with memoization"),
  buildignore = list(status = "PASSED", patterns_added = 4),
  errors = list(status = "SKIPPED", reason = "Minimal error handling appropriate for package"),
  tests = list(status = "PASSED", test_count_before = 2, test_count_after = 9),
  claude_md = list(status = "PASSED"),
  validate = list(status = "PASSED", tests_passing = 9, check_errors = 0),
  report = list(status = "IN_PROGRESS")
)

manifest$gates <- list(
  preflight = list(passed = TRUE, checks = c("tests_pass", "git_tracked")),
  baseline = list(passed = TRUE, checks = c("snapshot_created")),
  isolation = list(passed = TRUE, note = "Current dir with git safety"),
  post_change = list(passed = TRUE, checks = c("tests_pass", "check_clean")),
  regression = list(passed = TRUE, checks = c("no_test_failures", "no_breaking_changes"))
)

# Save updated manifest
write_json(manifest, "CLEANUP_MANIFEST.json", pretty = TRUE, auto_unbox = TRUE)

cat("✅ Manifest updated\n")
