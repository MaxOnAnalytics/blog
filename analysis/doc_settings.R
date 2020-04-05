
# These chunks can be shared across multiple documents with knitr::read_chunk().

# ---- knitr-opts-chunk ----
# Update knitr chunk options
knitr::opts_chunk$set(
  echo = F, warning = F, message = F,
  comment = NA,
  fig.align = "center",
  tidy = F,
  fig.path = paste0(tools::file_path_sans_ext(knitr::current_input()),
                    "_files", .Platform$file.sep, "figure-html",
                    .Platform$file.sep)
)

# ---- last-updated ----
# Insert the date the file was last updated
cat(sprintf("**Last updated:** %s", Sys.Date()))

# ---- code-version ----
# Insert the code version (Git commit SHA1) if Git repository exists and R
# package git2r is installed
if(requireNamespace("git2r", quietly = TRUE)) {
  if(git2r::in_repository()) {
    code_version <- substr(git2r::commits()[[1]]@sha, 1, 7)
  } else {
    code_version <- "Unavailable. Initialize Git repository to enable."
  }
} else {
  code_version <- "Unavailable. Install git2r package to enable."
}
cat(sprintf("**Code version:** %s", code_version))
rm(code_version)

# ---- session-info ----
sessionInfo()
