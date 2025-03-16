library(bibtex)
library(tidyverse)
library(cli)
library(fs)

mk <- read.bib("max.bib")

make_entry <- function(x) {
 year <- x$year
 author <- strsplit(as.character(x$author)[[1]], split = " ")[[1]]
 author <- author[length(author)]

 title <- gsub("[[:punct:]]", "", x$title)
 title <- strsplit(title,  split = " ")[[1]]
 title <- title[1:min(2, length(title))]
 title <- paste0(title, collapse = "_")

 paste(year, author, title, sep = "_")
}

make_yml <- function(x, pth) {
 title <- tools::toTitleCase(x$title)
 authors <- cli::format_inline("{.and {x$author}}")
 year <- x$year
 # type <- tolower(attr(x,"bibtype"))
 type <- "article"
 type <- paste0("  - ", type, "\n")

 cat("---\n", file = pth)
 cat('title: "', title, '"\n', sep = "", file = pth, append = TRUE)
 # cat('author: "', authors, '"\n', file = pth, append = TRUE)
 cat("year: ", year, "\n", file = pth, append = TRUE)
 cat("categories:\n", file = pth, append = TRUE)
 cat(type, file = pth, append = TRUE)
 cat("---\n", file = pth, append = TRUE)
}

scholar_url <- function(x, pth) {
 title <- as.character(x$title)
 year <- x$year

 if (is.na(title) || nchar(title) < 5) {
  return(title)
 }
 if (grepl("href", title)) {
  return(title)
 }

 tags <- c("em", "it", "tt", "bf")

 for (pat in tags) {
  pattern <- glue::glue("\\{\\\\|pat| (.*?)\\}", .open = "|", .close = "|")
  title <- gsub(pattern, "\\1", title)
 }


 title <- gsub("[[:punct:]]", " ", title)
 title <- gsub("[[:space:]]+", " ", title)
 title <- gsub("[[:space:]]", "+", title)
 # default to english :-(
 sc_url <- glue::glue("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q={title}{year}&btnG=")
 sc_md <- glue::glue("\n - [Google Scholar]({sc_url})\n")
 cat(sc_md, "\n", sep = " ", file = pth, append = TRUE)

}


for (i in 1:length(mk)) {
 dir_name <- make_entry(mk[[i]])
 dir_create("research", dir_name)
 file_nm <- file_create("research", dir_name, "index.qmd")
 make_yml(mk[[i]], file_nm)

 cat("\n", format(mk[[i]]), "\n\n", file = file_nm, append = TRUE, sep = "")
 scholar_url(mk[[i]], file_nm)
 cat("\n\n", file = file_nm, append = TRUE, sep = "")
 cat("\n - DOI: [XXX](https://doi.org/XXX)\n\n", file = file_nm, append = TRUE, sep = "")

 cat("\n## Abstract\n\n", file = file_nm, append = TRUE, sep = "")
}


