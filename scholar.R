library(tidyverse)
library(scholar)
library(janitor)

max_id <- "TMDDykAAAAAJ"
pubs <- get_publications(max_id)

get_publication_date(max_id, "Se3iqnhoufwC")
format_publications(max_id)
