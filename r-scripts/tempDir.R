
library(purrr)

tempdir()
dir("/tmp")
which(dir("/tmp")==gsub("/tmp/", "", tempdir(), fixed = T))

file.create(file.path(tempdir(), "test.R"))

map(dir("/tmp"), function(x) list.files(paste0("/tmp/", x)))



file.edit(file.path(tempdir(), "test.R"))
file.show(file.path(tempdir(), "test.R"))

map(dir("/tmp"), function(x) list.files(paste0("/tmp/", x)))
