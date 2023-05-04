library(dir2json)

setwd("~/RStudioDirectory")

dir2json("home\\tkt")
dir2tree("home\\tkt")
cat(dir2tree("home\\tkt"))

shinyDirTree("home\\tkt")
shinyDirTree("home")


library(devtools)
install_github("UvetAnalysis/uvet",
               auth_token = "9847444b843120a90db522c6f151bccde9729c36",
               force = TRUE)


library(uvet)
setDBAccess(driver="SQL Server")
