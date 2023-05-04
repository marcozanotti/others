
url <- 'https://www.windguru.cz/49079'
webpage <- download.file(url, "webpage")

library(rvest)
webpage_test <- rvest::read_html(url)
docket <- html(url) 

library(RCurl)
webpage <- getURL(url)

library(XML)
parsedpage <- htmlParse(url)


raw_data_json <- scan(url, "")
vegetation_data <- fromJSON(raw_data_json)



library(magick)
test <- image_read("test.png") %>% 
  image_ocr(language = "it")
test %>% 
  strsplit("\n")

