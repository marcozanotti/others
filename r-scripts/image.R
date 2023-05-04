install.packages("magick")
library(magick)

image_read("~/Pictures/profile.jpeg") %>%
  image_quantize(colorspace = "gray") %>% 
  image_write(path = "profile2.jpeg")
