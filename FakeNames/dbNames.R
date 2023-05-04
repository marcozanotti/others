
library(openxlsx)
df <- read.xlsx("namesComplete.xlsx", sheet = 1)
df2 <- read.xlsx("namesComplete.xlsx", sheet = 2)

names <- rbind(df, df2)

library(stringr)
library(magrittr)
names$FirstName <- names$FirstName %>% str_to_lower() 
names$LastName <- names$LastName %>% str_to_lower() 
names$Gender <- names$Gender %>% str_to_lower()
names$Title <- names$Title %>% str_to_lower()

names$Title <- names$Title %>% str_replace_all("(miss)|(ms)", "mrs")

table(names$Gender)
table(names$Title)
table(is.na(names$Title))
table(is.na(names$Gender))

table(is.na(names$Gender), is.na(names$Title), dnn = list("Gender", "Title"))

names[which(!is.na(names$Gender) & is.na(names$Title)),]$Title <- ifelse(names[which(!is.na(names$Gender) & is.na(names$Title)),]$Gender == "m", "mr", "mrs") 
names[which(is.na(names$Gender) & !is.na(names$Title)),]$Gender <- ifelse(names[which(is.na(names$Gender) & !is.na(names$Title)),]$Title == "mr", "m", "f") 

table(is.na(names$Gender), is.na(names$Title), dnn = list("Gender", "Title"))

table(is.na(names$FirstName))
table(is.na(names$LastName))
table(is.na(names$FirstName), is.na(names$LastName), dnn = list("First", "Last"))


err <- names[which(is.na(names$FirstName) | is.na(names$LastName)),]

names <- names[-which(is.na(names$FirstName) | is.na(names$LastName)),]
names <- rbind(names, err)


write.xlsx(names[1:500000,], "namesComplete1.xlsx")
write.xlsx(names[500001:nrow(names),], "namesComplete2.xlsx")

