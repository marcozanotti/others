library(uvet)
library(quanteda)
library(stringi)
library(stringr)

# function that extracts features from a single name
namesFeatures <- function(string, tolower = TRUE){
  
  if (!class(string) == "character")
    stop("\nArgument string must be of class character.\n")
  
  cat("\nExtracting features from string ...\n")
  cat("\n")
  
  vowels <- "[aeiouAEIOU]"
  consonants <- "[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]"
  others <- "[^aeiouAEIOUbcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]"
  
  if (tolower)
    string <- str_to_lower(string)
  
  
  # Pre-processing
  string <- str_squish(string)
  stringMod <- str_remove_all(string, others)
  
  
  ## Feature Engeneering
  
  # length analysis (simple length)
  len <- str_count(string) %>% as.integer()
  lenMod <- str_count(stringMod) %>% as.integer()
  
  # double - or more - name (dummy)
  doubleName <- str_count(string, "\\s") %>% as.integer()
  doubleName <- ifelse(doubleName >= 1, 1, 0)
  
  # special characters (dummy)
  specialChrs <- str_count(string, sprintf("[%s\\s]", str_remove_all(others, "\\[|\\]")))
  specialChrs <- ifelse(specialChrs >= 1, 1, 0)
  
  # first letter analysis (logical, whether the first letter is a consonant)
  firstLetter <- str_starts(string, consonants) %>% as.integer()
  
  # last letter analysis (logical, whether the last letter is a consonant)
  lastLetter <- str_ends(string, consonants) %>% as.integer()
  
  # num of vowels
  numVowels <- str_count(string, vowels) %>% as.integer()
  
  # num of consonants
  numConsonants <- str_count(string, consonants) %>% as.integer()
  
  # proportion of vowels (with lenMod)
  propVowels <- round(numVowels/lenMod, 2)
  
  # proportion of consonants (with lenMod)
  propConsonants <- round(numConsonants/lenMod, 2)
  
  # num of consecutive vowels
  numConsecVowels <- str_split(string, consonants) %>% 
    unlist() %>% 
    str_remove_all(others) %>% 
    str_trim() %>% 
    str_count()
  numConsecVowels <- sum(numConsecVowels > 1) %>% as.integer()
    
  # num of consecutive consonants
  numConsecConsonants <- str_split(string, vowels) %>% 
    unlist() %>%
    str_remove_all(others) %>%
    str_trim() %>% 
    str_count()
  numConsecConsonants <- sum(numConsecConsonants > 1) %>% as.integer()
  
  # max num of consecutive vowels (1 = aa, 2 = aaa, ecc)
  maxConsecVowels <- str_split(string, consonants) %>% 
    unlist() %>%
    str_remove_all(others) %>%
    str_trim() %>% 
    str_count() %>% 
    -1 %>% 
    max() %>% 
    as.integer() 
  
  # max num of consecutive consonants (1 = zz, 2 = zzz, ecc)
  maxConsecConsonants <- str_split(string, vowels) %>% 
    unlist() %>% 
    str_remove_all(others) %>%
    str_trim() %>% 
    str_count() %>% 
    -1 %>% 
    max() %>% 
    as.integer()
  
  # 3x consecutive vowels (dummy)
  Vowels3x <- str_count(string, sprintf("%s{3}", vowels))
  Vowels3x <- ifelse(Vowels3x > 0, 1L, 0L)
  
  # 4x consecutive vowels (dummy)
  Vowels4x <- str_count(string, sprintf("%s{4}", vowels))
  Vowels4x <- ifelse(Vowels4x > 0, 1L, 0L)
  
  # 5x consecutive vowels (dummy)
  Vowels5x <- str_count(string, sprintf("%s{5}", vowels))
  Vowels5x <- ifelse(Vowels5x > 0, 1L, 0L)
  
  # 3x consecutive consonants (dummy)
  Consonants3x <- str_count(string, sprintf("%s{3}", consonants))
  Consonants3x <- ifelse(Consonants3x > 0, 1L, 0L)
  
  # 4x consecutive consonants (dummy)
  Consonants4x <- str_count(string, sprintf("%s{4}", consonants))
  Consonants4x <- ifelse(Consonants4x > 0, 1L, 0L)
  
  # 5x consecutive consonants (dummy)
  Consonants5x <- str_count(string, sprintf("%s{5}", consonants))
  Consonants5x <- ifelse(Consonants5x > 0, 1L, 0L)
  
  # number of syllable
  nSyllables <- nsyllable(string)
  
  
  ## n-grams analysis (using modified string)
  tmp <- tokens(stringMod, "character")
  
  # number of 3-grams without vowels
  gram3 <- tokens_ngrams(tmp, n = 3, concatenator = "") %>% unlist() %>% unname()
  count <- str_count(gram3, vowels)
  numNoVowelsGram3 <- (count == 0) %>% sum() %>% as.integer()
  
  # number of 4-grams without vowels
  gram4 <- tokens_ngrams(tmp, n = 4, concatenator = "") %>% unlist() %>% unname()
  count <- str_count(gram4, vowels)
  numNoVowelsGram4 <- (count == 0) %>% sum() %>% as.integer()
  
  # number of 5-grams without vowels
  gram5 <- tokens_ngrams(tmp, n = 5, concatenator = "") %>% unlist() %>% unname()
  count <- str_count(gram5, vowels)
  numNoVowelsGram5 <- (count == 0) %>% sum() %>% as.integer()
    
  
  ## keyboard distance analysis
  dist <- keyboardDistance(string, method = "euclidean", keyboardType = "QWERTY")
  
  # total distance
  totDist <- dist$totDist

  # dynamic distance
  dynDist <- dist$dynDist
  
  # expand variables to include different keyboard types
  
  
  ## word entity analysis 
  #entity <- spacy_parse(string)$pos
  #entity <- ifelse(entity %in% c("NOUN", "PROPN"), 1, 0)
  # spacyr entity recognition is useless (it deos not capture names as noun or
  # it indentifies random strings as such)
  
  
  variables <- c(string, 
                 len, firstLetter, lastLetter, 
                 numVowels, numConsonants, 
                 propVowels, propConsonants, 
                 numConsecVowels, numConsecConsonants, 
                 maxConsecVowels, maxConsecConsonants,
                 Vowels3x, Vowels4x, Vowels5x,
                 Consonants3x, Consonants4x, Consonants5x,
                 nSyllables,
                 numNoVowelsGram3, numNoVowelsGram4, numNoVowelsGram5,
                 totDist, dynDist)
  names(variables) <- c("String", 
                        "Length", "FirstLetter", "LastLetter",
                        "NumV", "NumC",
                        "PropV", "PropC",
                        "NumConsecV", "NumConsecC",
                        "MaxConsecV", "MaxConsecC", 
                        "V3x", "V4x", "V5x", 
                        "C3x", "C4x", "C5x",
                        "nSyllables",
                        "NumNoVgram3", "NumNoVgram4", "NumNoVgram5",
                        "TotKeybDist", "DynKeybDist")
  
  df <- as.data.frame(t(variables), stringsAsFactor = FALSE)
  
  #return(invisible(variables))
  return(df)
  
}


# function that compute distances among keyboard letters
keyboardDistance <- function(string, rmPattern = "[^[:alpha:]]", method = "euclidean", 
                             keyboardType = "QWERTY", distanceDataFrame = FALSE){
  
  if (!class(string) == "character")
    stop("\nArgument string must be of class character.\n")
  
  if (!class(method) == "character")
    stop("\nArgument method must be of class character.\n")
  
  if (!class(keyboardType) == "character")
    stop("\nArgument keyboardType must be of class character.\n")
  
  
  method <- str_to_lower(method)
  keyboardType <- str_to_upper(keyboardType)
  
  # text mining sulla stringa
  stringMod <- str_to_lower(string) %>% str_trim() %>% str_remove_all(rmPattern)
  chrs <- str_split(stringMod, "") %>% unlist() # split on characters
  
  cat(sprintf("\nComputing keyboard distance of string %s ...\n", string))
  cat("\n")
  
  
  if (keyboardType == "QWERTY") {
    
    Names <- c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
               "a", "s", "d", "f", "g", "h", "j", "k", "l",
               "z", "x", "c", "v", "b", "n", "m")
    # X and Y coordinates of keyboard letters
    x <- c(rep(1L, 10), rep(2L, 9), rep(3L, 7))
    y <- c(1L:10L, 1L:9L, 1L:7L)
    
    # # with spacebar
    # Names <- c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
    #            "a", "s", "d", "f", "g", "h", "j", "k", "l",
    #            "z", "x", "c", "v", "b", "n", "m",
    #            "space")
    # # X and Y coordinates of keyboard letters
    # x <- c(rep(1L, 10), rep(2L, 9), rep(3L, 7), rep(4L, 1))
    # y <- c(1L:10L, 1L:9L, 1L:7L, mean(2L:8L)) # assign a mean value position for spacebar
    
  } else if (keyboardType == "QWERTZ") {
    
    Names <- c("q", "w", "e", "r", "t", "z", "u", "i", "o", "p",
               "a", "s", "d", "f", "g", "h", "j", "k", "l",
               "y", "x", "c", "v", "b", "n", "m")
    # X and Y coordinates of keyboard letters
    x <- c(rep(1L, 10), rep(2L, 9), rep(3L, 7))
    y <- c(1L:10L, 1L:9L, 1L:7L)
    
  } else if (keyboardType == "AZERTY") {
    
    Names <- c("a", "z", "e", "r", "t", "y", "u", "i", "o", "p",
               "q", "s", "d", "f", "g", "h", "j", "k", "l", "m",
               "w", "x", "c", "v", "b", "n")
    # X and Y coordinates of keyboard letters
    x <- c(rep(1L, 10), rep(2L, 10), rep(3L, 6))
    y <- c(1L:10L, 1L:9L, 1L:7L)
    
  } else {
    cat(sprintf("\nDoes not know keyboard type %s.\n", keyboardType))
    cat("\n")
    return(NULL)
  }
  
  
  # creates df of coordinates
  df <- data.frame(X = x, Y = y, row.names = Names, stringsAsFactors = FALSE)
  df <- df[which(rownames(df) %in% chrs),] # keeps only those characters within the string
  
  # compute distances among keyboard letters
  distMAT <- dist(df, method = method) %>%
    round(digits = 2) %>%
    as.matrix()
  
  # total distance (sum of all distances among string's letters divided by 2)
  totStringDist <- sum(distMAT)/2
  
  # dynamic distance (rolling sum of distances among string's letters)
  tmp <- c()
  for (i in 1:(length(chrs) - 1)) {
    tmp <- c(tmp, distMAT[chrs[i], chrs[i + 1]])
  }
  dynStringDist <- sum(tmp)
  
  # correction for spaces and/or other special characters
  # correction = (mean(distances) + sd(distances)) * numOfCorrections
  nCorrection <- str_count(string, rmPattern)
  correctionTOT <- round((mean(distMAT[lower.tri(distMAT)]) + sd(distMAT[lower.tri(distMAT)])) * nCorrection, 2)
  correctionDYN <- round((mean(tmp) + sd(tmp)) * nCorrection, 2)
  
  totStringDist <- totStringDist + correctionTOT
  dynStringDist <- dynStringDist + correctionDYN
  
  
  if (distanceDataFrame) {
    output <- list(totDist = totStringDist, dynDist = dynStringDist, distMAT = distMAT)
  } else {
    output <- list(totDist = totStringDist, dynDist = dynStringDist)
  }
   
  return(output)
  
}


out <- namesFeatures("stackoverflow")


# genere (Mr/Mrs), nazionalita, città, telefono, email
# metasearch, sito
# carrier, origine, destinazione


# function that generates random strings
randomStrings <- function(n, len, patt, doubleName = FALSE, 
                          len2 = NULL, sameDoubleName = FALSE){
  
  l <- length(length)
  if (l < n)
    stop("\nArgument n should be greater than the length of argument len.\n")
  
  # genero stringhe random 
  strings <- stri_rand_strings(n, len, patt)
  
  if (doubleName) { 
    if (sameDoubleName) { 
      strings <- str_c(strings, strings, sep = " ") # doppie dtringhe uguali
    } else {
      if (!is.null(len2))
        len <- len2 # diversa lunghezza se desiderato
      double <- stri_rand_strings(n, len, patt) # genero le seconde stringhe
      strings <- str_c(strings, double, sep = " ") 
    }
  }
  
  return(strings)
  
}


#set.seed(170792)
n <- 20
length <- sample(2L:10L, 20, replace = TRUE)
length2 <- sample(2L:10L, 20, replace = TRUE)
pattern <- "[A-Z]"

randomStrings(20, sample(2L:10L, 20, replace = TRUE), pattern)
# due nomi con stessa lunghezza
randomStrings(20, sample(2L:10L, 20, replace = TRUE), pattern, 
              doubleName = TRUE) 
# due nomi con lunghezza random
randomStrings(20, sample(2L:10L, 20, replace = TRUE), pattern, 
              doubleName = TRUE, len2 = length2)
# due nomi uguali
randomStrings(20, sample(2L:10L, 20, replace = TRUE), pattern, 
              doubleName = TRUE, sameDoubleName = TRUE)


#################################################-
set.seed(170792)
sample.int(10, 20, replace = TRUE)
set.seed(170792)
sample(2L:10L, 20, replace = TRUE)
#################################################-


## Random Names Generation
library(randomNames)

set.seed(170792)
df <- randomNames(n = 10000, return.complete.data = TRUE) # Male = 0, Female = 1
table(df$ethnicity)
table(df$ethnicity)/sum(table(df$ethnicity))

length(unique(df$first_name))
length(unique(df$last_name))

length(unique(paste0(df$first_name, df$last_name)))

set.seed(170792)
generateNames <- function(n){
  names <- unique(randomNames(n = n))
  need <- n - length(names)
  while (need > 0) { 
    names <- unique(c(randomNames(n = need), names))
    need <- n - length(names)
  }
  return(names)
}
df <- generateNames(n = 100000)
length(unique(df))



