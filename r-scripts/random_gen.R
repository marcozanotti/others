library(magrittr)
library(ggplot2)
library(ggridges)
library(purrr)

set.seed(236)
values <- 1L:5L
n_partecipants <- 100
p <- list(c(.2, .6, .1, .05, .05), 
          c(.05, .2, .6, .1, .05), 
          c(.0, .05, .05, .2, .7), 
          c(.6, .2, .1, .05, .05), 
          c(.0, .05, .05, .8, .1))

gen_weights <- function()


gen_survey_data <- function(values, n_partecipants, prob) {
  
  l <- map(seq_along(prob), function(i) 
    sample(values, n_partecipants, prob = prob[[i]], replace = TRUE))
  
  return(l)
  
}

l <- gen_survey_data(values, 100, p)
names(l) <- c("viaggi", "hotel", "ristorazione", "servizi", "onsite")
map(l, mean) %>% 
  unlist() %>% mean()

l1 <- gen_survey_data(values, 100, p[sample(values, 5, replace = FALSE)])
names(l1) <- c("viaggi", "hotel", "ristorazione", "servizi", "onsite")
map(l1, mean) %>%
  unlist() %>% mean()


# frequency plot by category
ggplot(df_e1, aes(x = score, y = category)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..),
    scale = 3,
    size = 0.3) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
  labs(title = 'Gradimento Medio per categoria') 

