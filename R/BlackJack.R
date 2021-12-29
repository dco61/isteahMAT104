#' Function Blackjack(): Draw random sets of 3 cards and compute sum of values
#'
#' Argument:
#' sampling_number:  number of card sets to draw (default = 30)
#' @export
#'
Blackjack <- function(sampling_number=30){
  card_suits <- c("Pique", "Coeur", "Carreau", "Trèfle")
  card_names <- c("As", "Roi", "Reine", "Valet", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  card_values <- c(1, 10, 10, 10, 10:2)
  n_suits <- length(card_suits)
  card_deck <- expand.grid(card_names, card_suits)
  card_deck <- card_deck %>%
    unite("Cards", c(1, 2), sep = "_", remove = TRUE) %>%
    mutate(Values = rep(card_values, n_suits))
  sample_sums = rep(NA,sampling_number)
  cards = rep(NA,sampling_number)
  for(i in 1:sampling_number){
    cards[i] <- list(sample(card_deck$Cards, size = sample_size, replace = FALSE))
    Card_val = card_deck$Values[card_deck$Cards %in% cards[[i]]]
    sample_sums[i] <- sum(Card_val)+10*(sum(Card_val==1)>0 & sum(Card_val)<=11)
  }
  return(sample_sums)
}
