
#' Draw 3 cards: BlackJack
#'
#'@param n draws (integer value, default = 30)
#'@return sum of card values for each draw
#'@importFrom dplyr "%>%"
#'@importFrom dplyr "mutate"
#'@importFrom tidyr "unite"

BlackJack <- function(n = 30) {
    card_suits <- c("Pique", "Coeur", "Carreau", "Trèfle")
    card_names <- c("As", "Roi", "Reine", "Valet", "10", "9", "8", "7", "6", "5", "4", "3",
        "2")
    card_values <- c(1, 10, 10, 10, 10:2)
    n_suits <- length(card_suits)
    card_deck <- expand.grid(card_names, card_suits)
    card_deck <- card_deck %>%
        tidyr::unite("Cards", c(1, 2), sep = "_", remove = TRUE) %>%
        dplyr::mutate(Values = rep(card_values, n_suits))
    sample_sums <- rep(NA, n)
    cards <- rep(NA, n)
    for (i in 1:n) {
        cards[i] <- list(sample(card_deck$Cards, size = 3, replace = FALSE))
        Card_val <- card_deck$Values[card_deck$Cards %in% cards[[i]]]
        sample_sums[i] <- sum(Card_val) + 10 * (sum(Card_val == 1) > 0 & sum(Card_val) <= 11)
    }
    return(sample_sums)
}
