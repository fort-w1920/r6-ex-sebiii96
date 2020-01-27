### Kartenspiel ###

library(R6)
library(checkmate)

Cards <- R6Class(
  "Cards", 
  public = list(
    deck = "<blank>", 
    shuffle = function() {
      self$deck <- private$.shuffle(self$deck)
    }, 
    draw = function(n) {
      check_integer(n, len = 1, lower = 0)
      check_true(n <= length(self$deck))
      # when drawing cards we have to be carefull if we draw the whole deck 
      which_cards <- logical(length = length(self$deck))
      which_cards[1:n] <- TRUE 
      
      drawn_cards <- self$deck[which_cards]
      # update the deck 
      self$deck <- self$deck[!which_cards]
      # return the drawn cards
      drawn_cards
    }, 
    refill = function() self$deck <- private$.shuffle(), 
    cut = function()  {
      check_true(!is.null(self$deck))
      deck_order <- seq_along(self$deck)
      # when we cut the deck we want to change something
      # therefore we dont allow e.g. to draw 32 in a deck of 32 cards
      where <- sample(deck_order[-length(deck_order)], 1)
      self$deck <- c(self$deck[(where + 1):length(deck_order)], self$deck[1:where])
    }, 
    initialize = function() {
      self$deck <- private$.shuffle()
    }
  ), 
  private = list(
    .shuffle = function(cards = NA) {
      check_true(is.null(cards)) # if we draw all cards we might end up 
      # with an empty deck, therefore we do the exception handling with NA 
      # and not NULL 
      farbe <- c("G", "H", "E", "S")
      wert <- c(6:10, "U", "O", "K", "A")
      deck <- paste0(rep(farbe, each = 9), rep(wert, times = 4))
      if (!is.na(cards[1])) {
        assert(
          check_true(all(cards %in% deck)), 
          check_true(length(unique(deck)) == length(deck)), 
          combine = "and"
        )
        deck <- cards
      }
      random_order <- sample(1:length(deck))
      deck[random_order]
    }
  )
)

