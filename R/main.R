rolldice = function(nwords) {
  cons = dim(showConnections(all=TRUE))[1]
  roll = paste0(ceiling(random::randomNumbers(n=nwords*5,max=59999, col = 5)/10000),
                collapse="")
  rolls = integer(nwords)
  for (i in 1:nwords) rolls[i] =
      as.integer(substr(roll, start= 5*(i-1) + 1, stop = 5*i))
  closeme = getConnection(cons)
  close(closeme)
  return(rolls)
}

#' Find the word associated with a roll
#'
#' @param roll A number that will be found in the "roll" column of the "lu" object.
#' @param lu A data frame with a "roll" and a "word" column
#'
#' @return A word from a lookup table
#' @export

wordlu = function(roll, lu=dicewaRe:::English_lu) {
  lu[lu$roll==roll,]$word
}

vwordlu = Vectorize(wordlu)

#' Generate a passphrase
#'
#' @param nwords The number of words in your passphrase
#'
#' @return A character string with the dice rolls (from Random.org) and the diceware lookup words
#'
#'
#' @export

phrase = function(nwords) {
  rolls = rolldice(nwords)
  cat(paste0(rolls," ",vwordlu(rolls), collapse=" "))
}
