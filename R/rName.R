#' rName
#' 
#' Generate names composed of random characters and numbers. There is no guarantee names won't collide, but they are guaranteed to start with a letter.
#' 
#' @param char The number of characters, not including the optional extension (numeric).
#' @param ext The optional extension, starting with a dot e.g. '.txt' (character).
#' @note Kind of similar to \code{basename(tempfile("",""))}, but less rigorous and not hex based.
#' @examples
#' rName()
#' @keywords manip
#' @export
rName <- function(char=16, ext=NULL) {
    if(!char > 1) stop('Minimum of 2 characters required.')
    paste0(sample(c(letters,LETTERS), 1), paste0(sample(c(letters,LETTERS,rep(0:9,3)), char-1, replace=TRUE),collapse=""), ext)
}
