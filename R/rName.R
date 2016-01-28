# Generate names composed of random characters and numbers
# No guarantee names won't collide, but they are guaranteed to start with a letter.

# char is numeric: the number of characters, not including the optional extension
# ext is character: the optional extension, starting with a dot e.g. '.txt' 


rName <- function(char=16, ext=NULL) {
    if(!char > 1) stop('Minimum of 2 characters required.')
    paste0(sample(c(letters,LETTERS), 1), paste0(sample(c(letters,LETTERS,rep(0:9,3)), char-1, replace=TRUE),collapse=""), ext)
}
