#' with_callAttr

#' Capture the System Call For Functions That Don't Already Do So
#' @param expr An expression to evaluate and capture as a call.
#' @param f The frame number, specified as an integer. Passed directly to \code{sys.call}.
#' @return The expected output of \code{expr} with an attribute named 'call', accessible with \code{attr(expr, 'call')}.
#' @author Jon Katz
#' @examples
#' 
#' test.expr <- with_callAttr(cbind(x=1:4, y=rnorm(4)), 0)
#' attr(test.expr,'call')
#' 
#' @keywords misc
#' @export 
#' 
with_callAttr <- function(expr, f) {
    attr(expr, 'call') <- sys.call(f)
    expr
}
