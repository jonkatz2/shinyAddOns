#' updateTextareaInput
#' 
#' @description Set the value of a textarea on a shiny application UI.
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param label The label to set for the input object
#' @param value The value to set for the input object.
#' @details Although a \code{textareaInput} doesn't exist in shiny it is possible to create one with HTML and update it using the shiny messaging system. This function appropriates the code from the shiny updateTextInput.
#' @examples
#' 
#' # textarea can be created in UI with HTML:
#'     HTML({"
#'         <label for='inText'>Old label</label>
#'         <textarea id='inText', class='textarea', placeholder= ''></textarea>"
#'     })
#' 
#' \dontrun{
#' # From ?updateTextInput
#' shinyServer(function(input, output, session) {
#' 
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#' 
#'     # This will change the value of input$inText, based on x
#'     updateTextareaInput(session, "inText", value = paste("New text", x))
#' 
#'     # Can also set the label, this time for input$inText2
#'     updateTextareaInput(session, "inText2",
#'       label = paste("New label", x),
#'       value = paste("New text", x))
#'   })
#' })
#' 
#' }


updateTextareaInput <- function(session, inputId, label=NULL, value=NULL) {
    message <- shiny:::dropNulls(list(label = label, value = value))
    session$sendInputMessage(inputId, message)
}

# Should maybe just make an alias for updateTextInput:
# updateTextareaInput <- shiny::updateTextInput
# or maybe even that is unnecessary?

#textareaInput <- function(inputID, label, value = "", placeholder = "", width='100%', height='8em') {
#    HTML({
#        paste0("
#            <label for=", inputID, ">", label, "</label>
#            <textarea id=", inputID, ", class='textarea', placeholder= ", placeholder, "></textarea>"
#        )
#    })
#}





