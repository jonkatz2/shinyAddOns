#' uiTakeSnapshot
#' 
#' Bulk Grab UI Element Values Given A List Of Names and Input Types 
#' @param inputs A named list or vector of input types (character), named with the inputId.
#' @param env Environment in which to find the shiny \code{input} list.
#' @details The output is designed to easily re-create the current state of the UI, setting values with the appropriate \code{update__Input} function. Note that it is not possible to set a \code{fileInput} value, but it is possible to store the name of the uploaded file.
#' @return A list of lists, with variable content according to the following structure:
#' \describe{
#' #'     \item{inputId}{\describe{
#'         \item{type}{Input function, as a character string.}
#'         \item{value}{Value of the UI input at time of function call.}
#'     }}
#' }
#' @author Jon Katz
#' @examples
#' \dontrun{
#' # Must be called within a shiny app
#' ui <- fluidPage(
#'     textInput("main", "Plot title:"),
#'     numericInput("randn", 'Number of Random Values:')
#'     plotOutput('outplot')
#' )
#' 
#' server <- function(input, output, session) {
#'     output$outplot <- renderPlot({
#'         input.funs <- c(main='textInput', randn='numericInput')
#'         snapshot <- uiTakeSnapshot(input.funs, environment())
#'         values <- rnorm(input$randn)
#'         plot(values, main=input$main)
#'     })
#' } 
#' }
#' @keywords misc
#' @export 

uiTakeSnapshot <- function(inputs, env) {
    inputs.v <- unlist(inputs)
    # assemble the expression as text
    input.vals <- paste0('input$', names(inputs.v))
    # parse to expression, then evaluate in the specified environment to get the value
    input.vals <- lapply(input.vals, function(x) eval(parse(text=x), envir=env))
    # consolidate the input type and the value
    inputs.l <- lapply(1:length(inputs.v), function(x) {
        if(inputs.v[[x]] == 'fileInput') list(type=inputs.v[[x]], value=input.vals[[x]]$name)
        else list(type=inputs.v[[x]], value=input.vals[[x]])
    })
    # transfer input ids
    names(inputs.l) <- names(inputs.v)
    inputs.l
}
