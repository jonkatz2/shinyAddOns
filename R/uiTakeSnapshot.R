#' uiTakeSnapshot
#' 
#' Bulk Grab UI Element Values Given A List Of Names and Input Types 
#' @param inputs A named list or vector of input types (character), named with the inputId.
#' @param env Environment in which to find the shiny \code{input} list.
#' @details The output is designed to easily re-create the current state of the UI, setting values with the appropriate \code{update__Input} function. Note that it is not possible to set a \code{fileInput} value, but it is possible to store the name of the uploaded file.
#' @return A list of lists, with variable content according to the following structure:
#' \describe{
#'     \item{inputId}{\describe{
#'         \item{type}{Input function, as a character string.}
#'         \item{value}{Value of the UI input at time of function call.}
#'     }}
#' }
#' @author Jon Katz
#' @examples
#' \dontrun{
#' # Must be called within a shiny app
#' library(shiny)
#' ui <- fluidPage(
#'     textInput("main", "Plot title:", value="First Plot"),
#'     numericInput("randn", 'Number of Random Values:', value=10, min=1, step=1),
#'     column(1, actionButton('submit', 'Submit')),
#'     column(1, actionButton('reset', 'Previous Settings')),
#'     div(style='clear:both;',
#'         column(9, plotOutput('outplot')),
#'         column(3, htmlOutput('snapshot', container=tags$pre, class='shiny-text-output'))
#'     )
#' )
#' 
#' server <- function(input, output, session) {
#'     snapshot <- list()
#'    
#'     observe({
#'         input$reset
#'         if(length(snapshot) > 0) {
#'             s.el <- length(snapshot)
#'             if(length(snapshot) > 1) snapshot <- snapshot[(s.el - 1):s.el]
#'             uiSetSnapshot(snapshot[[1]], session)
#'         }
#'         NULL
#'     })
#' 
#'     output$outplot <- renderPlot({
#'         input$submit
#'         isolate({
#'             input.funs <- c(main='textInput', randn='numericInput')
#'             snapshot <<- c(snapshot, list(uiTakeSnapshot(input.funs, environment())))
#'             values <- rnorm(input$randn)
#'             plot(values, main=input$main)
#'         })
#'     })
#'     
#'     output$snapshot <- renderPrint({
#'         input$submit
#'         input$reset
#'         if(length(snapshot)>1) {
#'             print(snapshot[[length(snapshot)-1]])
#'         } else print(snapshot[[length(snapshot)]])
#'     })
#' } 
#' shinyApp(ui, server)
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
        if(inputs.v[[x]] == 'fileInput') {
            list(type=inputs.v[[x]], value=input.vals[[x]]$name)
        } else if(inputs.v[[x]] %in% c('dateInput', 'dateRangeInput')) {
            list(type=inputs.v[[x]], value=as.character(input.vals[[x]]))
        } else list(type=inputs.v[[x]], value=input.vals[[x]])
    })
    # transfer input ids
    names(inputs.l) <- names(inputs.v)
    inputs.l
}
