


#' uiSetSnapshot
#' 
#' Bulk Set UI Element Values Given A Named List Of Input Types And Values
#' @param snapshot A Named List Of Input Types And Values.
#' @param sess The shiny \code{Session} object necessary to update values.
#' @return Returns a list of update results.
#' @author Jon Katz
#' @examples
#' \dontrun{
#' # Must be called within a shiny app
#' ui <- fluidPage(
#'     textInput("main", "Plot title:"),
#'     numericInput("randn", 'Number of Random Values:')
#'     actionButton('reset', 'Previous Plot')
#'     plotOutput('outplot')
#' )
#' 
#' server <- function(input, output, session) {
#'     snapshot <- NA
#'     
#'     observe({
#'         lastPlot()
#'     })
#'     
#'     lastPlot <- reactive({
#'         input$reset
#'         if(!is.na(snapshot)) uiSetSnapshot(snapshot, session)
#'         NULL
#'     })
#' 
#'     output$outplot <- renderPlot({
#'         input.funs <- c(main='textInput', randn='numericInput')
#'         snapshot <<- uiTakeSnapshot(input.funs, environment())
#'         values <- rnorm(input$randn)
#'         plot(values, main=input$main)
#'     })
#' } 
#' }
#' 
#' @keywords misc
#' @export
#' 
uiSetSnapshot <- function(snapshot, sess){
    fiErr <- function(...){"Can't update 'fileInput' widgets."}
    lapply(1:length(snapshot), function(x) {
        # Deconstruct the list for inputId, input type, and value
        func <- snapshot[[x]][['type']]
        id <- names(snapshot)[x]
        val <- snapshot[[x]][['value']]
        # Reconstruct appropriate argument list
        if(func %in% c('textInput', 'numericInput', 'checkboxInput', 'dateInput', 'sliderInput')) { 
            al <- list(session=sess, inputId=id, value=val)
        } else if(func %in% c('selectInput', 'checkboxGroupInput', 'radioButtons')) {
            al <- list(session=sess, inputId=id, selected=val)
        } else if(func == 'dateRangeInput') {
            al <- list(session=sess, inputId=id, start=value[1], end=value[2])
        }
        # Link input function with correct update function
        uf <- switch(func, 
            textInput=shiny::updateTextInput, 
            numericInput=shiny::updateNumericInput, 
            checkboxInput=shiny::updateCheckboxInput, 
            selectInput=shiny::updateSelectInput,
            checkboxGroupInput=shiny::updateCheckboxGroupInput, 
            radioButtons=shiny::updateRadioButtons,
            fileInput=fiErr
        )
        # Safeguard the call constructor to avoid using NULL as uf
        if(exists('al')) do.call(uf, al)
    })
}
