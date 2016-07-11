


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
        } else al <- list()
        # Link input function with correct update function
        uf <- switch(func, 
            textInput=shiny::updateTextInput, 
            numericInput=shiny::updateNumericInput, 
            checkboxInput=shiny::updateCheckboxInput, 
            selectInput=shiny::updateSelectInput,
            checkboxGroupInput=shiny::updateCheckboxGroupInput, 
            radioButtons=shiny::updateRadioButtons,
            dateInput=shiny::updateDateInput,
            dateRangeInput=shiny::updateDateRangeInput,
            fileInput=fiErr
        )
        # Safeguard the call constructor to avoid using NULL as uf
        if(exists('al')) do.call(uf, al)
    })
}
