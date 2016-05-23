
#' Create a series of label-less inputs in a table envrironment with rownames and colnames.
#' @param inputId The inputId for the table.
#' @param label The label to display for the table.
#' @param nrow Number of rows the table will display.
#' @param ncolNumber of columns the table will display.
#' @param dimnames List of names for the table dimensions. rownames may be named, name for colnames will not be used.
#' @param col.inputs Named list of input types, with one element per column. Sublist elements should be the named arguments to each input type. The shinyAddOn 'inline' inputs are recommended (inumericInput, itextInput, iradioButtons).
#' @param class Optional class to assign to the table (for css formatting).
#' @export
#' @author Jon Katz
#' @examples
#'tableInput(inputId='test', label='test table', nrow=4, ncol=2, 
#'    dimnames = list(
#'        rows=c(paste('row', 1:4)), 
#'        c(paste('col', 1:2))
# '   ),
#'    col.inputs <- list(
#'        inumericInput=list(value=NA),
# '       iradioButtons=list(choices=c('yes', 'no'))
# '   )
#')
#' @keywords misc


tableInput <- function(inputId, label, nrow, ncol, dimnames, col.inputs, class=NULL) {
    colfun <- rep(names(col.inputs), nrow)
    fun.args <- rep(col.inputs, nrow)
    indices <- which(is.na(matrix(NA, nrow, ncol)), arr.ind=TRUE)
    indices <- paste0(indices[,1], '_', indices[,2])
    inputIds <- paste0(inputId, '_', indices)
    inputIds <- c(t(matrix(inputIds, nrow, ncol)))
    
    col.names <- paste0('<th>', dimnames[[2]], '</th>')
    rn <- names(dimnames[[1]])
    if(is.null(rn)) rn <- ''
    row.names <- paste0('<td>', c(rn, dimnames[[1]]), '</td>')
    
    fun.args <- lapply(1:length(fun.args), function(x) c(list(inputId=inputIds[x], label=''), fun.args[[x]]))
    table.data <- lapply(1:(nrow*ncol), function(x) paste0('<td>', do.call(colfun[x], fun.args[[x]]), '</td>'))
    table.data <- matrix(unlist(table.data), nrow, ncol, byrow=TRUE)
    table.data <- rbind(col.names, table.data)
    table.data <- cbind(row.names, table.data)
    table.html <- sapply(1:(nrow+1), function(x) paste0('  <tr>\n    ', paste0(table.data[x,], collapse='\n    '), '\n  </tr>'))
    
    paste0('<table class="', class,'">', paste0(table.html, collapse='\n  '), '</table>')
}    
    
    
#    HTML({paste0("
#        <table>
#            <tr>
#                <td>", itextInput('test_1_1', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#                <td>", itextInput('test_1_2', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#                <td>", itextInput('test_1_3', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#            </tr>
#            <tr style='width:15em;'>
#                <td>", itextInput('test_2_1', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#                <td>", itextInput('test_2_2', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#                <td>", itextInput('test_2_3', '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style='margin-bottom:0px;'), "</td>
#            </tr>
#       </table>"
#   )})
#}


#tableInput <- function(inputId, label, nrow, ncol, dimnames, column.width) {
#    row.names <- dimnames[[1]]
#    col.names <- dimnames[[2]]
#    columnnames <- shiny::column(column.width[1],
#        shiny::p(style='padding:2.5em 1em 0em 0em;text-align:right;', row.names[1]),
#        lapply(2:nrow, function(x) shiny::p(style='padding:0.59em 1em 0em 0em;text-align:right;', row.names[x]))
#    )
#    columninputs <- lapply(1:ncol, function(x) {
#        shiny::column(column.width[x+1],
#            div(style='clear:both;',
#                shiny::p(style='text-align:center;', col.names[x]),
#                lapply(1:nrow, function(y) itextInput(paste0(inputId,'_',y,'_',x), '',  label.style='float:left;padding:0.5em 1em 0.5em 0em;width=3em;text-align:right;', input.style='width:100%;border-radius:0px;', container.style="margin-bottom:0px;"))#, container=shiny::div))
#            )
#        )
#    })
#    
#    shiny::div(class = "form-group shiny-input-container", 
#        shiny::tags$label(label, `for` = inputId),
#        shiny::div(style='clear:both;', list(columnnames, columninputs))
#    )
#}



