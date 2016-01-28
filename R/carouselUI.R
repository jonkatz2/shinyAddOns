#' carouselUI
#'  
#' Make a dynamic carousel for use with shiny::renderUI() and shiny::uiOutput(). 
#'  
#' @param slides A list of HTML tag elements, e.g. \code{list(shiny::div(), shiny::div())}.
#' @param data.interval Milliseconds to pause before auto advancing (numeric).
#' @param data.ride Either \code{carousel} or \code{NA}; unclear if this works.
#' @return HTML code with class \code{shiny.tag} that can be passed to \code{uiOutput}. 
#' @author Jon Katz
#' @note Adapted by JEK from \url{https://github.com/dcurrier/carouselPanel/blob/master/carouselPanel.R}
#' @examples
#' c.slides <- lapply(1:5, function(x) div(h3(style='text-align:center;',paste('Slide', x))))
#' carouselUI(slides=c.slides, data.interval=5000, data.ride=FALSE)
#' 
#' \dontrun{
#' /*     place in www/css/styles.css:    */
#'     .carousel-indicators li {
#'         background-color: #DDD;
#'         background-color: rgba(70,70,70,.25);
#'     }
#'     .carousel-indicators .active {
#'         background-color: #999;
#'     }
#'     .carousel-indicators {
#'         bottom: 15px !important;
#'         top: auto;
#'         list-style: none outside none;
#'         margin: 0;
#'         position: absolute;
#'         right: 70px;
#'         z-index: 5;
#'     }
#'     .carousel-control {
#'         opacity: 0.2;
#'         border: none;
#'         top: auto;
#'     }
#'     .item {
#'         padding: 0px 17px;
#'     }
#' }
#' @keywords manip
#' @export

carouselUI <- function(slides, data.interval=10000, data.ride="carousel"){
    if(!is.list(slides)) stop("'slides' must be a list in which each element becomes a slide.")
    # Each slide corresponds to one level of a list
    active <- list(shiny::div(class="item active", slides[[1]]))
    items <- lapply(slides[2:length(slides)], function(x) {
        shiny::div(class="item", x)
    })
    # A random string that always starts with a letter
    carouselID <- rName()
    #Set up carousel
    shiny::div(id=paste0("carousel-", carouselID), class="carousel slide", "data-interval"=as.character(data.interval), "data-ride"=tolower(as.character(data.ride)), 
        # Carousel Inner Div - contains the content to display            
        shiny::div(class="carousel-inner", style="padding-bottom:40px;", active, items),

        # Carousel controls
        shiny::a(class="left carousel-control",
            "data-slide"="prev",
            href=paste0("#carousel-", carouselID),
            style="background: transparent; color: #000",
            shiny::HTML(paste0("<i class='fa fa-chevron-left'></i>")) 
        ),

        shiny::a(class="right carousel-control",
            "data-slide"="next",
            href=paste0("#carousel-", carouselID),
            style="background: transparent; color: #000",
            shiny::HTML(paste0("<i class='fa fa-chevron-right'></i>")) 
        ),

        # Generate the carousel indicators
        shiny::HTML("<ol class='carousel-indicators'>"),
        shiny::tag('li', list(class='active', "data-slide-to"=paste(0),
            "data-target"=paste0("#carousel-", carouselID))
        ),
        mapply(function(i){
           list(shiny::tag('li', list(class='', "data-slide-to"=paste(i),
              "data-target"=paste0("#carousel-", carouselID))) )
        }, 1:(length(slides)-1), SIMPLIFY=F, USE.NAMES=F),
        shiny::HTML("</ol>")
    )
}



