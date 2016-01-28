# Make a dynamic carousel for use with renderUI() and uiOutput() 
# Adapted by JEK from https://github.com/dcurrier/carouselPanel/blob/master/carouselPanel.R

# slides is a list of HTML tag elements, e.g. list(div(), div())
# data.interval is a number, in milliseconds
# data.ride is either "carousel" or NA; unclear if this works 

#/*     in www/css/styles.css:    */
#    .carousel-indicators li {
#        background-color: #DDD;
#        background-color: rgba(70,70,70,.25);
#    }
#    .carousel-indicators .active {
#        background-color: #999;
#    }
#    .carousel-indicators {
#        bottom: 15px !important;
#        top: auto;
#        list-style: none outside none;
#        margin: 0;
#        position: absolute;
#        right: 70px;
#        z-index: 5;
#    }
#    .carousel-control {
#        opacity: 0.2;
#        border: none;
#        top: auto;
#    }
#    .item {
#        padding: 0px 17px;
#    }

carouselUI <- function(slides, data.interval=10000, data.ride="carousel"){
    if(!is.list(slides)) stop("'slides' must be a list in which each element becomes a slide.")
    # Each slide corresponds to one level of a list
    active <- list(div(class="item active", slides[[1]]))
    items <- lapply(slides[2:length(slides)], function(x) {
        div(class="item", x)
    })
    # A random string that always starts with a letter
    carouselID <- rName()
    #Set up carousel
    div(id=paste0("carousel-", carouselID), class="carousel slide", "data-interval"=as.character(data.interval), "data-ride"=as.character(data.ride), 
        # Carousel Inner Div - contains the content to display            
        div(class="carousel-inner", style="padding-bottom:40px;", active, items),

        # Carousel controls
        a(class="left carousel-control",
            "data-slide"="prev",
            href=paste0("#carousel-", carouselID),
            style="background: transparent; color: #000",
            HTML(paste0("<i class='fa fa-chevron-left'></i>")) 
        ),

        a(class="right carousel-control",
            "data-slide"="next",
            href=paste0("#carousel-", carouselID),
            style="background: transparent; color: #000",
            HTML(paste0("<i class='fa fa-chevron-right'></i>")) 
        ),

        # Generate the carousel indicators
        HTML("<ol class='carousel-indicators'>"),
        tag('li', list(class='active', "data-slide-to"=paste(0),
            "data-target"=paste0("#carousel-", carouselID))
        ),
        mapply(function(i){
           list(tag('li', list(class='', "data-slide-to"=paste(i),
              "data-target"=paste0("#carousel-", carouselID))) )
        }, 1:(length(slides)-1), SIMPLIFY=F, USE.NAMES=F),
        HTML("</ol>")
    )
}



