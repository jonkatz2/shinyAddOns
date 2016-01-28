# Make a dynamic carousel for use with renderUI() and uiOutput() 
# Adapted by JEK from https://github.com/dcurrier/carouselPanel/blob/master/carouselPanel.R

# \dots are a list of HTML tag elements, e.g. list(div(), div())
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

carouselUI <- function(..., data.interval=10000, data.ride="carousel"){
    contents <- list(...)
    contents <- contents[[1]]
    # Each frame corresponds to one level of a list
    active <- list(div(class="item active", contents[[1]]))
    items <- lapply(contents[2:length(contents)], function(elm) {
        div(class="item", elm)
    })
    # A random string that always starts with a letter
    carouselID <- paste0(sample(c(letters,LETTERS), 1), paste0(sample(c(letters,LETTERS,rep(0:9,3)), 15, replace=TRUE),collapse=""),collapse="")
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
        }, 1:(length(contents)-1), SIMPLIFY=F, USE.NAMES=F),
        HTML("</ol>")
    )
}



