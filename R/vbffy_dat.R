#' Title catch_dat_expand
#'
#' Create vBffy data.list to supply to vbffy_model function
#'
#' @param x data.frame of survey data
#' @param quarter column name containing the quarter of the survey as string
#' @param age column name containing fish age class as string
#' @param length_class column name containing fish length class as string
#' @param CANoAtLngt
#' @param spawn_loc species spawning location parameter (describing von Mises distribution)
#' @param spawn_spread species spawning spread parameter (describing von Mises distribution)
#'
#' @return expanded data.frame with rows replicated according to the value `CANoAtLngt`
#' @export
#'
#' @examples
vbffy_dat <- function(x, survey_toy, age, length, replicates, spawn_loc = NULL, spawn_spread = NULL) {
    x <- as.data.frame(x)
    x <- x[rep(seq(nrow(x)), x[,replicates]), ]
    x$age0 <- x[,age] == 0
    x <- x[order(x$age0),]

    vb_dat <- list(age = x[,age],
                   length = x[,length],
                   N = nrow(x),
                   N1 = sum(x$age0 == FALSE),
                   N0 = sum(x$age0 == TRUE),
                   toy = time_of_year(survey_toy, x),
                   mu_a = spawn_loc * 2 * pi - pi,
                   sigma_a = spawn_spread)
    attr(vb_dat, "class") <- "vb_dat"

    return(vb_dat)
}

# converts from quarter, month or date to toy (proportional yearly time elapsed)
time_of_year <- function(toy, x) {
    switch(toy$type,
           quarter = tempbin2prop(toy, x),
           month = tempbin2prop(toy, x),
           date = date2prop(toy, x))
}

# converts quarter or month data to toy (proportional yearly time elapsed)
tempbin2prop <- function(toy, x){
    if(!toy$location %in% c("start", "midpoint", "end")){
        stop('toy$location "', toy$location, '" not one of "start", "midpoint", "end"')
        }
    locate <- c(start = -1, midpoint = -0.5, end = 0)
    switch(toy$type,
           quarter = x[,toy$name] + locate[toy$location]/4,
           month = x[,toy$name] + locate[toy$location]/12)
    }
# converts date to toy (proportional yearly time elapsed)
date2prop <- function(toy, x) {
    if(class(x[,toy$name])[1] %in% c("Date", "POSIXlt", "POSIXct")){
        as.POSIXlt(x[,toy$name])$yday/365
    }else{
        if(!"format" %in% names(toy)){
            stop("date format string missing from toy.")
        }else{
            as.POSIXlt(x[,toy$name], format = toy$format)$yday/365
        }
    }
}
