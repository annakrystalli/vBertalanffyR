#' Title catch_dat_expand
#'
#' Expand data.frame by replicating rows according to `CANoAtLngt`
#' @param x data.frame containing survay data
#' @param quarter column name containing the quarter of the survey as string
#' @param age column name containing fish age class as string
#' @param length_class column name containing fish length class as string
#' @param CANoAtLngt
#'
#' @return expanded data.frame with rows replicated according to the value `CANoAtLngt`
#' @export
#'
#' @examples
vbffy_dat <- function(x, quarter, age, length_class, CANoAtLngt, spawn_loc = NULL, spawn_spread = NULL) {
    x <- as.data.frame(x)
    x <- x[rep(seq(nrow(x)), x[,CANoAtLngt]), c(quarter, age, length_class)]
    names(x) <- c("quarter", "age", "length_class")
    x$qs <- (x$quarter==4)*0.75
    x$temp <- x$age == 0
    x <- x[order(x$temp),]


    vb_dat <- list(t = x$age,
                   l = x$length_class,
                   N = nrow(x),
                   N1 = sum(x$temp == FALSE),
                   N4 = sum(x$temp == TRUE),
                   q = x$quarter,
                   mu_a = spawn_loc * 2 * pi - pi,
                   sigma_a = spawn_spread)


    return(vb_dat)
}
