
#' Title
#'
#' @param vb_model
#' @param data
#' @param iter
#' @param warmup
#' @param chains
#' @param control
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vbffy_model <- function(vb_model = "vb", data, iter = 2000, warmup = floor(iter/2),
                        chains = 4, control = list(adapt_delta = 0.95), ...){

    fit_model <- rstan::stan(file = paste0(stan_dir, vb_model, ".stan"),
                  data = data, iter = iter, chains = chains,
                  control = control, ...)

    return(fit_model)
}
