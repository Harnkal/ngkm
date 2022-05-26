#' ngkm_predict
#'
#' This function returns predictions given an input text and a model. The number
#' of predictions will depend on the model
#'
#' @param input A string that will be used as predictor for the model.
#' @param model An ngk model.
#' @return A data table with the predictions and their statistics.
#' @export
ngkm_predict <- function(input, model) {
    pred_tokens <- c(rep("<NA>", model$ngrams-1), ngkm_prep(input, model)[[1]])

    pred_tokens <- tail(pred_tokens, model$ngrams-1)

    search_fields <- paste("word_", (model$ngrams-1):1, sep = "")

    pred <- model$pred_table

    for (i in 1:(model$ngrams-1)) {
        pred <- pred[get(search_fields[i]) == pred_tokens[i]|get(search_fields[i])== "<NA>",]
    }

    pred <- setorder(pred, -adjprob)

    pred <- pred[, head(.SD, 1), by = "word"]

    pred <- head(pred, model$npred)

    return(pred)
}
