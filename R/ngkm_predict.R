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

    for (i in 1:(model$ngrams-1)) {
        model$pred_table <- model$pred_table[get(search_fields[i]) == pred_tokens[i]|get(search_fields[i])== "<NA>",]
    }

    model$pred_table <- setorder(model$pred_table, -adjprob)

    model$pred_table <- model$pred_table[, head(.SD, 1), by = "word"]

    model$pred_table <- head(model$pred_table, model$npred)

    return(model$pred_table)
}
