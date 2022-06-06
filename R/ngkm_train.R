"%!in%" <- Negate("%in%")

#' ngkm_train
#'
#' Trains an ngk model.
#' Warning: can be very intense on your computer depending on the numbers you select.
#'
#' @param token_object A token object from quanteda to be transformed into a dfm.
#' @param ngrams The maximum number of ngrams to used for training. It is
#' important to notice that if you train a model with ngrams = 5, the model will
#' be created with 4 predictors and 1 predicter tokens.
#' @param lambda The rate at which a prediction probability will be penalized for
#' having less than the maximum number of ngrams as predictors. The penalization
#' is exponential to the amount of missing matching tokens
#' @param k Any ngram that appears k or less times in the dataset is going to be
#' removed from the model
#' @param npred the number of prediction the model is going to return
#' @param no_pred_list a vector containing words that might be used as a predictor,
#' but will never be predicted by the model
#' @return A list of tokens
#' @export
ngkm_train <- function(token_object, ngrams = 2, lambda = 0.4, k = 0, npred = 3,
    no_pred_list = c("<unk>", "<no>", "<url>")) {

    ## Vars prep
    new.cols <- paste("word_", 0:(ngrams-1), sep="")
    new.cols[new.cols=="word_0"] <- "word"
    independent_cols <- rev(setdiff(new.cols, "word"))

    ## Creating prediction table
    pred_table <- dfm(tokens_ngrams(token_object, n = 1:ngrams))
    pred_table <- featfreq(pred_table)

    pred_table <- data.frame(pred_table)
    names(pred_table) <- "frequency"
    pred_table["full_token"] <- row.names(pred_table)

    pred_table <- as.data.table(pred_table)

    pred_table[, paste("V", 1:ngrams, sep="") := tstrsplit(full_token, "_", fixed = TRUE, fill = "<NA>")]
    pred_table[, full_token := NULL]
    pred_table[, ngram := rowSums(.SD != "<NA>"), .SDcol = 2:(ngrams+1)]
    for (i in 1:ngrams) {
        for (j in 1:ngrams) {
            sd <- ((j<i)*i+(j>=i)*(j-i+1))+1
            pred_table[ngram == j, new.cols[i] := .SD, .SDcol = sd]
        }
    }
    pred_table[, paste("V", 1:ngrams, sep="") := rep(NULL, ngrams)]

    setcolorder(pred_table, append(rev(new.cols), "frequency"))


    pred_table[, prob := frequency/sum(frequency), by = independent_cols]

    pred_table <- subset(pred_table, frequency > k)
    pred_table <- subset(pred_table, word %!in% no_pred_list)
    pred_table <- pred_table[, head(.SD, npred), by = independent_cols]

    pred_table[, adjprob := prob * (lambda ^ (ngrams - ngram))]

    setorder(pred_table, -adjprob)

    ## additional info
    input_dictionary <- unique(unlist(pred_table[,0:(ngrams-1)]))

    cutpoint <-  max(nchar(input_dictionary))*5*ngrams

    ## assembling model
    model <- list(pred_table, input_dictionary, cutpoint, ngrams, lambda, k, npred)
    names(model) <-  c("pred_table", "input_dictionary", "cutpoint", "ngrams", "lambda", "k", "npred")

    ## returning
    return(model)
}

#' ngkmodel
#'
#' A model trained using about 3.2 million documents. The model was trained with
#' ngrams = 5, lambda = 0.75, k = 3 an npred = 6.
#'
#' @format a ngkm model
#' @source \url{https://github.com/Harnkal/DS_CapstoneProject}
"ngkmodel"
