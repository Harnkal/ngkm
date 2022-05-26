#' ngkm_prep
#'
#' This takes a raw string and tokenize it in a way it can be used to predict or
#' or to train an ngk model
#'
#' @param input A string that will be tokenized.
#' @param model An ngk model. If an ngk model is provided, only the last n tokens
#' are going to be returned, being n the number of ngrams you used to train the 
#' model decreased by 1.
#' @return A list of tokens
#' @export
ngkm_prep <- function(input, model = NULL, dictionary = NULL){
    if(!is.null(model)){
        input <- substr(input, nchar(input)-model$cutpoint, nchar(input))
    }
    
    input <- tolower(input)
    input <- ngkm_replace(input)
    
    tokens <- tokens(input, what = "word", remove_punct = TRUE,
        remove_symbols = TRUE, remove_numbers = TRUE, 
        remove_url = TRUE, remove_separators=TRUE,
        split_hyphens = TRUE, split_tags = TRUE)
    
    if(!is.null(model)){
        tokens <- tokens_select(tokens, startpos = -model$ngrams+1, endpos = -1)
    }
    
    tokens <- tokens_replace(tokens, "aurlthatwillbereplaced", "<url>")
    tokens <- tokens_replace(tokens, "anothatwillbereplaced", "<no>")
    
    if(!is.null(model)){
        unk <- unique(unlist(tokens))
        unk <- unk[unk %!in% model$input_dictionary]
        tokens <- tokens_replace(tokens, unk, rep("<unk>", length(unk)))
    }
    
    if(!is.null(dictionary)){
        unk <- unique(unlist(tokens))
        unk <- unk[unk %!in% dictionary]
        tokens <- tokens_replace(tokens, unk, rep("<unk>", length(unk)))
    }
    
    return(tokens)
}