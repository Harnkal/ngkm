#' ngkm_prep
#'
#' Just a useful tool to replace/remove "urls", "no" and "_" from the text. I 
#' wish I had the stackoverflow treads to credit the kind people that wrote those 
#' regular expressions but I lost them.
#'
#' @param input A string that will be have their what(s) removed
#' @param what An "url", "no", "_" or any combination of these passed as a vector.
#' @param replacement A string or a list of strings that are going to be used to 
#' replace the whats found in your text. If more than one, they should be listed 
#' in the same order.
#' @return A list of tokens
#' @export

ngkm_replace <- function(input, what = c("url", "no", "_"), replacement = c("aurlthatwillbereplaced", "anothatwillbereplaced", " ")) {
    
    if (length(replacement) != length(what)){
        stop("length of replacement must be equal to the length of what")
    }
    
    if ("url" %in% what) {
        regex <- "(((((http|ftp|https|gopher|telnet|file|localhost):\\/\\/)|(www\\.)|(xn--)){1}([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-])?)|(([\\w_-]{2,200}(?:(?:\\.[\\w_-]+)*))((\\.[\\w_-]+\\/([\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-])?)|(\\.((org|com|net|edu|gov|mil|int|arpa|biz|info|unknown|one|ninja|network|host|coop|tech)|(jp|br|it|cn|mx|ar|nl|pl|ru|tr|tw|za|be|uk|eg|es|fi|pt|th|nz|cz|hu|gr|dk|il|sg|uy|lt|ua|ie|ir|ve|kz|ec|rs|sk|py|bg|hk|eu|ee|md|is|my|lv|gt|pk|ni|by|ae|kr|su|vn|cy|am|ke))))))(?!(((ttp|tp|ttps):\\/\\/)|(ww\\.)|(n--)))"
        input <- gsub(regex, replacement[what == "url"], input, perl = TRUE)
    }
    
    if ("no" %in% what) {
        regex <- "([0-9])+(\\/([0-9])+)?(\\.([0-9])+)?"
        input <- gsub(regex, replacement[what == "no"], input, perl = TRUE)
    } 
    
    if ("_" %in% what) {
        regex <- "_"
        input <- gsub(regex, replacement[what == "_"], input, perl = TRUE)
    }
    
    return(input)
}