#' Get complete conversation 
#' 
#' This function fetches all tweets that form part of a conversation 
#'
#' @param conversation_ids string conversation_id or vector of conversation ids 
#' @inheritParams get_all_tweets
#' @return a data frame
#' @export
#' 
get_conversations <- function(conversation_ids, ...){
    
    df.new <- data.frame()
    
    for (x in conversation_ids){
        
        new_data <- get_all_tweets(conversation_id = x, ...)
        
        if (nrow(df.new) == 0){
            if(nrow(new_data) > 0){
                df.new <- new_data
            }
        } else if (nrow(df.new) > 1){
            if(nrow(new_data) > 1){
                df.new <- dplyr::bind_rows(df.new, new_data)
            }
        }
        
        
        Sys.sleep(1)
    }
    
    return(df.new)
    
}