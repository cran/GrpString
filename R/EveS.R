#' @export
EveS <-
  function(eves.vec, eveName.vec, char.vec){
    
    ##### 1. number of eveNames in the vector
    
    eveName_num <- length(eves.vec)
    
    
    ##### 2. replace eveNames to characters

    # 2.0 make sure elements in conversion key are character
    eveName.vec <- as.character(eveName.vec)
    char.vec <- as.character(char.vec)
    
    # 2.1 replace   
    eveName_to_char.vec <- plyr::mapvalues(eves.vec, eveName.vec, char.vec, warn_missing = FALSE)
    
    
    # 2.2 remove NA
    eveName_to_char.vec1 <- plyr::mapvalues(eveName_to_char.vec, NA, "", warn_missing = FALSE)  
    
    
    ##### 3. combine characters to a string 
    
    # 3.1 combine chars to a string 
    char_to_string <- paste(eveName_to_char.vec1, collapse = "")
    
    # 3.2 check the sum of the lengths of strings, to see whether it matches the number in 1. 
    #     This does not apply to eveNames of single characters converting to char
    string_lengths <- nchar(char_to_string)
    
    
    return(char_to_string)
    
    if (string_lengths > eveName_num){
      stop('One or more event names are not replaced by characters. Please check the output vector and your char file')
    } else {
      cat('All the event Names have been replaced to chars and the strings are in the vector.\n')
    }
  }
