post_vec <- function(file_name_vec){
  n <- length(file_name_vec)
  corrected_text <- vector("list", length = (n+1))
  
  for(i in 1:n){
    current_file_name <- sub(".txt","",file_name_vec[i])
    current_tesseract_txt <- 
      readLines(paste("../data/tesseract/",current_file_name,".txt",sep=""), warn=FALSE)
    tesseract_vec <- str_split(paste(current_tesseract_txt, collapse = " ")," ")[[1]]
    tesseract_vec <- tesseract_vec %>% removePunctuation() %>% tolower()
    
    #loop through tesseract_vec, create key for each word, store the word as value
    dic <- list()
    for(word in tesseract_vec){
      if(is.null(dic[[word]])){
        dic[[word]] = word
      }
    }
    
    #loop through corrected_word, for each key in dic, substitute the value
    for(key in names(corrected_word[[i]])){
      for(dic_key in names(dic)){
        if(key == dic_key){ 
          dic[[dic_key]] = corrected_word[[i]][[key]]
          break
        }
      }
    }
    
    #loop through tesseract_vec, create a vector containing word after correction
    for(words in tesseract_vec){
      corrected_text[[i]] <- c(corrected_text[[i]], dic[[words]])
    }
  }
  return(corrected_text)
}

