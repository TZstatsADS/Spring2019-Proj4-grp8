#divide_string function: divide text into strings segments using intersections words as anchors
divide_string <- function(vec, intersect_vec){
  position <- c(0,which(vec %in% intersect_vec), length(vec))
  string_list = list()
  for(i in 1:(length(position)-1)){
    str = character(0)
    if(position[i+1] - position[i] > 1 ) {
      for(j in (position[i]+1) : (position[i+1]-1)){
        str = c(str, vec[j])
      } 
    }
    
    string_list[[i]] = str
  }
  return(string_list)
}


#count_error_char function: input two lists of strings, count total number of error characters
count_error_char <- function(list1, list2){
  if(length(list1) == length(list2)){
    no_of_error <- 0
    for(i in 1:length(list1)){
      no_of_error <- no_of_error + sum(diag(adist(list1[[i]], list2[[i]])))
    }
  }
  else{ no_of_error <- NA }
  
  return(no_of_error)
}

#performance_measure function
performance_measure <- function(file_name_vec){
  n <- length(file_name_vec)
  word_recall_before <- numeric(n)
  word_pres_before <- numeric(n)
  word_recall_post <- numeric(n)
  word_pres_post <- numeric(n)
  char_recall_before <- numeric(n)
  char_pres_before <- numeric(n)
  char_recall_post <- numeric(n)
  char_pres_post <- numeric(n)
  
  for(i in 1:n){
    current_file_name <- sub(".txt","",file_name_vec[i])
    
    current_ground_truth_txt <- 
      readLines(paste("../data/ground_truth/",current_file_name,".txt",sep=""), warn=FALSE)
    
    current_tesseract_txt <- 
      readLines(paste("../data/tesseract/",current_file_name,".txt",sep=""), warn=FALSE)
    
    ground_truth_vec <- str_split(paste(current_ground_truth_txt, collapse = " ")," ")[[1]]
    tesseract_vec <- str_split(paste(current_tesseract_txt, collapse = " ")," ")[[1]]
    ground_truth_vec <- ground_truth_vec %>% removePunctuation() %>% tolower()
    tesseract_vec <- tesseract_vec %>% removePunctuation() %>% tolower() 
    tesseract_post_vec <- corrected_text[[i]]
    
    ### word level ###
    old_intersect_vec<- vecsets::vintersect(ground_truth_vec, tesseract_vec)
    new_intersect_vec <- vecsets::vintersect(ground_truth_vec, tesseract_post_vec)
    
    word_recall_before[i] <- length(old_intersect_vec)/length(ground_truth_vec)
    word_pres_before[i] <- length(old_intersect_vec)/length(tesseract_vec)
    word_recall_post[i] <- length(new_intersect_vec)/length(ground_truth_vec)
    word_pres_post[i] <- length(new_intersect_vec)/length(tesseract_post_vec)
    
    
    ### character level ###
    # extract non-duplicate words
    truth_unique <- ground_truth_vec[which(!(duplicated(ground_truth_vec)|
                                               duplicated(ground_truth_vec, fromLast=TRUE)))]
    
    tesseract_unique <- tesseract_vec[which(!(duplicated(tesseract_vec)|
                                                duplicated(tesseract_vec, fromLast=TRUE)))]
    
    tesseract_post_unique <- tesseract_post_vec[which(!(duplicated(tesseract_post_vec)|
                                                          duplicated(tesseract_post_vec, fromLast=TRUE)))]
    
    #intersection of unique words
    char_old_intersect_vec<- vecsets::vintersect(truth_unique, tesseract_unique)
    char_new_intersect_vec<- vecsets::vintersect(truth_unique, tesseract_post_unique)
    
    #divide into string segments
    truth_string <- divide_string(ground_truth_vec, char_old_intersect_vec)
    tesseract_string <- divide_string(tesseract_vec, char_old_intersect_vec)
    truth_post_string <- divide_string(ground_truth_vec, char_new_intersect_vec)
    tesseract_post_string <- divide_string(tesseract_post_vec, char_new_intersect_vec)
    
    #count error characters
    old_error <- count_error_char(truth_string, tesseract_string)
    new_error <- count_error_char(truth_post_string, tesseract_post_string)
    
    char_recall_before[i] <- 1 - old_error/sum(nchar(ground_truth_vec))
    char_pres_before[i] <- 1 - old_error/sum(nchar(tesseract_vec))
    char_recall_post[i] <- 1 - new_error/sum(nchar(ground_truth_vec))
    char_pres_post[i] <- 1 - new_error/sum(nchar(tesseract_vec))
  }
  return(list(word_recall_before = word_recall_before,
              word_pres_before = word_pres_before,
              word_recall_post = word_recall_post,
              word_pres_post = word_pres_post,
              char_recall_before = char_recall_before,
              char_pres_before = char_pres_before,
              char_recall_post = char_recall_post,
              char_pres_post = char_pres_post
              ))
}
