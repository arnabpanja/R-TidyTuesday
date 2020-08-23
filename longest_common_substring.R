# Function to find the longest common substring between two strings 

fn_strcommon <- function(x1 = "", x2 = "") {
  i_list1 <- strsplit(x = x1, split = "", fixed = TRUE)
  len_list1 <- length(i_list1[[1]]) # length of first string
  i_list2 <- strsplit(x = x2, split = "", fixed = TRUE)
  len_list2 <- length(i_list2[[1]]) # length of second string
  

  first_match_index = vector(mode = "integer", length = 1)
  first_match_found = vector(mode = "logical", length = 1)

  # Initialize two copies of vectors for storing the strings
  
    if(len_list1[[1]] >= len_list2[[1]]) {
    final_combo_string = vector(mode = "character", len_list1[[1]])
    final_combo_string2 = vector(mode = "character", len_list1[[1]])
  }
  if(len_list1[[1]] < len_list2[[1]]) {
    final_combo_string = vector(mode = "character", len_list2[[1]])
    final_combo_string2 = vector(mode = "character", len_list2[[1]])
  }
  
  final_combo_string2 = ""
  
  
  if(len_list1[[1]] <= len_list2[[1]]){ # when first string is shorter or equal to second string
    for (i in seq(i_list1[[1]])) {
      first_match_found[1] = FALSE #initialize the logical vector to false at start
      
      # search for the first match 
      for (j in 1:len_list2[[1]]){
        
        if (i_list1[[1]][[i]] == i_list2[[1]][[j]]){
          first_match_index[1] = j
          first_match_found[1] = TRUE
          break
        }
        j <-  j + 1
      }
      
      if(isTRUE(first_match_found[[1]])){
        
        m <-  i
        
        insert_counter <-  1
        
        final_combo_string = "" #intialize the final string to all quotes 
        
        
        for(k in first_match_index[[1]]:len_list2[[1]]) {
          
          
          if(i_list1[[1]][[m]] == i_list2[[1]][[k]]) {
            final_combo_string[[insert_counter]] = i_list2[[1]][[k]]
            insert_counter = insert_counter + 1
          } else {
            break
          }
          
          k <-  k + 1
          m <-  m + 1
          
          if(m > len_list1[[1]] || k > len_list2[[1]]){
            break 
          }
          
        }
        
        
        if(length(final_combo_string) > length(final_combo_string2))
          final_combo_string2 <- final_combo_string

      }
      
      i  <-  i + 1
    }
  } else { # when the first string is longer than the second string 
    
    for (i in seq(i_list2[[1]])) {
      first_match_found[1] = FALSE #initialize the logical vector to false at start
      
      # search for the first match 
      for (j in 1:len_list1[[1]]){
        
        if (i_list2[[1]][[i]] == i_list1[[1]][[j]]){
          first_match_index[1] = j
          first_match_found[1] = TRUE
          break
        }
        j <-  j + 1
      }
      
      if(isTRUE(first_match_found[[1]])){
        
        m <-  i
        
        insert_counter <-  1
        
        final_combo_string = "" #intialize the final string to all quotes 
        
        
        for(k in first_match_index[[1]]:len_list1[[1]]) {
          
          
          if(i_list2[[1]][[m]] == i_list1[[1]][[k]]) {
            final_combo_string[[insert_counter]] = i_list1[[1]][[k]]
            insert_counter = insert_counter + 1
          } else {
            break
          }
          
          k <-  k + 1
          m <-  m + 1
          
          if(m > len_list2[[1]] || k > len_list1[[1]]){
            break 
          }
          
        }
        
        
        if(length(final_combo_string) > length(final_combo_string2))
          final_combo_string2 <- final_combo_string
        
        
      }
      
      i  <-  i + 1
    }
  }
  
  
  if(final_combo_string2[[1]] != "") {
    #print(paste("Longest Common Substring = ", paste0(final_combo_string2, collapse = ""), sep = ""))
    return(paste0(final_combo_string2, collapse = ""))
  } else {
    #print("No Common Substring")
    return("No Common Substring")
  }
}

# Testing Scripts ------------------------------------------
# Create two vectors with all different combinations to test 
# v1 and v2 -----------------------------------------------

v1 <- c("Data Science", 
        "Data Science", 
        "Data Wrangling", 
        "Data Munging", 
        "Data Munging", 
        "Data", 
        "", 
        "Data Analysis", 
        "", 
        "Data Analysis")
v2 <- c("Science", 
        "Data", 
        "Wrangking", 
        "Munging", 
        "Data", 
        "Data", 
        "", 
        "", 
        "Data Analysis", 
        "Data Analysis")

# Call the function with element and check the results -----------
purrr::map2_chr(.x = v1, .y = v2, .f = fn_strcommon)




