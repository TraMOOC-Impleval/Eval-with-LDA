#!usr/bin/env Rscript

match_bilingual_topics <- function(num_topic_1, num_topic_2, num_keys, max_cost, 
                                   topic_keys_file_1, topic_keys_file_2, source_language, target_language)
{
  options(stringsAsFactors = FALSE)
  
  #ensures that the number of topics, the number of keys and the maximum BabelNet-cost are interpreted as numbers
  num_topic_1 <- as.numeric(num_topic_1)
  num_topic_2 <- as.numeric(num_topic_2)
  num_keys <- as.numeric(num_keys)
  max_cost <- as.numeric(max_cost)
  
  #creates a vector containing the words that don't need to be looked up on BabelNet, because their files have already been saved
  word_vector <<- readLines(con = "seen_words.txt", n = -1)
  
  #creates weighting vectors, in which the highest weight (20) is in first position (belongs to the highest ranking topic key), the second-highest weight (19) is in second position (belongs to the second-highest ranking topic key), etc.
  weighting_top1 <<- c(num_keys:1)
  weighting_top2 <<- c(num_keys:1)
  
  #creates an empty matrix for the similarity scores with the right number of rows and columns (both corresponding to the number of topics)
  similarity_matrix <<- matrix(nrow = num_topic_1, ncol = num_topic_2)
  
  #fills the empty spaces in the matrix with zeroes
  similarity_matrix[1:num_topic_1, 1:num_topic_2] <<- 0
  
  #creates an empty matrix to keep track of which words have already been matched before, with a number of rows equal to the number of topics (400) and a number of columns equal to the number of topic keys (20)
  matched_matrix <<- matrix(nrow = num_topic_2, ncol = num_keys)
  
  #creates two empty matrices - one for the source language (English) and one for the target language (Dutch) - for the topic keys with a number of rows equal to the number of topics (400) and a number of columns equal to the number of topic keys (20)
  overview_topics_1 <<- matrix(nrow = num_topic_1, ncol = num_keys)
  overview_topics_2 <<- matrix(nrow = num_topic_2, ncol = num_keys)
  
  read_files(topic_keys_file_1, topic_keys_file_2)
  find_translations_for_topic_keys(source_language, target_language, num_keys, max_cost)
  
  export_seen_words()
  export_topic_overviews(source_language, target_language, num_topic_1, num_topic_2)
  export_similarity_matrix(source_language, target_language, num_topic_1, num_topic_2)
  
  topic_match_vector <- match_topics()
  export_topic_match_vector(topic_match_vector, source_language, target_language)
}

read_files <- function(topic_keys_file_1, topic_keys_file_2)
{
  #opens the two files containing the topic keys as tables
  topic_keys_1 <<- read.table(file = topic_keys_file_1, quote = "") 
  topic_keys_2 <<- read.table(file = topic_keys_file_2, quote = "")
}

find_translations_for_topic_keys <- function(source_language, target_language, num_keys, max_cost)
{
  #loops through all of the source language topics
  for(a in 1:nrow(topic_keys_1))
  {
    #writes one line of source language topic keys to a vector
	topic <- unname(unlist(topic_keys_1[a, 3:{num_keys+2}]))
    
	#writes the number corresponding to the current source language topic to a separate variable
	topic_id <- a
	
	#writes the vector containing the source language topic keys for this topic to the right line in the new source language topic keys file
    overview_topics_1[a, 1:num_keys] <<- topic
    
	#writes one line of target language topic keys to a vector
    topic_2 <- unname(unlist(topic_keys_2[a, 3:{num_keys+2}]))
    
	#writes the vector containing the target language topic keys for this topic to the right line in the new target language topic keys file
	overview_topics_2[a, 1:num_keys] <<- topic_2
    
	#fills the match matrix with zeroes
    matched_matrix[1:nrow(matched_matrix), 1:num_keys] <<- 0
    
	#loops through all of the topic keys belonging to the current source language topic
    for(b in 1:length(topic))
    {
      #creates an empty vector for the translations of the current source language topic key
	  translations <- vector(length = 0)
      
	  #writes the current source language topic key to a separate variable
      word <- as.character(topic[b])
      
	  #checks whether the current source language topic key is a number (consists entirely of digits)
      if(grepl(pattern = "^[0-9]+$", x = word))
      {
        #changes the content of the topic key variable (previously a number) to "#int" (so all numbers are treated as being the same entity)
		word <- "#int"
		
        #calculates topic similarity using "#int" as input
		calculate_topic_similarity(word, topic_id, num_keys, b)
		
		#skips to the next iteration of the loop (skips to the next source language topic key)
        next
      }
      
	  #checks whether the current source language topic key is listed in the vector containing the topic keys for which target language translation files have already been saved
      if(word %in% word_vector)
      {
        #uses the topic key to compile the name of the file in which target language translations for that topic key are listed
		name_transl_input <- paste("Babelnet-translations_", word, ".txt", sep = "")
        
		#opens the file containing target language translations for the current source language topic key
		translation_input <- file(description = name_transl_input, open = "r")
        
		#writes the lines in the translations file to a vector
		translations <- readLines(con = translation_input, n = -1)
        
		close(translation_input)
        
		#checks whether the vector that should contain the target language translations has actual content
        if(length(translations) == 0)
        {
          #if the translations vector is empty, the source language topic key itself is used as input instead of target language translations of that topic key
		  calculate_topic_similarity(word, topic_id, num_keys, b)
        }
        else
        {
          #if the translations vector has content, the target language lemmas corresponding to the source language topic key are extracted
		  extract_lemmas(translations, topic_id, num_keys, b)
        }
      }
	  else
      {
        #compiles the command to invoke the Python script
		command_spec <- paste("python3 /vol/bigdata2/datasets2/TraMOOC/TopicModel/Scripts/babelnet_funcs_Dennis.py", 
                              word, source_language, target_language, max_cost, sep=" ")
        
        #invokes the Python script 
		translations <- system(command = command_spec, intern = TRUE)
        
		#opens a new output file for the target language translations of the source language topic keys from BabelNet
        name_transl_output <- paste("Babelnet-translations_", word, ".txt", sep = "")
        translation_output <- file(description = name_transl_output, open = "w")
        
		#writes the target language translations of the source language topic keys to the output file
		writeLines(text = translations, con = translation_output)
		
        close(translation_output)
        
		#adds the current source language topic key to the vector of words for which target language translation files have been saved
        word_vector <<- c(word_vector, word)
        
		#checks whether the vector that should contain the target language translations has actual content
        if(length(translations) == 0)
        {
          #if the translations vector is empty, the source language topic key itself is used as input instead of target language translations of that topic key
		  calculate_topic_similarity(word, topic_id, num_keys, b)
        }
        else
        {
          #if the translations vector has content, the target language lemmas corresponding to the source language topic key are extracted
		  extract_lemmas(translations, topic_id, num_keys, b)
        }
      }
    }
  }
}

extract_lemmas <- function(translations, topic_id, num_keys, key_id)
{
  #creates an empty vector for the target language lemmas, of a length equal to the length of the translations vector
  translated_lemmas <- vector(length = length(translations))
  
  #loops through the lines in the translations vector, each containing a synset-id, a lemma and a POS tag
  for(a in 1:length(translations))
  {
    #extracts the target language lemma and adds it to the previously created vector
	translated_lemmas[a] <- sub(pattern = "^\\(\'.+\',\\s\'(.+)\',\\s\'.+\'\\)$", 
                                replacement = "\\1", x = translations[a], perl = TRUE)
  }
  
  #loops through the target language lemmas
  for(b in 1:length(translated_lemmas))
  {
    #calculates topic similarity using a target language lemma as input
	calculate_topic_similarity(translated_lemmas[b], topic_id, num_keys, key_id)
  }
}

calculate_topic_similarity <- function(word, topic_id, num_keys, key_id)
{
  #converts all characters in the target language lemma to lowercase
  word <- gsub(pattern = "^(.+)$", replacement = "\\L\\1", x = word, perl = TRUE)
  
  #if the target language lemma consists of a word with a specification between parentheses following the word and an underscore, the word is kept while the part starting from the underscore is disposed of
  word <- gsub(pattern = "^(.+)_\\(.+$", replacement = "\\1", x = word, perl = TRUE)
  
  #loops through the target language topics
  for(a in 1:nrow(topic_keys_2))
  {
    #writes one line of target language topic keys to a vector
	topic_2 <- unname(unlist(topic_keys_2[a, 3:(num_keys+2)]))
    
	#loops through all of the topic keys belonging to the current target language topic
    for(b in 1:length(topic_2))
    {
      #checks whether the current target language topic key is a number (consists entirely of digits)
	  if(grepl(pattern = "^[0-9]+$", x = topic_2[b]))
      {
        #changes the content of the topic key variable (previously a number) to "#int" (so all numbers are treated as being the same entity)
		topic_2[b] <- "#int"
      }
      
	  #checks whether the current target language topic key is the same word as the current source language topic key and whether the current target language topic key has already been matched with any of the other topic keys belonging to the current source language topic (so that only the first match with a target language topic key counts toward the similarity score between the two topics)
      if((topic_2[b] == word) & (matched_matrix[a, b] == 0))
      {
        #if both conditions are met, the weighting factors belonging to the source language topic key and the target language topic key are multiplied and the resulting number is added to the number already present in the cell that corresponds to the combination of this particular source language topic and target language topic in the similarity matrix
		similarity_matrix[topic_id, a] <<- similarity_matrix[topic_id, a] + {weighting_top1[key_id] * weighting_top2[b]}
        
		#the zero in the cell corresponding to the current target language topic key in the match matrix is replaced with a one
		matched_matrix[a, b] <<- 1
		
		#any remaining topic keys for the current target language topic are skipped, so the translation of the source language topic key cannot match more than one target language topic key
        break
      }
    }
  }
}

export_seen_words <- function()
{
  #opens a new output file for the list of source language keys for which target language translation files have been saved (to be used in any additional run of the script)
  output <- file(description = "seen_words_4.txt", open = "w")
  
  #writes the list of source language keys for which target language translation files have been saved to the output file
  writeLines(text = word_vector, con = output)
  
  close(output)
}

export_topic_overviews <- function(source_language, target_language, num_topic_1, num_topic_2)
{
  #creates an empty vector for the row names of the new source language topic key table, of a length equal to the number of source language topics (400)
  headers_row_lang_1 <- vector(length = num_topic_1)
  
  #loops through the source language topics in the topic key matrix
  for(a in 1:nrow(overview_topics_1))
  {
    #converts the number corresponding to the current topic to a character string and saves it as a separate variable
	number <- as.character(a)
    
    if(a < 10)
    {
      #if the topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and added to the row names vector
	  headers_row_lang_1[a] <- paste("T", "00", number, sep = "")
    }
    else if((a >= 10) & (a < 100))
    {
      #if the topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and added to the row names vector
	  headers_row_lang_1[a] <- paste("T", "0", number, sep = "")
    }
    else if(a >= 100)
    {
      #if the topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and added to the row names vector
	  headers_row_lang_1[a] <- paste("T", number, sep = "")
    }
  }
  
  #uses the source language label to compile the name of the file to which a source language topic key table will be written
  name_output_1 <- paste("overview_topics-", source_language, "_4", ".txt", sep = "")
  
  #opens a new output file for the source language topic key table
  output_lang_1 <- file(description = name_output_1, open = "w")
  
  #writes the source language topic key table to the output file
  write.table(x = overview_topics_1, file = output_lang_1, quote = FALSE, sep = "\t", 
              row.names = headers_row_lang_1, col.names = FALSE)
  
  close(output_lang_1)
  
  #creates an empty vector for the row names of the new target language topic key table, of a length equal to the number of target language topics (400)
  headers_row_lang_2 <- vector(length = num_topic_2)
  
  #loops through the target language topics in the topic key matrix
  for(b in 1:nrow(overview_topics_2))
  {
    #converts the number corresponding to the current topic to a character string and saves it as a separate variable
	number <- as.character(b)
    
    if(b < 10)
    {
      #if the topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and added to the row names vector
	  headers_row_lang_2[b] <- paste("T", "00", number, sep = "")
    }
    else if((b >= 10) & (b < 100))
    {
      #if the topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and added to the row names vector
	  headers_row_lang_2[b] <- paste("T", "0", number, sep = "")
    }
    else if(b >= 100)
    {
      #if the topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and added to the row names vector
	  headers_row_lang_2[b] <- paste("T", number, sep = "")
    }
  }
  
  #uses the target language label to compile the name of the file to which a target language topic key table will be written
  name_output_2 <- paste("overview_topics-", target_language, "_4", ".txt", sep = "")
  
  #opens a new output file for the target language topic key table
  output_lang_2 <- file(description = name_output_2, open = "w")
  
  #writes the target language topic key table to the output file
  write.table(x = overview_topics_2, file = output_lang_2, quote = FALSE, sep = "\t", 
              row.names = headers_row_lang_2, col.names = FALSE)
  
  close(output_lang_2)
}

export_similarity_matrix <- function(source_language, target_language, num_topic_1, num_topic_2)
{
  #divides every value in the similarity matrix by 2870 (the highest possible similarity score when using 20 topic keys per topic) and then multiplies them by 1000 to express topic similarity with a value ranging from 0 to 1000
  similarity_matrix <<- {similarity_matrix / 2870} * 1000
  
  #formats the similarity scores so each of them is printed with three decimals
  similarity_matrix_print <- format(similarity_matrix, digits = 3)
  
  #creates an empty vector for the row names of the similarity matrix, of a length equal to the number of source language topics (400)
  headers_row <- vector(length = num_topic_1)
  
  #creates an empty vector for the column names of the similarity matrix, of a length equal to the number of target language topics (400)
  headers_col <- vector(length = num_topic_2)
  
  #loops through the source language topics
  for(a in 1:num_topic_1)
  {
    #converts the number corresponding to the current topic to a character string and saves it as a separate variable
	number <- as.character(a)
    
    if(a < 10)
    {
      #if the topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and added to the row names vector
	  headers_row[a] <- paste("T", "00", number, sep = "")
    }
    else if((a >= 10) & (a < 100))
    {
      #if the topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and added to the row names vector
	  headers_row[a] <- paste("T", "0", number, sep = "")
    }
    else if(a >= 100)
    {
      #if the topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and added to the row names vector
	  headers_row[a] <- paste("T", number, sep = "")
    }
  }
  
  #loops through the target language topics
  for(b in 1:num_topic_2)
  {
    #converts the number corresponding to the current topic to a character string and saves it as a separate variable
	number_2 <- as.character(b)
    
    if(b < 10)
    {
      #if the topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and added to the column names vector
	  headers_col[b] <- paste("T", "00", number_2, sep = "")
    }
    else if((b >= 10) & (b < 100))
    {
      #if the topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and added to the column names vector
	  headers_col[b] <- paste("T", "0", number_2, sep = "")
    }
    else if(a >= 100)
    {
      #if the topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and added to the column names vector
	  headers_col[b] <- paste("T", number_2, sep = "")
    }
  }
  
  #uses the source and target language labels to compile the name of the file to which the similarity matrix will be written
  name_output <- paste("topic-similarity-matrix_", source_language, "-", target_language, "_4", ".txt", sep = "")
  
  #opens a new output file for the similarity matrix
  output <- file(description = name_output, open = "w")
  
  #writes the similarity matrix to the output file
  write.table(x = similarity_matrix_print, file = output, quote = FALSE, sep = "\t", 
              row.names = headers_row, col.names = headers_col)
  
  close(output)
}

match_topics <- function()
{
  #divides every value in the similarity matrix by 1000 and then multiplies them by 100 to express topic similarity as a percentage
  similarity_matrix <<- {similarity_matrix / 1000} * 100
  
  #creates an empty vector for text lines containing a source language topic number and its matching target language topic numbers with corresponding similarity percentages
  topic_match_vector <- vector(length = 0)
  
  #loops through all the source language topics in the similarity matrix
  for(a in 1:nrow(similarity_matrix))
  {
    #sorts the percentages for the current source language topic in decreasing order and writes the values to a vector
	sorted_percentages <- sort(x = similarity_matrix[a, ], decreasing = TRUE)
    
	#creates an empty vector to keep track of the topic numbers belonging to target language topics that have been matched with a source language topic
	topic_matches <- vector(length = 0)
    
	#creates an empty vector for combinations of a formatted target language topic number and a similarity percentage
	formatted_topic_vector <- vector(length = 0)
    
	#loops through all of the similarity percentages for the current source language topic
    for(b in 1:length(sorted_percentages))
    {
      #checks whether the current percentage is above or equal to 10
	  if(sorted_percentages[b] >= 10)
      {
        #loops through all the target language topics in the unsorted similarity matrix
		for(d in 1:ncol(similarity_matrix))
        {
          #checks whether the current percentage is equal to the percentage in the cell corresponding to the combination of the current source language topic and target language topic, and whether the target language topic isn't already present in the vector that contains those target language topics that have already been matched
		  if((sorted_percentages[b] == similarity_matrix[a, d]) & !(d %in% topic_matches))
          {
            #if both conditions are met, the number belonging to the current target language topic is added to the appropriate vector
			topic_matches <- c(topic_matches, d)
            
			#converts the number corresponding to the current target language topic to a character string and saves it as a separate variable
			top_number <- as.character(d)
            
            if(d < 10)
            {
              #if the target language topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and saved as a new variable
			  topic <- paste("T", "00", top_number, sep = "")
            }
            else if((d >= 10) & (d < 100))
            {
              #if the target language topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and saved as a new variable
			  topic <- paste("T", "0", top_number, sep = "")
            }
            else if(d >= 100)
            {
              #if the target language topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and saved as a new variable
			  topic <- paste("T", top_number, sep = "")
            }
            
			#puts the formatted target language topic number and the similarity percentage set to have one decimal together like "T001 (100%)"
            formatted_topic <- paste(topic, " ", "(", format(similarity_matrix[a, d], digits=3), "%", ")", sep="")
            
			#adds the combined target language topic number and similarity percentage to the appropriate vector
			formatted_topic_vector <- c(formatted_topic_vector, formatted_topic)
            
			#any remaining target language topics are skipped, so that each similarity percentage leads to the addition of only one target language topic to the vector above
            break
          }
        }
      }
    }
    
	#converts the number corresponding to the current source language topic to a character string and saves it as a separate variable
    main_top_number <- as.character(a)
    
    if(a < 10)
    {
      #if the source language topic number is below 10 (consists of one digit), it is appended to a 'T' and two zeroes and saved as a new variable
	  main_topic <- paste("T", "00", main_top_number, sep = "")
    }
    else if((a >= 10) & (a < 100))
    {
      #if the source language topic number is above (or equal to) 10 but below 100 (consists of two digits), it is appended to a 'T' and one zero and saved as a new variable
	  main_topic <- paste("T", "0", main_top_number, sep = "")
    }
    else if(a >= 100)
    {
      #if the source language topic number is above (or equal to) 100 (consists of three digits), it is appended to a 'T' and saved as a new variable
	  main_topic <- paste("T", main_top_number, sep = "")
    }
    
	#pastes a tab stop and a vertical bar to the source language topic number, so it looks like "T001 |"
    formatted_textline <- sprintf("%s\t%s", main_topic, "|")
    
    if(length(formatted_topic_vector) == 0)
    {
      #if the vector that should contain formatted target language topic numbers and similarity percentages is empty (if there were no topic matches exceeding 10% similarity), a tab stop and four dashes are added to the string variable containing the formatted source language topic number instead
	  formatted_textline <- sprintf("%s\t%s", formatted_textline, "----")
    }
    else
    {
	  #if the vector that should contain formatted target language topic numbers and similarity percentages has content, this loops through all of them
	  for(e in 1:length(formatted_topic_vector))
      {
        #adds the current target language topic number and similarity percentage combination to the string variable containing the source language target number and preceding target language topic numbers and similarity percentages
		formatted_textline <- sprintf("%s\t%s", formatted_textline, formatted_topic_vector[e])
      }
    }
    
	#adds the text line containing the current source language topic number and its matching target language topic numbers with corresponding similarity percentages to the appropriate vector
    topic_match_vector <- c(topic_match_vector, formatted_textline)
  }
  
  return(topic_match_vector)
}

export_topic_match_vector <- function(topic_match_vector, source_language, target_language)
{
  #uses the source and target language labels to compile the name of the file to which the source language topics, their target language topic matches, and the corresponding similarity percentages will be written
  name_output <- paste("topic-match-vector_", source_language, "-", target_language, "_4", ".txt", sep = "")
  
  #opens a new output file for the source language topics, their target language topic matches, and the corresponding similarity percentages
  output <- file(description = name_output, open = "w")
  
  #writes the source language topics, their target language topic matches, and the corresponding similarity percentages to the output file
  writeLines(text = topic_match_vector, con = output)
  
  close(output)
}

args <- commandArgs(trailingOnly=TRUE)
match_bilingual_topics(args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8])