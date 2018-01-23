#!usr/bin/env Rscript

evaluate_translation_quality <- function(filename_input_1, filename_output_1, filename_stoplist_1, filename_mallet_1, filename_inferencer_1, filename_output_doc_topics_1, filename_input_2, filename_output_2, filename_stoplist_2, filename_mallet_2, filename_inferencer_2, filename_output_doc_topics_2, filename_sim_list, filename_results, source_language, target_language)
{
	options(stringsAsFactors = FALSE)
	
	#invokes MALLET to infer the topics present in the two input files, of which one contains sentences in the source language (English) and the other contains their translations in the target language (Dutch) (using stopword lists to filter out stopwords much like when building a topic model)
	infer_topics(filename_input_1, filename_output_1, filename_stoplist_1, filename_mallet_1, filename_inferencer_1, filename_output_doc_topics_1)
	infer_topics(filename_input_2, filename_output_2, filename_stoplist_2, filename_mallet_2, filename_inferencer_2, filename_output_doc_topics_2)
	
	scores <- calculate_content_similarity(filename_output_doc_topics_1, filename_output_doc_topics_2, filename_sim_list)
	
	export_results(scores, filename_input_1, filename_input_2, filename_results, source_language, target_language)
}

infer_topics <- function(filename_input, filename_output, filename_stoplist, filename_mallet, filename_inferencer, filename_output_doc_topics)
{
	#builds the first MALLET command (to convert the text provided as input to MALLET's internal format)
	command_spec_1 <- paste("mallet import-file --keep-sequence TRUE --token-regex \"[\\p{L}\\p{N}_]+|[\\p{L}\\p{P}]*\\p{L}]+\" --preserve-case FALSE --stoplist-file", filename_stoplist, "--use-pipe-from", filename_mallet, "--input", filename_input, "--output", filename_output, sep = " ")
	
	#runs the first MALLET command
	system(command = command_spec_1)
	
	#builds the second MALLET command (to infer topics in the text provided as input)
	command_spec_2 <- paste("mallet infer-topics --inferencer", filename_inferencer, "--input", filename_output, "--output-doc-topics", filename_output_doc_topics, "--doc-topics-max 10", sep = " ")
	
	#runs the second MALLET command
	system(command = command_spec_2)
}

calculate_content_similarity <- function(filename_output_doc_topics_1, filename_output_doc_topics_2, filename_sim_list)
{
	#opens two MALLET output files as input, which list the sentences in the source and target language and the topics that are present in them, and the file containing general bilingual topic similarity percentages
	con_doc_topics_1 <- file(description = filename_output_doc_topics_1, open = "r")
	con_doc_topics_2 <- file(description = filename_output_doc_topics_2, open = "r")
	con_sim_list <- file(description = filename_sim_list, open = "r")
	
	#writes the contents of the three files to separate vectors
	doc_topics_1 <- readLines(con = con_doc_topics_1, n = -1)
	doc_topics_2 <- readLines(con = con_doc_topics_2, n = -1)
	sim_list <- readLines(con = con_sim_list, n = -1)
	
	close(con_doc_topics_1)
	close(con_doc_topics_2)
	close(con_sim_list)
	
	#creates an empty vector for topical content similarity scores (which express the degree to which a source language sentence and its target language translation contain the same topics), of a length equal to the number of source language sentences
	similarity_scores <- vector(length = length(doc_topics_1))
	
	#loops through all of the text lines in the files listing the topics present in the source language sentences and their target language translations (Note: starts from line 2 to skip the line containing column headers)
	for(a in 2:length(doc_topics_1))
	{
		#puts every element on a text line from the source language file that is separated by spaces in a vector
		raw_1 <- unlist(strsplit(x = doc_topics_1[a], split = " "))
		
		#extracts the topic id numbers and their respective proportions from the vector (Note: the MALLET command specified that only 10 topics should be written to the output file, and the last element of the vector was always a single whitespace character, which is what explains the chosen range)
		raw_1 <- raw_1[{length(raw_1)-20}:{length(raw_1)-1}]
		
		#puts every element on a text line from the target language file that is separated by spaces in a vector
		raw_2 <- unlist(strsplit(x = doc_topics_2[a], split = " "))
		
		#extracts the topic id numbers and their respective proportions from the vector (Note: the MALLET command specified that only 10 topics should be written to the output file, and the last element of the vector was always a single whitespace character)
		raw_2 <- raw_2[{length(raw_2)-20}:{length(raw_2)-1}]
		
		#create empty vectors for the relevant source language topic id numbers, target language topic id numbers, and source language topic proportions
		indices_1 <- vector(length = 0)
		indices_2 <- vector(length = 0)
		proportions_1 <- vector(length = 0)
		
		#introduces a variable to fill with the sum of source language topic proportions
		total <- 0
		
		#loops through the topic id numbers and their respective proportions for both the source language sentence and its target language translation
		for(b in 1:20)
		{
			#checks whether the current value corresponds to a position in the vectors containing a topic id number
			if({b %% 2} > 0)
			{
				if(as.numeric(raw_1[b+1]) > 0.05)
				{
					#if the topic proportion for the current source language topic exceeds 0.05, its topic id number is written to the appropriate vector, as is the corresponding topic proportion, in a way that puts corresponding topic id numbers and proportions in the same position in their respective vectors (Note: the topic id number is raised by one, because MALLET assigned the number 0 to the first topic in the topic model, while the topic similarity scores file assigned it the number 1)
					indices_1[{b/2}+0.5] <- as.numeric(raw_1[b]) + 1
					proportions_1[{b+1}/2] <- as.numeric(raw_1[b+1])
				}
				
				if(as.numeric(raw_2[b+1]) > 0.05)
				{
					#if the topic proportion for the current target language topic exceeds 0.05, its topic id number is written to the appropriate vector (Note: the topic id number is raised by one, because MALLET assigned the number 0 to the first topic in the topic model, while the topic similarity scores file assigned it the number 1)
					indices_2[{b/2}+0.5] <- as.numeric(raw_2[b]) + 1
				}
			}
		}
		
		#multiplies all values in the source language topic proportions vector by 100
		proportions_1 <- proportions_1 * 100
		
		#calculates the sum of all source language topic proportions for this sentence and saves it to an earlier introduced variable
		total <- sum(proportions_1)
		
		#multiplies the source language topic proportions by a value equal to 100 divided by the sum of source language topic proportions, so that the resulting values (the maximum scores per topic match) add up to 100, and writes these values to a new vector
		max_scores <- proportions_1 * {100 / total}
		
		#creates an empty vector for the actual scores, of a length equal to the length of the vector containing the maximum scores
		scores <- vector(length = length(max_scores))
		
		#fills the vector for the actual scores with zeroes (as a default score)
		scores[1:length(scores)] <- 0
		
		#loops through the source language topic id numbers for the current source language sentence
		for(d in 1:length(indices_1))
		{
			#saves the current topic id number as a separate variable
			topic_id <- indices_1[d]
			
			#finds the line belonging to the current topic id number in the topic similarity scores file and writes it to a vector
			matches <- unlist(strsplit(x = sim_list[topic_id], split = "\t", perl = TRUE))
			
			#extracts the target language topic matches from the vector (the first two positions in the vector always contain a topic id number, like "T001", and a vertical bar)
			matches <- matches[3:length(matches)]
			
			#extracts the highest topic similarity percentage from the vector and saves it as a separate variable
			max_perc <- as.numeric(sub(pattern = "^T[0-9]{3}\\s\\((.+)%\\)$", replacement = "\\1", x = matches[1]))
			
			#loops through the target language topic matches belonging to the current source language topic
			for(e in 1:length(matches))
			{
				#extracts the target language topic id number from a string, saves it as a separate variable, and converts it to an actual number
				topic_match <- sub(pattern = "^T([0-9]{3})\\s\\(.+%\\)$", replacement = "\\1", x = matches[e])
				topic_match <- as.numeric(topic_match)
				
				if(topic_match %in% indices_2)
				{
					#if the current target language topic id number is present in the vector containing the topic id numbers of the topics present in the target language translation of the current source language sentence, the percentage expressing the degree of similarity between the current source language topic and the current target language topic is saved to a separate variable
					perc <- as.numeric(sub(pattern = "^T[0-9]{3}\\s\\((.+)%\\)$", replacement = "\\1", x = matches[e]))
					
					#the bilingual topic similarity percentage is first transformed so that it's proportionate to the best matching target language topic being set to 100% similarity to the source language topic, and then it is divided by 100 and multiplied by the proportion of the source language topic for the current sentence, to create this topic match's contribution to the score expressing topical content similarity between the source language sentence and its target language translation, which is subsequently written to the vector containing separate topic match scores
					scores[d] <- max_scores[d] * {{perc * {100 / max_perc}} / 100}
					
					#any remaining target language topic matches for the current source language topic are skipped, so that only one match counts towards the topical content similarity score, namely the highest scoring one
					break
				}
			}
		}
		
		#combines the various topic match scores to calculate the overall topical content similarity score (ranging from 0 to 100) for the current source language sentence and its target language translation as an estimate of translation quality, and writes this score to the appropriate vector
		similarity_scores[a] <- sum(scores)
	}
	
	return(similarity_scores)
}

export_results <- function(scores, filename_lines_1, filename_lines_2, filename_results, source_language, target_language)
{
	#opens the two original input files, respectively containing the source language sentences and their target language translations
	con_lines_1 <- file(description = filename_lines_1, open = "r")
	con_lines_2 <- file(description = filename_lines_2, open = "r")
	
	#writes the text lines in the two files to separate vectors
	lines_1 <- readLines(con = con_lines_1, n = -1)
	lines_2 <- readLines(con = con_lines_2, n = -1)
	
	close(con_lines_1)
	close(con_lines_2)
	
	#creates an empty vector for the lines that are to be written to the eventual output file, of a length that's one over the number of source language sentences
	formatted_lines <- vector(length = length(lines_1) + 1)
	
	#writes the string "Evaluation of translation quality (0-100) [tab stop] English [tab stop] Dutch" to the first position in the vector
	formatted_lines[1] <- sprintf("%s\t%s\t%s", "Evaluation of translation quality (0-100)", source_language, target_language)
	
	#loops through all of the topical content similarity scores, of which there is one per sentence (Note: starts at 2, because the loop that filled the vector with scores started at 2 to skip an irrelevant line of column headers in the input, causing the first actual score to be in the second position of this vector, rather than the first)
	for(a in 2:length(scores))
	{
		#puts every element on a text line from the source language file that is separated by tab stops in a vector
		line_1 <- unlist(strsplit(x = lines_1[a-1], split = "\t", perl = TRUE))
		
		#extracts the source language sentence from the vector and saves it to a variable replacing that vector (each line consists of a line number, a language label, and the actual sentence, all separated by tab stops)
		line_1 <- line_1[3]
		
		#puts every element on a text line from the target language file that is separated by tab stops in a vector
		line_2 <- unlist(strsplit(x = lines_2[a-1], split = "\t", perl = TRUE))
		
		#extracts the target language sentence from the vector and saves it to a variable replacing that vector (each line consists of a line number, a language label, and the actual sentence, all separated by tab stops)
		line_2 <- line_2[3]
		
		#combines the topical content similarity score (two decimals allowed), the source language sentence, and its target language translation into one string, in which the elements are separated by tab stops, and writes it to the appropriate vector
		formatted_lines[a] <- sprintf("%3.2f\t%s\t%s", scores[a], line_1, line_2)
	}
	
	#opens a new output file for the topical content similarity scores, the source language sentences, and their target language translations
	con_results <- file(description = filename_results, open = "w")
	
	#writes the topical content similarity scores, the source language sentences, and their target language translations to the output file
	writeLines(text = formatted_lines, con = con_results)
}

args <- commandArgs(trailingOnly=TRUE)
evaluate_translation_quality(args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16])