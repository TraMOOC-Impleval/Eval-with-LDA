#!usr/bin/env Rscript

extract_language <- function(filename_input, language, column_number)
{
  options(stringsAsFactors = FALSE)
  
  as.numeric(column_number) -> column_number
  paste("Coursera", language, sep=".") -> filename_output
  
  read.delim(file=filename_input, header=FALSE, quote="") ->> table1
  file(description=filename_output, open="w") -> output
  line_number <- 0
  text_lines <- vector(length=0)
  
  for(i in 1:nrow(table1))
  {
    if((grepl(pattern="\\w+", x=table1[i,column_number])) & !(grepl(pattern="^\"\\s*(\\[|\\()", x=table1[i,column_number])))
    {
      line_number <- line_number + 1
      
      gsub(pattern="^\"(.+)\"$", replacement="\\1", x=table1[i,column_number], perl=TRUE) -> text_lines[line_number]
    }
  }
  
  writeLines(text=text_lines, con=output)
  close(output)
}

args <- commandArgs(trailingOnly=TRUE)
extract_language(args[1], args[2], args[3])