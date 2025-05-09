library(tidyverse)
library(shiny)
library(knitr)


#Read github text file into a data frame. 
read_github_pr <- function(text_file){
  
  #Changing to format to the diff file. 
  #Replace Matches with New Text, Stringr, https://stringr.tidyverse.org/reference/str_replace.html
  file_name <- paste0(str_replace(text_file, 
                           pattern = "https://github.com/", 
                           replacement = "https://patch-diff.githubusercontent.com/raw/"), ".diff")
  
  #readLines: Read Text Lines from a Connection, Rdocumentation, https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readLines
  all_text_df <- data.frame(lines = readLines(file_name))
  
  return(all_text_df)
}

#Function for labeling additions in the github pull request. 
label_additions <- function(text_file_df){
  
  #Grepl Function
  #grep: Pattern Matching and Replacement, Rdocumentation, https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/grep
  df <- text_file_df %>% 
    mutate(lines = ifelse(grepl("^\\+\\s+", lines), lines %>% 
                            str_replace("^\\+\\s+", "**") %>% 
                            str_replace("$", "**"), lines))
  return(df)
}

#Function for labeling subtractions in the github pull request. 

label_subtraction <- function(text_file_df){
  
  df <- text_file_df %>% 
    mutate(lines = ifelse(grepl("^\\-\\s+", lines), lines %>% 
                          str_replace("^\\-\\s+", "~~") %>% 
                          str_replace("$", "~~"), lines)) %>% 
    mutate(lines = str_replace_all(lines, "\\s*~~\\s*", "~~"))
  
  return(df)
 
}

#Converting dataframe to lines

convert_to_lines <- function(text_file_df){
  
  lines <- text_file_df$lines
  
}

# Adding section styles that convert sections from a directory format to one 
# with parentheticals. 
# ex. t29/s203/a/1/i -> 203(a)(2)(3)

#Extract the Complete Match, Stringr, https://stringr.tidyverse.org/reference/str_extract.html
section_style <- function(section){
  section_num <- str_extract(section, "s\\d+") %>% str_replace("s", "")
  #print(section_num)
  split <- (str_split(trimws(section), "s\\d+") %>% unlist())[2]
  #print(split)
  section_string <- ""
  switch <- "("
  
  #print(str_split(split, pattern="")[[1]])
  
  for (i in str_split(split, pattern="")[[1]]){
    #print(i)
    if (i != "/" & i!=":"){
      section_string <- paste0(section_string, "(", i, ")")
    } else {
      next
    }
    
  }
  
  return(paste0(section_num, section_string))
}

df_section_changes <- function(list_changes){
  sections <- c()
  
  change_block <- ""
  changes <- c()
  
  section_styled <- c()
  
  for (s in all_text){
    if (grepl("/us/usc/", s)){
      if (grepl("\\~", change_block) & !grepl("\\*", change_block)){
        
        change_block <- change_block %>% 
          str_replace_all("\\~", "") %>% 
          str_replace("^", "~~") %>% 
          str_replace("$", "~~")
        
      }
      
      if (grepl("\\*", change_block) & !grepl("\\~", change_block)){
        
        change_block <- change_block %>% 
          str_replace_all("\\*", "") %>% 
          str_replace("^", "**") %>% 
          str_replace("$", "**")
      }
      
      
      changes <- c(changes, change_block)
      change_block <- ""
      
      sections <- c(sections, s)
      section_styled <- c(section_styled, section_style(s %>% str_replace_all("\\*", "")))
      next
      
    } else if (grepl("\\~", s) | grepl("\\*\\*", s)) {
      
      change_block <- paste0(change_block, s)
      next
      
    } else {
      next
    }
  }
  
  all_changes <- data.frame("section" = sections, 
                            "changes" = changes) %>% 
    mutate(changes = str_squish(str_replace(changes, "text:", "")),
           section_styled = section_styled,
           section_num = str_extract(section, "s\\d+") %>% str_replace("s", "") %>% str_trim(),
           title_section = str_extract(section, "t\\d+/s\\d+"),
           changes = str_replace_all(changes, "\\*\\**", "\\*\\*"))
  
  return(all_changes)
}

get_additions <- function(all_changes){
  additions <- all_changes %>% 
    filter(grepl("\\*\\*", section)) %>% 
    mutate(section_styled = str_replace_all(section_styled, "\\(\\)", ""))
  
  return(additions)
  
}



