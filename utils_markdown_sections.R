library(tidyverse)
library(shiny)
library(knitr)

#THe following is code for taking any 
#additions to the statute and creating 
#legislative text in markdown. 

#See generally Yihui Xie, Christophe Dervieux, & Emily Riederer, R Markdown Cookbook (April, 3, 2025),https://bookdown.org/yihui/rmarkdown-cookbook/

create_sections <- function(additions){
  
  temp <- additions
  s_tag <- ""
  s_tag_list <- c()
  s_text <- ""
  s_text_list <- c()
  
  #Finding all Unique Titles 
  #Unique: Extract Unique Elements, Rdocumentation, https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/unique
  ts <- (additions$title_section %>% unique()) %>% unlist()
  
  resp <- ""
  count <- 1
  title_ext <- ""
  section_ext <- ""
  
  for (i in ts){
    
    count <- count + 1
    
    #Extract the Complete Match, Stringr, https://stringr.tidyverse.org/reference/str_extract.html
    title_ext <- str_extract(string=str_extract(string=i, pattern="t\\d+"), pattern="\\d+") 
    section_ext <- str_extract(string=str_extract(string=i, pattern="s\\d+"), pattern="\\d+")
    
    resp <- paste0(resp, paste0("####### Sec. ", count), paste0("(a) Section ", section_ext, " of Title ", title_ext, " of the United States Code is amended to add the following sections: \n"))
    
    temp <- additions %>% filter(section_num == section_ext)
    #List of each new section 
    s_tag_list <- temp$section_styled
    #List of changes for each section.
    s_text_list <- temp$changes
    
    for (i in 1:length(s_tag_list)){
      #5.2 Indent Text, R Markdown Cookbook, https://bookdown.org/yihui/rmarkdown-cookbook/indent-text.html (discussing how to indent text)
      resp <- paste0(resp, paste0("######## ", s_tag_list[i]), "\n\n")
      resp <- paste0(resp, paste0("######### ", s_text_list[i]), "\n\n")
      resp <- paste0(resp, "\n\n")
      
    }
    
    
  }
  
  return(resp)
  
}