library(shiny)
library(shinyFeedback)
library(knitr)

#Code is drawn from the sources in the comments.

#For Shiny to Generate RMarkdown formats:
#See Hadley Wickham, Mastering Shiny 146-47 (2021) (describing code for RShiny applications for RMarkdown documents)

#Shiny - Download Markdown File, Stack OverFlow (Jan. 1, 2023), https://stackoverflow.com/questions/75127311/shiny-download-markdown-file
#Download File as Word Document in a Shiny App, Stack Overflow (Apr. 12, 2021), https://stackoverflow.com/questions/67063320/download-file-as-word-document-in-a-shiny-app
#R2Docx: Create a Word Document with R, STHDA, https://www.sthda.com/english/articles/index.php?url=/print/7-r2docx-create-a-word-document-with-r/

# Define UI 
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Everybody Can Legislate!"),
  #Hadley Wickam, 2.3.1 Text, Mastering Shiny, https://mastering-shiny.org/basic-ui.html?q=rendertext#text (discussing how to render text).
  textOutput("info_page"),
  
  selectInput(
    "select",
    "Jurisdiction",
    list('United States' = 'us_code'),
    multiple=FALSE),
    
    # Main panel 
    mainPanel(
      #Asking users to input the bill name
      textInput(inputId = "bill_name", label="Bill Name", width = "1500px", value="Raise the Minimum Wage"),
      #Asking users to input the bill number
      textInput(inputId = "bill_number", label="Bill Number", width = "1500px", value="1234"),
      #Asking users to input the bill purpose
      textInput(inputId = "bill_purpose", label="Bill Purpose", width = "1500px", value="To raise the minimum wage, reform the subminimal wage, and for other purposes."),
      
      #Adding shiny Feedback to the bill url input.
      # See Hadley Wickam, 8.1 Validation, Mastering Shiny, https://mastering-shiny.org/action-feedback.html#validate (describing code for validating responses).
      #Garrett Grolemund & Joe Cheng, Write Error Messages for Your UI With Validate, Shiny (June 28, 2017), https://shiny.posit.co/r/articles/improve/validation/
      shinyFeedback::useShinyFeedback(),
      textInput(inputId = "bill_url", label="Branch (url)", width = "1500px", value="https://github.com/mnfurey25/us-code/pull/2"),
      textOutput("url_check"),
      
      textInput(inputId = "cosponsors", label="Cosponsors", width = "1500px", value="Jane Smith, John Smith"),
      downloadButton("print", "Generate Bill!")
      
    )
  
)

server <- function(input, output, session) {
  
  #See Hadley Wickam, 8.1 Validation, Mastering Shiny, https://mastering-shiny.org/action-feedback.html#validate (describing code for validating responses).
  #Garrett Grolemund & Joe Cheng, Write Error Messages for Your UI With Validate, Shiny (June 28, 2017), https://shiny.posit.co/r/articles/improve/validation/
  url_check <- reactive({
    
    #Using the str_detect function to compare URL to the right repository.
    #Detect the Presence/Absence of a Match, Stringr, https://stringr.tidyverse.org/reference/str_detect.html
    check <- str_detect(input$bill_url, 
                        "https://github.com/mnfurey25/us-code/pull/\\d*$")
    shinyFeedback::feedbackWarning("bill_url", !check, "Sorry, this is an invalid url. Please use one in this format https://github.com/mnfurey25/us-code/pull/{Number}")
    
  })
  
  output$info_page <- renderText({
    "Hello! Welcome to Everybody Can Legislate - an app that reads your
    your proposed statute changes in a PR to the mnfurey25/us-code repository and 
    converts it to a bill draft template depending on the jurisdiction. 
    
    A few NOTES:
    
    This is a law school project, and therefore any part of this application, including input and output, is not intended to be legal advice. \n

    No part of this application is intended to provide legal or professional advice.\n
    
    This draft template is not an official template.\n\n

    All opinions are my own. This application is also a work-in-progress! \n
    
    Please also note that when calling the PR, there may be an error, because GitHub 
    is detecting too many requests. Please wait a few minutes between generating bills
    on the same pull request URL if you are experiencing errors.\n
    
    "
  })
  
  output$url_check <- renderText(url_check())
  
  output$print <- downloadHandler(
    filename="us_bill.docx",
    
    content = function(file){
      
      #One could also use a temp directory to write the RMarkdown file. 
      #Winson Chang, Generating Downloadable Reports, Shiny (July 1, 2016), https://shiny.posit.co/r/articles/build/generating-reports/ (describing code for rendering RMarkdown document and temp files)
      
      params <- list(
        bill_name = input$bill_name, 
        bill_number = input$bill_number,
        bill_purpose = input$bill_purpose,
        cosponsors = input$cosponsors,
        bill_url = input$bill_url)
      
      id <- showNotification(
        "Knitting Bill ...",
        duration=NULL,
        closeButton=FALSE
      )
      on.exit(removeNotification(id), add=TRUE)
      
      rmarkdown::render("us_bill_template.Rmd",
                        output_file=file,
                        params=params,
                        envir=new.env(parent=globalenv())
                        
      )
      
    }
  )
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)