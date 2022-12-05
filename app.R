library(shiny)
library(pdftools)
library(tidyverse)
library(writexl)
library(readxl)
library(shinyjs)
library(shinybusy)
library(textreadr)
ui <- fluidPage(style="padding:60px;",
tags$head(
    useShinyjs()
    ,
    add_busy_bar(color = "#8fb8ff")

),
h1("Tool 1: Convert multiple pdf, doc, docx, pptx, odt and text  files to an xlsx spreadsheet"),
p("Tool 1 is probably all you need."),
p("Preparing your files: In doc, docx and text files you can separate text into statements by inserting lines containing just '--' where you want to separate statements."),
p("Otherwise, doc and docx files will be broken into paragraphs and pdf files will be broken into pages."),
hr(),
p("Select more than one file (if required) by pressing shift, cmd or control when selecting. Or drag files onto the field below."),
p("You will then be able to download an Excel-format spreadsheet with all the text data in a single column called 'text' and a column for document name."),
    fileInput("upload_upload0",icon("upload") %>% span(" Upload"),
              multiple=T,
              width="400px",
              placeholder="Drag files here or press Browse",
              accept = c("pdf", "docx", "doc", "txt")),
    div(id="down0",h3("Now download your file"),downloadButton("download_download0","Download")) %>% hidden,
hr(),
h1("Tool 2: Convert multiple pdf files to an xlsx spreadsheet, preserving page numbers"),
p("Tool 1 should be enough. Only use this if you need to preserve page numbers."),
p("Select more than one file (if required) by pressing shift, cmd or control when selecting. Or drag files onto the field below."),
p("You will then be able to download an Excel-format spreadsheet with all the text data in a single column, with one row for each of the original text areas in the documents, together with columns for absolute (not necessarily printed) page number and a column for document name."),
    fileInput("upload_upload",icon("upload") %>% span(" Upload"),
              multiple=T,
              width="400px",
              placeholder="Drag files here or press Browse",
              accept = c("application/pdf")),
    div(id="down",h3("Now download your file"),downloadButton("download_download","Download")) %>% hidden,
h1("Tool 3: Convert xlsx export from https://www.adobe.com/uk/acrobat/online/pdf-to-excel.html to a Causal Map compatible format"),
p("You may need this tool if Tool 2 is mangling columns, tables etc."),
p("First, go to https://www.adobe.com/uk/acrobat/online/pdf-to-excel.html to convert any problematic PDFs individually to xlsx."),
p("Then, batch convert them here to a format which Causal Map can read."),
p("Select more than one file (if required) by pressing shift, cmd or control when selecting. Or drag files onto the field below."),
p("You will then be able to download an Excel-format spreadsheet with all the text data in a single column, with one row for each of the original text areas in the documents, with no column for page number but with a column for document name."),
    fileInput("upload_adobe",icon("upload") %>% span(" Upload"),
              multiple=T,
              width="400px",
              placeholder="Drag files here or press Browse"
              # ,
              # accept = c("application/xlsx","application/xls")
              ),
    div(id="down_adobe",h3("Now download your file"),downloadButton("download_adobe","Download")) %>% hidden,
div("Free service by ",a("Causal Map",href="https://causalmap.app"),style="position:absolute;bottom:60px;left:60px")

)

server <- function(input, output) {
options(shiny.maxRequestSize = 300*1024^2)
    control <- reactiveValues()

    multi_import <- function(inFile){
      statements <-
        tibble(text=read_document(inFile$datapath) ) %>%
        mutate(source_id=inFile$name)



        if(str_detect(statements$text,"^-- *$|\n-- *\n") %>% any){
      # browser()

      statements <-
          statements %>%
        mutate(text= str_split(text,"\n" %>% unlist)) %>%
        unnest(cols=c(text))

      statements <-
          statements %>%
          filter(text!="^ *$") %>%
          mutate(section=str_detect(text,"^-- *") %>% cumsum) %>%
          group_by(section,source_id) %>%
          summarise(text=paste0(text,collapse="\n\n") %>% str_remove_all("--\n\n")) %>%
          ungroup %>%
          select(-section) %>%
        mutate(text=str_replace_all(text,"\n\n","\n")) %>%
        filter(text!="^-- *$")

        }
statements
      # list(
      #   statements=statements
      # )

    }

    observeEvent(input$upload_upload0,{
# browser()
            inFile <- isolate(input$upload_upload0)
        if (is.null(inFile))
            return(NULL)
        statements <-
          seq_along(1:nrow(inFile)) %>%
              map(~multi_import(inFile[.,])) %>%
              bind_rows

        sources <-
          statements %>%
          select(source_id) %>%
          distinct()
        control$content0 <-

          list(statements=statements,sources=sources)

        shinyjs::show("down0")
    })

    output$download_download0 <-
        downloadHandler(
            filename = function() {
                paste("import-",Sys.Date(), ".xlsx", sep="")
            },
            content = function(file) {
              # browser()
                write_xlsx(
                    control$content0,  path = file)


            }
        )
    observeEvent(input$upload_upload,{

# browser()
            inFile <- isolate(input$upload_upload)
        if (is.null(inFile))
            return(NULL)
            control$content <- seq_along(1:nrow(inFile)) %>%
                map(~get_text(inFile,.)) %>%
                bind_rows %>%
                list(statements=.)

        shinyjs::show("down")
    })

    get_text <- function(inFile,x){
      # browser()
      pdf_text(inFile$datapath[x])%>%
        map(~str_replace_all(.," {2,}"," ")) %>%
        # map(~str_replace_all(.,"\r\n"," ")) %>%
        unlist %>%
        tibble(text=.,page=seq_along(.),source_id=inFile$name[[x]])#,author=pdf_info(inFile$datapath[[x]])$author,modified=pdf_info(inFile$datapath[[x]])$modified)
    }


    output$download_download <-
        downloadHandler(
            filename = function() {
                paste("pdfs-",Sys.Date(), ".xlsx", sep="")
            },
            content = function(file) {
                write_xlsx(
                    control$content,  path = file)


            }
        )
    observeEvent(input$upload_adobe,{

            inFile <- isolate(input$upload_adobe)
        if (is.null(inFile))
            return(NULL)
            control$content_adobe <- seq_along(1:nrow(inFile)) %>%
                map(~convert_adobe(inFile[.,])) %>%
              bind_rows %>%
              list(statements=.)

        shinyjs::show("down_adobe")
    })

    output$download_adobe <-
        downloadHandler(
            filename = function() {
                paste("causalmap-adobe-",Sys.Date(), ".xlsx", sep="")
            },
            content = function(file) {
                write_xlsx(
                    control$content_adobe,  path = file)


            }
        )
    # output$pdf <- renderRpdf(rpdf(path = 'http://www.pdf995.com/samples/pdf.pdf',scale=0.75), env = parent.frame(), quoted = FALSE)

}
convert_adobe <- function(inFile){
  # browser()
  titl <- inFile$name
  readxl::read_xlsx(sheet = 1,col_types = "text",n_max = 10000,inFile$datapath,col_names = F) %>%
    select(where(~!all(is.na(.)))) %>%
    mutate_all(as.character)%>%
    mutate_all(~replace_na(.,""))%>%
    select(!is.logical) %>%
    mutate(end="\n") %>%
    unite(text,everything(),sep=" ") %>%
    mutate(text=str_replace_all(text,"  "," ")) %>%
    filter(!str_detect(text,"^ *\n")) %>%
    mutate(nchar=nchar(text)) %>%
    mutate(cumnchar=cumsum(nchar)) %>%
    mutate(trigger=(cumnchar-lag(cumnchar))>200) %>%
    mutate(x=as.numeric(trigger) %>% replace_na(0)) %>%
    mutate(cumtrigger=cumsum(x)) %>%
    group_by(cumtrigger) %>%
    summarise(text=paste0(text,collapse="\n")) %>%
    mutate(source_id=titl) %>%
    select(text,source_id)




    # mutate(section=row_number()) %>%
    # pivot_longer(-section) %>%
    # filter(value!="") %>%
    # select(-name,text=value) %>%
    # mutate(source_id=titl) %>%
    # select(text,section,source_id)

}
# Run the application
shinyApp(ui = ui, server = server)
