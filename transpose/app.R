
library(shiny)
library(knitr) # To prepare Rmarkdown instructions
library(tidyverse) # For data manipulation
library(readtext) # Read in .doc and .docx files
library(udpipe) # Part-of-speech-tagger
library(tools) # To get file extension
library(DT) # To create a datatable
library(colourpicker) 
library(googleLanguageR)
library(tokenizers)
library(stringdist)
# library(shinyalert)
# library(shinyjs)
# library(V8)


langs <- read_csv("langs.csv")
lang_list <- langs$lang_long
colours <- read_csv("colours.csv")


VERB_colour <- colours[2,2]
COPULA_colour <- colours[3,2]
AUXILIARY_colour <- colours[4,2]
PARTICLE_colour <- colours[5,2]
ADVB_colour <- colours[6,2]
NOUN_colour <- colours[7,2]
DET_colour <- colours[8,2]
ADJ_colour <- colours[9,2]
PRON_colour <- colours[10,2]
PREP_colour <- colours[11,2]
SUB_colour <- colours[12,2]
COORD_colour <- colours[13,2]
PUNCT_colour <- colours[14,2]
INTERJECTION_colour <- colours[15,2]



shinyApp(
  
  ui <- fluidPage( # Open fluid page ----
    # Instructions page ----
    navbarPage("Translation App",
               
               tabPanel("Instructions",
                        uiOutput('Rmarkdown_instructions')
               ),
               
               # Let's get started navbar ----
               navbarMenu("Let's get started!",
                          #(1) Enter text tab panel ----
                          tabPanel("(1) Enter text",
                                   radioButtons("radio", label = h3("How do you wish to enter your data?"),
                                                choices = list("Upload file (.doc, .docx, or .txt)" = 1, "Enter text in textbox" = 2), 
                                                width = '100%', selected = 1),
                                   conditionalPanel(condition = "input.radio == 1",
                                                    fileInput("text_file", "Select file",
                                                              multiple = FALSE,
                                                              accept = c("text/plain",
                                                                         "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                                                                         "application/msword")
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.radio == 2",
                                                    textAreaInput("text_file_TA", "Enter text here...",
                                                                  placeholder = "Enter text here...",
                                                                  width = "100%", height = "100%", resize = "both")
                                                    # verbatimTextOutput("value")
                                   )
                          ), # End of tabPanel
                          
                          #(2) Check language tab panel ----
                          tabPanel("(2) Check language",
                                   
                                   htmlOutput("text_example"),
                                   
                                   uiOutput(label = "from... to...",
                                            "selectize")
                                   
                                   # conditionalPanel(condition = "length(input$selectize) == 0",
                                   #                  h2("Bingo")
                                   #                  # verbatimTextOutput("value")
                                   # )
                                   
                            ) # End of tabPanel
               ), # End of navBarMenu "Let's get started!"
               
               # Let's explore nav bar ----
               tabPanel("Let's explore!",

                                   tags$head(
                                     tags$style(HTML({"
                                       .mytooltip {
                                       position: relative;
                                       display: inline-block;
                                       }
                                       
                                       .mytooltip .tooltiptext {
                                       visibility: hidden;
                                       width: 120px;
                                       background-color: #4d0026;
                                       color: #fff;
                                       text-align: center;
                                       border: 6px solid #ff80ff;
                                       padding: 5px 0;

                                       
                                       /* Position the tooltip */
                                       position: absolute;
                                       z-index: 1;
                                       bottom: 100%;
                                       left: 50%;
                                       margin-left: -60px;
                                       }
                                       
                                       .mytooltip:hover .tooltiptext {
                                       visibility: visible;
                                       }

                                       "}))
                                   ),
                                   
                                   h3("Table will take a few seconds to appear/refresh..."),
                                   DT::dataTableOutput("table_coloured")
                          ), # End of tabPanel "Let's Explore!"
                          
               # Colours tab ----
               tabPanel("Colours", 
                        
                        selectInput(inputId = "colour_scheme",
                                    label = h3("Select colour scheme"), 
                                    choices = list("All colours" = 2,
                                                   "Verb-related words only" = 3,
                                                   "Noun-related words only" = 4,
                                                   "Linking words (conjunctions and Prepositions)" = 5), 
                                    selected = 2),
                        
                        h3("Widgets contain hexadecimal colour codes.
                                   Colours may be conveniently copied and pasted by copying and pasting these codes."),
                        
                        br(),
                        h3("Word classes in the Verb Complex (sometimes called Verb Phrase)"),
                        htmlOutput("colour_picker_verb"),
                        htmlOutput("colour_picker_copula"),
                        htmlOutput("colour_picker_auxiliary"),
                        htmlOutput("colour_picker_particle"),
                        htmlOutput("colour_picker_advb"),
                        br(),
                        h3("Word classes in the Noun Phrase"),
                        htmlOutput("colour_picker_noun"),
                        htmlOutput("colour_picker_det"),
                        htmlOutput("colour_picker_adj"),
                        htmlOutput("colour_picker_pron"),
                        br(),
                        h3("Linking words"),
                        htmlOutput("colour_picker_prep"),
                        htmlOutput("colour_picker_sub"),
                        htmlOutput("colour_picker_coord"),
                        br(),
                        h3("Other"),
                        htmlOutput("colour_picker_punct"),
                        htmlOutput("colour_picker_interjection")
                        
                        
                    ), # End of tabPanel "Colors"
               
                  # Punctuation tab ====
                  tabPanel("Punctuation",
                           h4("Punctuation characters can cause a lot of problems for the app, because Google Translate
                              treats different characters differently in different languages. For example, when translating
                              from English to Spanish it will replace a comma with a semi-colon. This then effects the way
                              the app segments sentences (e.g. how it decides when the sentence begins and ends). This will
                              result either in an error, or a weird output that is difficult to interpret."),
                           h4("To prevent this,
                              the app replaces problematic punctuation characters. Typically, one needs to replace \"exotic\"
                              characters with \"boring\" ones, e.g. semi-colons with commas. This page allows you to see how
                              characters are replaced, and allows you to specify your own rules if you wish. Just type
                              the original characters in the left hand box, and the replacing characters in the right hand
                              box. You can also edit boxes which already contain characters."),
                           
                           # From https://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
                           
                           # fluidRow(
                           #   box(width = 12, title = "A Box in a Fluid Row I want to Split", 
                                 splitLayout(
                                   textInput("replaced1", value = ":", label = "Original character"),
                                   textInput("replacer1", value = ",", label = "Replacement character")
                                 ),
                                 splitLayout(
                                   textInput("replaced2", value = ";", label=NULL),
                                   textInput("replacer2", value = ",", label=NULL)
                                 ),
                                 splitLayout(
                                   textInput("replaced3", value = ":", label=NULL),
                                   textInput("replacer3", value = ",", label=NULL)
                                 ),
                                 splitLayout(
                                   textInput("replaced4", value = "(", label=NULL),
                                   textInput("replacer4", value = ",", label=NULL)
                                 ),
                                 splitLayout(
                                   textInput("replaced5", value = ")", label=NULL),
                                   textInput("replacer5", value = ",", label=NULL)
                                 ),
                           
                                 splitLayout(
                                   textInput("replaced6", value = "", label=NULL),
                                   textInput("replacer6", value = "", label=NULL)
                                 ),
                           
                               splitLayout(
                                 textInput("replaced7", value = "", label=NULL),
                                 textInput("replacer7", value = "", label=NULL)
                               ),
                           
                               splitLayout(
                                 textInput("replaced8", value = "", label=NULL),
                                 textInput("replacer8", value = "", label=NULL)
                               ),
                           
                           splitLayout(
                             textInput("replaced9", value = "", label=NULL),
                             textInput("replacer9", value = "", label=NULL)
                           ),
                           
                           splitLayout(
                             textInput("replaced10", value = "", label=NULL),
                             textInput("replacer10", value = "", label=NULL)
                           ),
                           
                           #   )
                           # )
                           
                           # https://github.com/jienagu/DT-Editor
                           # 
                           # titlePanel("Replace punctuation characters"),
                           # h3("Translation may go wrong if punctuation characters are not adequately dealt with."),
                           # h3("In general, unusual punctuation characters such as dashes need to be replaced with commas"),
                           # h3("The table below shows you which characters in the `source` are replaced with"),
                           # shinyjs::useShinyjs(),
                           # shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                           # actionButton("refresh", "Reset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           # 
                           # helpText("Note: Remember to save any updates!"),
                           # br(),
                           # ### tags$head() is to customize the download button
                           # tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                           # # downloadButton("Trich_csv", "Download in CSV", class="butt"),
                           # useShinyalert(), # Set up shinyalert
                           # uiOutput("MainBody_punct_table"),
                           # actionButton(inputId = "Updated_punct_table",label = "Save")
                           # 
                           ) # End of tabPanel "Punctuation"
               
               
                  ) # End of navBarPage
    
  ), # End of fluidPage

                          
                          
                          

  # server statement----
  server <- function(input, output, session){
  
    text <- reactive({
      
      if(is.null(input$text_file) & input$text_file_TA=="") return(NULL)
      
      if(is.null(input$text_file)==FALSE){
        text <- readtext(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      # 
      
      return(text)
      
    })
    
    
    
    
    # lang_iso (obtaining language) ----
    
    lang_iso <- reactive({
      
      if(is.null(input$text_file) & input$text_file_TA=="") return(NULL)
      
      if(is.null(input$text_file)==FALSE){
      
        
        text <- readtext(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      
      gl_auth("translation-app-256015-5b586d7ca141.json")
        
      # gl_auth("/Users/nickriches/Google Drive/AHRC artificial intelligence/translation_shiny_web_app_prototype/translation app/translation-app-256015-5b586d7ca141.json")
      
      lang_iso <- googleLanguageR::gl_translate_detect(text)$language
      
      return(lang_iso)
      
    })
    
    # lang_eng ----
    
    lang_eng <- reactive({
      
      lang_eng <- langs$lang_long[which(langs$iso_code == lang_iso())]
      
      return(lang_eng)
      
    })
    
    # table (showing transcript)----
    
    table <- reactive({
      
      # Main functions for generating colours and labels
      
      # highlights text in a particular colours
      
      highlight <- function(text, colour){
        result <- paste0("<span style=\"background-color:", colour, ";\">",
                         "&thinsp;", text, "&thinsp;",
                         "</span>")
        return(result)
      }
      
      add_tool_tip <- function(text, label){
        result <- paste0("<div class=\"mytooltip\">",
                         text,
                         "<span class=\"tooltiptext\">",
                         label,
                         "</span>",
                         "</div>")
        return(result)
      }
      
      # browser(); one <- 1; one <- 1; one <-1 ; one <- 1
      
      if(is.null(input$VERB_colour)) {VERB_colour <- "#FFAB94"} else {VERB_colour <- input$VERB_colour}
      if(is.null(input$COPULA_colour)) {COPULA_colour <- "#FFAB94"} else {COPULA_colour <- input$COPULA_colour}
      if(is.null(input$AUXILIARY_colour)) {AUXILIARY_colour <- "#FAD4CB"} else {AUXILIARY_colour <- input$AUXILIARY_colour}
      if(is.null(input$PARTICLE_colour)) {PARTICLE_colour <- "#FAD4CB"} else {PARTICLE_colour <- input$PARTICLE_colour}
      if(is.null(input$ADVB_colour)) {ADVB_colour <- "#FAD4CB"} else {ADVB_colour <- input$ADVB_colour}
      if(is.null(input$NOUN_colour)) {NOUN_colour <- "#B6B6F5"} else {NOUN_colour <- input$NOUN_colour}
      if(is.null(input$DET_colour)) {DET_colour <- "#ADFFFF"} else {DET_colour <- input$DET_colour}
      if(is.null(input$ADJ_colour)) {ADJ_colour <- "#ADFFFF"} else {ADJ_colour <- input$ADJ_colour}
      if(is.null(input$PRON_colour)) {PRON_colour <- "#99FF69"} else {PRON_colour <- input$PRON_colour}
      if(is.null(input$PREP_colour)) {PREP_colour <- "#FFFF52"} else {PREP_colour <- input$PREP_colour}
      if(is.null(input$SUB_colour)) {SUB_colour <- "#FCAD46"} else {SUB_colour <- input$SUB_colour}
      if(is.null(input$COORD_colour)) {COORD_colour <- "#FFCD7D"} else {COORD_colour <- input$COORD_colour}
      if(is.null(input$PUNCT_colour)) {PUNCT_colour <- "#eeeedd"} else {PUNCT_colour <- input$PUNCT_colour}
      if(is.null(input$INTERJECTION_colour)) {INTERJECTION_colour <- "#C29A72"} else {INTERJECTION_colour <- input$INTERJECTION_colour}

      
      highlight_wc <- function(string, wc){
        if(is.na(wc)){return(string)}
        # red (original colours - user may change)
        else if(wc == "VERB"){result <- add_tool_tip(highlight(paste0("<b>",string,"</b>"), VERB_colour), "VERB")}
        else if(wc == "COPULA"){result <- add_tool_tip(highlight(paste0("<b>", string, "</b>"), COPULA_colour), "COPULA")}
        # orange
        else if(wc == "SCONJ"){result <- add_tool_tip(highlight(string, SUB_colour), "SCONJ.")}
        # light orange
        else if(wc == "CCONJ"){result <- add_tool_tip(highlight(string, COORD_colour), "CCONJ.")}
        # green
        else if(wc == "PRON"){result <- add_tool_tip(highlight(string, PRON_colour), "PRON.")}
        # pink
        else if(wc == "AUX"){result <- add_tool_tip(highlight(string, AUXILIARY_colour), "AUX.")}
        else if(wc == "ADV"){result <- add_tool_tip(highlight(string, ADVB_colour), "ADV.")}
        else if(wc == "PART"){result <- add_tool_tip(highlight(string, PARTICLE_colour), "PARTICLE")}
        # dark blue
        else if(wc == "NOUN"){result <- add_tool_tip(highlight(string, NOUN_colour), "NOUN")}
        else if(wc == "PROPN"){result <- add_tool_tip(highlight(string, NOUN_colour), "PROPN")}
        # cyan
        else if(wc == "DET"){result <- add_tool_tip(highlight(string, DET_colour), "DET.")}
        else if(wc == "DET.poss"){result <- add_tool_tip(highlight(string, DET_colour), "DET.poss")}
        else if(wc == "ADJ"){result <- add_tool_tip(highlight(string, ADJ_colour), "ADJ.")}
        else if(wc == "NUM"){result <- add_tool_tip(highlight(string, DET_colour), "NUM.")}
        # brown
        else if(wc == "INTJ"){result <- add_tool_tip(highlight(string, INTERJECTION_colour), "INTJ")}
        # yellow
        else if(wc == "ADP"){result <- add_tool_tip(highlight(string, PREP_colour), "PREP.")}
        # grey
        else if(wc == "PUNCT"){result <- add_tool_tip(highlight(string, PUNCT_colour), "PUNCT.")}
        else if(wc == "X"){result <- add_tool_tip(highlight(string, "#b8b894"), "X")}
        else if(wc == "SYM"){result <- add_tool_tip(highlight(string, "#b8b894"), "SYM")}
        else{result <- string}
        return(result)
      }
      
      from_text <- text()
      
      adbs <- function(x){ # Add Double Backslash where necessary
        return(case_when(
          x == ")" ~ "\\)",
          x == "(" ~ "\\(",
          x == "]" ~ "\\]",
          x == "[" ~ "\\[",
          x == "}" ~ "\\}",
          x == "{" ~ "\\{",
          x == "" ~ " ",
          TRUE ~ x
        ))
      }
      
      from_text <- str_replace_all(from_text, adbs(input$replaced1), adbs(input$replacer1))
      from_text <- str_replace_all(from_text, adbs(input$replaced2), adbs(input$replacer2))
      from_text <- str_replace_all(from_text, adbs(input$replaced3), adbs(input$replacer3))
      from_text <- str_replace_all(from_text, adbs(input$replaced4), adbs(input$replacer4))
      from_text <- str_replace_all(from_text, adbs(input$replaced5), adbs(input$replacer5))
      from_text <- str_replace_all(from_text, adbs(input$replaced6), adbs(input$replacer6))
      from_text <- str_replace_all(from_text, adbs(input$replaced7), adbs(input$replacer7))
      from_text <- str_replace_all(from_text, adbs(input$replaced8), adbs(input$replacer8))
      from_text <- str_replace_all(from_text, adbs(input$replaced9), adbs(input$replacer9))
      from_text <- str_replace_all(from_text, adbs(input$replaced10), adbs(input$replacer10))
      
      # To be inserted back into final version
      
      # browser(); one <- 1; one <- 1; one <- 1; one <- 1; one <- 1
      
      num_targets <- length(input$selectize) - 1
      
      
      from_iso <- langs$iso_code[which(langs$lang_long == input$selectize[1])]

      from_lang_long <- langs$lang_long[which(langs$lang_long == input$selectize[1])]

      # To be removed from final version
      
      # from_text <- "Qué quieres hacer esta noche? Yo quiero ir al cine. Quieres venir conmigo?"
      # to_text <- "What do you want to do tonight? I want to go to the movies. Do you want to come with me?"
      # from_lang_long <- "Spanish; Castilian"
      # to_lang_long <- "English"
      
      #xxxxxxxxxxxxxxxxxxxxxxxxx
     
      # Load models
      
      from_udpipe_model_name <- langs$udpipe_name[which(langs$lang_long == from_lang_long)]
      
      # Routine for if model is found.
      
      if(is.na(from_udpipe_model_name) == FALSE){
      
      from_model <- udpipe_download_model(from_udpipe_model_name, model_dir = tempdir())
      from_model <- udpipe_load_model(from_model$file_model)
      
      from_parsed <- as.data.frame(udpipe_annotate(from_model, from_text))
      
      
      # browser(); one <- 1; one <- 1; one <- 1; one <- 1
      
      from_parsed$coloured <- mapply(highlight_wc, from_parsed$token, from_parsed$upos)
      
      from_parsed$hasclass <- paste0("has", tolower(from_parsed$upos))
      
      from_parsed %>%
        group_by(sentence_id) %>%
        summarise(sentence_coloured = paste(coloured, collapse = " "),
                  sentence_not_coloured = paste(token, collapse = " "),
                  hasclass = paste(hasclass, collapse = " ")) ->
        from_table
      
      }
      
      # Routine for if model is not found (basically creates dataframe with no colouring)
      
      if(is.na(from_udpipe_model_name) == TRUE){
        from_table <- as.data.frame(unlist(tokenize_sentences(from_text)))
        names(from_table)[1] <- "sentence_coloured"
        from_table$sentence_not_coloured <- from_table$sentence_coloured
        from_table$hasclass = ""
        from_table$sentence_id <- as.numeric(row.names(from_table))
        from_table <- subset(from_table, select=c(4,1,2,3))
      }
      
      
      all_table <- from_table
      
      
      for(loop in 1:num_targets){
      
      to_iso <- langs$iso_code[which(langs$lang_long == input$selectize[loop + 1])]
      
      to_lang_long <- langs$lang_long[which(langs$lang_long == input$selectize[loop + 1])]
      
      to_text <- gl_translate(text(), target = to_iso, source = from_iso)$translatedText
      
      to_text <- str_replace_all(to_text, adbs(input$replaced1), adbs(input$replacer1))
      to_text <- str_replace_all(to_text, adbs(input$replaced2), adbs(input$replacer2))
      to_text <- str_replace_all(to_text, adbs(input$replaced3), adbs(input$replacer3))
      to_text <- str_replace_all(to_text, adbs(input$replaced4), adbs(input$replacer4))
      to_text <- str_replace_all(to_text, adbs(input$replaced5), adbs(input$replacer5))
      to_text <- str_replace_all(to_text, adbs(input$replaced6), adbs(input$replacer6))
      to_text <- str_replace_all(to_text, adbs(input$replaced7), adbs(input$replacer7))
      to_text <- str_replace_all(to_text, adbs(input$replaced8), adbs(input$replacer8))
      to_text <- str_replace_all(to_text, adbs(input$replaced9), adbs(input$replacer9))
      to_text <- str_replace_all(to_text, adbs(input$replaced10), adbs(input$replacer10))
      
      
      to_udpipe_model_name <- langs$udpipe_name[which(langs$lang_long == to_lang_long)]
      
      # Routine for if model is found.
      
      if(is.na(to_udpipe_model_name) == FALSE){
      
      to_model <- udpipe_download_model(to_udpipe_model_name, model_dir = tempdir())
      to_model <- udpipe_load_model(to_model$file_model)
      
      to_parsed <- as.data.frame(udpipe_annotate(to_model, to_text))
      
      to_parsed$coloured <- mapply(highlight_wc, to_parsed$token, to_parsed$upos)
      
      to_parsed$hasclass <- paste0("has", tolower(to_parsed$upos))
      
      to_parsed %>%
        group_by(sentence_id) %>%
        summarise(sentence_coloured = paste(coloured, collapse = " "),
                  sentence_not_coloured = paste(token, collapse = " "),
                  hasclass = paste(hasclass, collapse = " ")) ->
        to_table
      
         }
      
      # Routine for if model is not found (basically creates dataframe with no colouring)
      
      if(is.na(to_udpipe_model_name) == TRUE){
        to_table <- as.data.frame(unlist(tokenize_sentences(to_text)))
        names(to_table)[1] <- "sentence_coloured"
        to_table$sentence_not_coloured <- to_table$sentence_coloured
        to_table$hasclass = ""
        to_table$sentence_id <- as.numeric(row.names(to_table))
        to_table <- subset(to_table, select=c(4,1,2,3))
      }
      
      all_table <- rbind(all_table, to_table)
      
      }
      


      
      all_table %>% arrange(sentence_id) -> all_table
      
      # Create a variable that swaps from and to for each sentence_id to aid Universal Filter
      
      all_table$swapped <- ""
      
      ref <- 1
      
      for(i in 1:nrow(all_table)){
        
        if(i %% (num_targets + 1) == 1){ref <- i}
        
        if(i == ref){
          start <- i + 1
          stop <- i + num_targets
          all_table$swapped[i] <- paste(all_table$sentence_not_coloured[start:stop], collapse = " ")
        }
        
        if(i > ref){
          all_table$swapped[i] <- all_table$sentence_not_coloured[ref]
        }
        
      }
      
      # Add symbol in from of all "from" lines
      
      all_table$is_source <- rep(x = c(1, rep(0, num_targets)), times = nrow(all_table) / (num_targets + 1))
      
      add_plus <- function(sentence, is_source){
        if(is_source == 1) return(paste("<font color=\"red\">===</font>", sentence))
        if(is_source == 0) return(paste("<font color=\"white\">===</font>", sentence))
      }

      all_table$sentence_coloured <- mapply(add_plus, all_table$sentence_coloured, all_table$is_source)

      return(all_table)
      
    })
    
    # colours ----
    
    verb_col <- reactive({
      colour <- colours[1, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    copula_col <- reactive({
      colour <- colours[2, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    auxiliary_col <- reactive({
      colour <- colours[3, as.numeric(input$colour_scheme)]
      return(colour)
    })

    particle_col <- reactive({
      colour <- colours[4, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    advb_col <- reactive({
      colour <- colours[5, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    noun_col <- reactive({
      colour <- colours[6, as.numeric(input$colour_scheme)]
      return(colour)
    })

    det_col <- reactive({
      colour <- colours[7, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    adj_col <- reactive({
      colour <- colours[8, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    pron_col <- reactive({
      colour <- colours[9, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    prep_col <- reactive({
      colour <- colours[10, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    sub_col <- reactive({
      colour <- colours[11, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    coord_col <- reactive({
      colour <- colours[12, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    punct_col <- reactive({
      colour <- colours[13, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    interjection_col <- reactive({
      colour <- colours[14, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    output$colour_picker_verb <- renderUI({
      colourpicker::colourInput(
        inputId = "VERB_colour",
        label = "Main Verb (label = VERB)",
        value = verb_col()
      )
    })
    
    output$colour_picker_copula <- renderUI({
      colourpicker::colourInput(
        inputId = "COPULA_colour",
        label = "Copula (label = COPULA)",
        value = copula_col()
      )
    })
    
    output$colour_picker_auxiliary <- renderUI({
      colourpicker::colourInput(
        inputId = "AUXILIARY_colour",
        label = "Auxiliary verb (label = AUXILIARY)",
        value = auxiliary_col()
      )
    })
    
    output$colour_picker_particle <- renderUI({
      colourpicker::colourInput(
        inputId = "PARTICLE_colour",
        label = "Verb particle (label = PARTICLE)",
        value = particle_col()
      )
    })
    
    output$colour_picker_advb <- renderUI({
      colourpicker::colourInput(
        inputId = "ADVB_colour",
        label = "Adverb (label = ADVB)",
        value = advb_col()
      )
    })
    
    output$colour_picker_noun <- renderUI({
      colourpicker::colourInput(
        inputId = "NOUN_colour",
        label = "Noun (label = NOUN)",
        value = noun_col()
      )
    })
    
    
    output$colour_picker_det <- renderUI({
      colourpicker::colourInput(
        inputId = "DET_colour",
        label = "Determiner (label = DET)",
        value = det_col()
      )
    })
    
    
    output$colour_picker_adj <- renderUI({
      colourpicker::colourInput(
        inputId = "ADJ_colour",
        label = "Adjective (label = ADJ)",
        value = adj_col()
      )
    })
    
    
    output$colour_picker_pron <- renderUI({
      colourpicker::colourInput(
        inputId = "PRON_colour",
        label = "Pronoun (label = PRON)",
        value = pron_col()
      )
    })
    
    output$colour_picker_prep <- renderUI({
      colourpicker::colourInput(
        inputId = "PREP_colour",
        label = "Preposition (label = PREP)",
        value = prep_col()
      )
    })
    
    output$colour_picker_sub <- renderUI({
      colourpicker::colourInput(
        inputId = "SUB_colour",
        label = "Subordinator (label = SUB)",
        value = sub_col()
      )
    })
    
    output$colour_picker_coord <- renderUI({
      colourpicker::colourInput(
        inputId = "COORD_colour",
        label = "Coordinator (label = COORD)",
        value = coord_col()
      )
    })
    
    output$colour_picker_punct <- renderUI({
      colourpicker::colourInput(
        inputId = "PUNCT_colour",
        label = "Punctuation (label = PUNCT)",
        value = punct_col()
      )
    })
    
    output$colour_picker_interjection <- renderUI({
      colourpicker::colourInput(
        inputId = "INTERJECTION_colour",
        label = "Interjection (label = INTERJECTION)",
        value = interjection_col()
      )
    })
    
    
    
    
    
    
    # ***RENDERING STATEMENTS*** ----
    # Rmarkdown_instructions ----
    
    #colour_set statement ====
    # output$VERB_colour = renderUI({ # NB it looks as if there needs to be a new renderstatement for each dropdown
    #   
    #   colourpicker::colourInput(
    #     inputId = "VERB_colour",
    #     label = "Main Verb (label = VERB)",
    #     # value = colours[1, input$scheme]
    #     value = "#FFAB94"
    #     # showColour = "background"
    #   )
    #   
    #   # colourpicker::colourInput(
    #   #   inputId = "COPULA_colour",
    #   #   label = "Copula (label = COP.)",
    #   #   value = "#FFAB94"
    #   #   # showColour = "background"
    #   # )
    #   
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "AUXILIARY_colour",
    #   #   label = "Auxiliary Verb (label = AUX.)",
    #   #   value = "#FAD4CB"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "PARTICLE_colour",
    #   #   label = "Particle e.g. \"to\" in \"to go\" (label = PART.)",
    #   #   value = "#FAD4CB"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "ADV_colour",
    #   #   label = "Adverb (label = ADV.)",
    #   #   value = "#FAD4CB"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # hr(),
    #   # 
    #   # h3("Noun Phrase"), 
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "NOUN_colour",
    #   #   label = "Noun (label = NOUN)",
    #   #   value = "#B6B6F5"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "DET_colour",
    #   #   label = "Determiner (label = DET., or DET.poss if possessive)",
    #   #   value = "#ADFFFF"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "ADJ_colour",
    #   #   label = "Adjective (label = ADJ.)",
    #   #   value = "#ADFFFF"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "PRON_colour",
    #   #   label = "Pronoun (label = PRON.)",
    #   #   value = "#99FF69"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # hr(),
    #   # 
    #   # h3("Prepositions"),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "PREP_colour",
    #   #   label = "Prepositions (label = PREP.)",
    #   #   value = "#FFFF52"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # hr(),
    #   # 
    #   # h3("Linking Words"),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "SUB_colour",
    #   #   label = "Subordinating Conjunction (label = SCONJ.)",
    #   #   value = "#FCAD46"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "COORD_colour",
    #   #   label = "Coordinating Conjunction (label = CCONJ.)",
    #   #   value = "#FFCD7D"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # hr(),
    #   # 
    #   # h3("Others"),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "PUNCT_colour",
    #   #   label = "Punctuation Character (label = PUNCT.)",
    #   #   value = "#eeeedd"
    #   #   # showColour = "background"
    #   # ),
    #   # 
    #   # colourpicker::colourInput(
    #   #   inputId = "INTERJECTION_colour",
    #   #   label = "Interjection (label = INTJ.)",
    #   #   value = "#C29A72"
    #   #   # showColour = "background"
    #   #   )
    #   
    #   # ) # end of HTML statement ====
    #   
    # })
    # 
    
    output$Rmarkdown_instructions <- renderUI({
      # HTML(rmarkdown::render('Rmarkdown_instructions.Rmd'))
      HTML(markdown::markdownToHTML(knit('Rmarkdown_instructions_reduced.Rmd', quiet = TRUE)))
      # includeHTML("Rmarkdown_instructions.html")
    })
    
    # (2) Check language tab panel ----
    output$text_example <- renderUI({
      text <- substr(text(), 1, 1000)
      HTML(paste0("<p><h1>Text</h1><h3>(up to 1000th character)</h3>", text,"</p>"))
    })
    
    # Language Selectize ----
    output$selectize = renderUI({
      selectizeInput(inputId = "selectize", # NB refer to input$selectize
                     label = "from... to...",
                     choice = lang_list,
                     selected = lang_eng(),
                     multiple = TRUE)
    })
    
    
    # table_coloured ----
    output$table_coloured = DT::renderDataTable({
      datatable(table(),
                filter = c("top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = TRUE,
                               search = list(regex = TRUE, scrollX = TRUE)
                              )
      ) %>% formatStyle(columns = c(1), width='100px') %>% 
        formatStyle("sentence_coloured","white-space"="nowrap") %>% 
        formatStyle("sentence_not_coloured","white-space"="nowrap", color = "lightgray") %>%
        formatStyle("hasclass","white-space"="nowrap", color = "lightgray") %>%
        formatStyle("swapped","white-space"="nowrap", color = "lightgray") %>% 
        formatStyle("is_source","white-space"="nowrap", color = "lightgray")
    })
    
    
    
  } # end of server statement
  
)




