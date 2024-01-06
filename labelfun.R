#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(base64enc)
library(magick)
library(tesseract)
library(data.table)
library(DT)

#tesseract_download("chi_tra")
# Adapted from https://github.com/kyamagu/bbox-annotator/
# Edited original JS to add color_list as an option
# ...should be the same length as labels
# ...and controls the color of the rectangle
# ...will probably be broken for input_method = "fixed" or "text"
# Also added color as a value in each rectangle entry
js <- '
    $(document).ready(function() {
       // define options to pass to bounding box constructor
        var options = {
          url: "https://www.r-project.org/logo/Rlogo.svg",
          input_method: "select", 
          labels: [""],
          color_list:  [""], 
          onchange: function(entries) {
                Shiny.onInputChange("rectCoord", JSON.stringify(entries, null, " "));
          }
        };

        // Initialize the bounding-box annotator.
        var annotator = new BBoxAnnotator(options);

        // Initialize the reset button.
        $("#reset_button").click(function(e) {
            annotator.clear_all();
        })

        // define function to reset the bbox
        // ...upon choosing new label category or new url
        function reset_bbox(options) {
          document.getElementById("bbox_annotator").setAttribute( "style", "display:inline-block; border-style: dashed; border-color: LightGrey;"); 
          $(".image_frame").remove();
          annotator = new BBoxAnnotator(options);
        }

        // update image url from shiny
        Shiny.addCustomMessageHandler("change-img-url", function(url) {
          options.url = url;
          options.width = null;
          options.height = null;
          reset_bbox(options);
        });

        // update colors and categories from shiny
        Shiny.addCustomMessageHandler("update-category-list", function(vals) {
          options.labels = Object.values(vals);
          options.color_list = Object.keys(vals);
          reset_bbox(options);
        });

        // redraw rectangles based on list of entries
        Shiny.addCustomMessageHandler("redraw-rects", function(vals) {
          var arr = JSON.parse(vals);
          arr.forEach(function(rect){
             annotator.add_entry(rect);
          });
          if (annotator.onchange) {
             annotator.onchange(annotator.entries);
          }
        }); 
    });
'
#max-width: 1024px;max-height: 800px
ui <- fluidPage(
  tags$head(tags$script(HTML(js)),
            tags$head(
              tags$script(src = "bbox_annotation.js")
            )),
  
  titlePanel("Bounding box annotator demo"),
  sidebarLayout(
    sidebarPanel(
      fileInput("img_url", "Choose a file", accept = c('image/png', 'image/jpeg', 'image/jpg', 'application/pdf')),  
      #selectInput(
      #"img_url",
      #"URLs",
      #c(
      #  "https://www.r-project.org/logo/Rlogo.svg",
      #   "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"
      #  )
      # ),
      selectInput("category_type", "Label Category", c("FUNDinfo", "VOTEinfos")),  #c("animals", "fruits")
      div(HTML(
        '<input id="reset_button" type="reset" />'
      )),
      HTML(
        '<input id="annotation_data" name="annotation_data" type="hidden" />'
      ),
      hr(),
      h4("Entries"),
      downloadButton("downloadData", 
                     label = "Download",
                     icon = icon("file-download")
      ),
      dataTableOutput("rectCoordOutput")
    ),
    mainPanel(
      div(id = "bbox_annotator", style = "display:inline-block;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      uiOutput('formrecognize')
    ),
    mainPanel(
      div(
        uiOutput('cropimgs'),
        style = " border-style: dashed; border-color: LightGrey;max-width: 1000px;max-height: 1000px;"
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # bbob_annotation data
  labeldata <- reactive({
    if(!is.null(input$rectCoord)){
      if( gsub("\\[\\]", "", input$rectCoord)!= "" ){
        mylabeldata <<-
          as.data.frame(
            jsonlite::fromJSON(input$rectCoord)
          ) %>%
          mutate(
            'mappingcode'= 
              paste0(
                width, 
                height,
                left, 
                top
              )
          )
      }else{NULL}
    }
  })
  
  # generate ocr result
  ocr_recognize <- reactive({
    if(!is.null(input$rectCoord)  ){
      if( gsub("\\[\\]", "", input$rectCoord)!= "" ){
        load_images <- loaded_image()
        load_images_length <- load_images %>% length
        Reg_Texts <<-
          sapply(X= 1:load_images_length,
                 y= load_images,
                 function(x,y){
                   ocr(y[[x]] , engine = tesseract("chi_tra")) %>%
                     gsub(" |\n", "", .)
                 }
          ) %>%
          data.frame("Reg_Text"=. ) %>%
          mutate(
            'mappingcode'= 
              paste0(
                labeldata()$width, 
                labeldata()$height,
                labeldata()$left, 
                labeldata()$top
              )
          )
      }else{NULL}
    }
  })
  
  
  #labeltb1 <- reactiveValues(tb = NULL)
  
  # Crop data table
  output$rectCoordOutput <- renderDataTable({
    if(!is.null(input$rectCoord) ){
      if( gsub("\\[\\]", "", input$rectCoord) != "" ){
        labeltb1 <<-
          if(exists("labeltb1")){
            merge(labeldata(),ocr_recognize(), by='mappingcode', all.x=T) %>%
              merge(.,
                    labeltb1 %>%
                      mutate(
                        'mappingcode'=
                          paste0(
                            width,
                            height,
                            left,
                            top
                          )
                      )%>%
                      select('mappingcode', 'Adjusted_Text'),
                    by='mappingcode',
                    all.x =T
              ) %>%
              select(-'mappingcode')
          }else{
            print(2)
            merge(labeldata(),ocr_recognize(), by='mappingcode', all.x=T) %>%
              mutate(
                'Adjusted_Text'=""
              ) %>%
              select(-'mappingcode')
          }
      }
    }
  },
  escape=FALSE,
  options = list(scrollX = TRUE),
  editable = list( target = "cell", disable = list(columns = c(1:7)))
  )
  
  #update data of cropped and recognized
  observeEvent(input$rectCoordOutput_cell_edit, {
    labeltb1 <<-
      merge(labeldata(),ocr_recognize(), by='mappingcode', all.x=T) %>%
      merge(.,
            editData(labeltb1, 
                     input$rectCoordOutput_cell_edit, 
                     'rectCoordOutput') %>%
              mutate(
                'mappingcode'=
                  paste0(
                    width,
                    height,
                    left,
                    top
                  )
              )%>%
              select('mappingcode', 'Adjusted_Text'),
            by='mappingcode',
            all.x =T
      ) %>%
      select(-'mappingcode')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(labeltb1, file)
    }
  )
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(labeltb1, file)
  #   }
  # )
  # lapply(
  #   X=loaded_image(),
  #   function(x){
  #     image_write(x, format = "png")
  #   }      
  # )
  # Turn image file into url
  base64 <- reactive({
    inFile <<- input$img_url
    #grepl("pdf",inFile$type)
    if(!is.null(inFile)){
      # if(grepl("pdf",inFile$type)){
      #   tiff_file <- tempfile()
      #   pdftools::pdf_convert(inFile)
      #   myimgurl <-
      #     image_write(, path = tiff_file, format = 'png') %>%
      #     paste()
      #     dataURI(file = inFile$datapath, mime =  c('image/png', 'image/jpeg', 'image/jpg'))
      # }else{
      #test <<- pdftools::pdf_convert(inFile$datapath)
      myimgurl <- dataURI(file = inFile$datapath, mime =  c('image/png', 'image/jpeg', 'image/jpg'))
      # }
    }
  })
  
  
  # output[["image"]] <- renderUI({
  #   if(!is.null(base64())){
  #     tags$div(
  #       tags$img(src= base64(), width="100%"),
  #       style = "width: 1200px;"
  #     )
  #   }
  # })
  # onclick('Click',
  #         showModal(
  #           modalDialog(
  #             #size = 'l',
  #             footer = NULL,
  #             easyClose = TRUE,
  #             div(id = "bbox_annotator", style = "display:inline-block;"),
  #             tags$script(HTML(js))
  #             )
  #           )
  #         )
  
  
  # send chosen URL from shiny to JS
  observeEvent(input$img_url, {
    session$sendCustomMessage("change-img-url", base64())
  })
  
  #send chosen category list from shiny to JS
  observeEvent(input$category_type, {
    vals <- switch(input$category_type,
                   FUNDinfo = list("red" = "title",
                                   "blue" = "contents"),
                   VOTEinfo = list("red" = "title",
                                   "blue" = "contents")
    )
    
    # update category list
    session$sendCustomMessage("update-category-list", vals)
    # redraw rectangles
    session$sendCustomMessage("redraw-rects", input$rectCoord)
    
  })
  
  loaded_image <- reactive({
    #nrow(labeldata()) != 0 %>% print
    
    crop_position <<- 
      paste0(
        labeldata()$width,
        'x', 
        labeldata()$height,
        '+',
        labeldata()$left, 
        '+',
        labeldata()$top
      )
    if(crop_position[1] != 'x++'){
      lapply(
        X=crop_position,
        function(x,img){
          magick::image_read(req(input$img_url$datapath)) %>%
            image_crop(., x)
        }
      )
    }else{
      NULL
    }
  })
  
  
  
  output$formrecognize <-
    renderUI({
      load_images <- loaded_image()
      load_images_length <- load_images %>% length
      
      if( !is.null(loaded_image()) ){
        lapply(X= 1:load_images_length,
               y= load_images,
               function(x,y){
                 outputname <- paste0('formrecognize',x,sep='_') 
                 verbatimTextOutput(outputname)
                 output[[outputname]] <-
                   renderPrint({
                     ocr(y[[x]] , engine = tesseract("chi_tra")) %>% 
                       gsub(" |\n", "", .)
                   })
               }
        )
      }
    })
  
  
  output$cropimgs <-
    renderUI({
      load_images <- loaded_image()
      load_images_length <- load_images %>% length
      if( !is.null(loaded_image()) ){
        lapply(X= 1:load_images_length,
               y= load_images,
               function(x,y){
                 outputname <- paste0('cropimg',x,sep='_') 
                 plotOutput(outputname)
                 output[[outputname]] <-
                   renderPlot({
                     image_ggplot(
                       y[[x]] 
                     )
                   },
                   width = image_info( y[[x]] )$width,
                   height = image_info( y[[x]] )$height)
               }
        )
      }
    })
}

# args <- commandArgs(trailingOnly = TRUE)
# cat(paste('Input PORT :', args), '\n')
# if(identical(args,character(0))){
#   args <- 3333
# }
# options(shiny.port = as.integer(args))
# options(shiny.host = '10.98.12.151')


# Run the application
shinyApp(ui = ui, server = server)
