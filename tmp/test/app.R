library(shiny)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  sliderInput(
    inputId = "Annotation Title Font Size",
    label = "Annotation Title Font Size",
    min = 4,
    max = 20,
    value = 5
  ),
  tags$div(id = 'id2_div',
           sliderInput(
             inputId = "i2",
             label = "i2",
             min = 4,
             max = 20,
             value = 5
           )),
  actionButton("check", "Print value of i2"),
  actionButton("remove", "Remove i2"),
  actionButton("hide", "Hide i2"),
  actionButton("add", "Add a new i2"),
  actionButton("addX", "Add a new i2 with template_idx"),
  verbatimTextOutput("console")

)

server <- function(input, output) {
  template_idx = reactiveVal(value=0)
  observeEvent(input$check,
               output$console <- renderText(input$i2)
  )

  observeEvent(input$remove,
               #removeUI(selector = sprintf('.shiny-input-container:has(#%s)',el_id))
               {
                 removeUI(selector = '.shiny-input-container:has(#i2)')
                 shinyjs::runjs('Shiny.onInputChange("i2", null)')
                 output$console <- renderText("")

                }
  )

  observeEvent(input$hide,
               #removeUI(selector = sprintf('.shiny-input-container:has(#%s)',el_id))
               {
                 shinyjs::hide(id = "#id2_div")
                 output$console <- renderText("")

               }
  )

  observeEvent(input$add,
               #removeUI(selector = sprintf('.shiny-input-container:has(#%s)',el_id))
               {
                 insertUI('#console','afterEnd',
                          tags$div(id = 'id2_div',
                                   sliderInput(
                                     inputId = "i2",
                                     label = "i2",
                                     min = 4,
                                     max = 20,
                                     value = 5
                                   )))

               })

               observeEvent(input$addX,
                            #removeUI(selector = sprintf('.shiny-input-container:has(#%s)',el_id))
                            {
                              template_idx = template_idx() + 1
                              insertUI('#console','afterEnd',
                                       tags$div(id = paste0('id2_div',template_idx),
                                                sliderInput(
                                                  inputId = paste0('i2',template_idx),
                                                  label = "i2",
                                                  min = 4,
                                                  max = 20,
                                                  value = 5
                                                )))

                            }
  )

}

shinyApp(ui = ui, server = server)
