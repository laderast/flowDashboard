library(shiny)
library(ggvis)


m <- function(input, output, session){
  ns <- session$ns

  testMake <- reactive({
  mtcars %>% ggvis(~mpg, ~disp) %>%
    layer_points() %>%
    handle_click(function(location,...){print(location)})

  })

  testMake %>% bind_shiny(ns("plot-plot"), session=session)
    #, session=getDefaultReactiveDomain()[["parent"]])
}

mUi <- function(id, label){
  ns <- NS(id)
  tagList(
  ggvisOutput(ns("plot-plot"))
  )
}

ui <- {
  mUi(id="plot")
}

server <- function(input, output, session) {
  callModule(m, 'plot')

}
shinyApp(ui = ui, server = server)


