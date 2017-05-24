#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Flow Dashboard"),
    dashboardSidebar(
      sidebarMenu(id = "sidebarmenu",
                  menuItem("QC", tabName = "QCDash", selected = TRUE),
                  conditionalPanel("input.sidebarmenu === 'QCDash'",
                                   subsetModuleUICDO(QCO)),

                   menuItem("Gating", tabName="GatingDash", selected = FALSE),
                   conditionalPanel("input.sidebarmenu === 'GatingDash'",
                                    subsetModuleUICDO(GO)),

                  menuItem("Expression", tabName="PopExpression", selected=FALSE),
                 conditionalPanel("input.sidebarmenu === 'PopExpression'",
                                   subsetModuleUICDO(PEO)),

                  menuItem("dotPlots", tabName="DotPlot", selected=FALSE),
                  conditionalPanel("input.sidebarmenu === 'DotPlot'",
                                   subsetModuleUICDO(GO, objId = goObjId2)),

                 menuItem("Waterfall", tabName="Waterfall", selected=FALSE),
                 conditionalPanel("input.sidebarmenu === 'Waterfall'",
                                  subsetModuleUICDO(GO, objId = goObjId3))


    )),
    dashboardBody(
      tabItems(
        tabItem(tabName="QCDash",
                box(
                  absolutePanel(qcModuleUIFromQCO(QCO), fixed=FALSE),
                  selected=TRUE)),

         tabItem(tabName = "GatingDash",
                 #div(style = 'overflow-x: scroll',

                 box(
                       absolutePanel(id="heatmap",
                                     h4("Population Heatmap (Click on box to see provenance)"),
                                     ggvisOutput("populationHeatmap"), top=250, left=0, fixed=FALSE)
                       ),

                   absolutePanel(id="gating",draggable=TRUE,top=0, left=0,
                               fixed=FALSE,
                               style="opacity: 0.8; background-color: white",
                               height=200,width="auto",
                               h4("Gating Scheme (draggable)"),
                               imageOutput("gating"))

                 ),

       tabItem(tabName = "PopExpression", violinUIFromCDO(PEO)),
      tabItem(tabName= "DotPlot", dotPlotUIFromGO(GO, objId = goObjId2)),
      tabItem(tabName="Waterfall", waterfallOutputUIfromGO(GO, objId= goObjId3))

      )
    )
))
