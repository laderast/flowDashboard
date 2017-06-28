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

                  menuItem("GatingGG", tabName="GatingFromGO", selected = FALSE),
                  conditionalPanel("input.sidebarmenu === 'GatingFromGO'",
                                   subsetModuleUICDO(GO, objId = "GObj4")),

                  menuItem("Expression", tabName="PopExpression", selected=FALSE),
                 conditionalPanel("input.sidebarmenu === 'PopExpression'",
                                   subsetModuleUICDO(PEO)),

                  menuItem("dotPlots", tabName="DotPlot", selected=FALSE),
                  conditionalPanel("input.sidebarmenu === 'DotPlot'",
                                   subsetModuleUICDO(GO, objId = goObjId2)),

                 menuItem("Waterfall1", tabName="Waterfall", selected=FALSE)#,
                 #conditionalPanel("input.sidebarmenu === 'Waterfall1'",
                  #                subsetModuleUICDO(GOadam))#,

#                 menuItem("Waterfall2", tabName="Waterfall2", selected=FALSE),
#                 conditionalPanel("input.sidebarmenu === 'Waterfall2'",
#                                  subsetModuleUICDO(GOCD4CD8))


    )),
    dashboardBody(
      tabItems(
        tabItem(tabName="QCDash",
                #box(
                  absolutePanel(qcModuleUIFromQCO(QCO)),
                  selected=TRUE), #),

        tabItem(tabName = "GatingFromGO",
                plotOutput("test", click = "clickGate"),
                div(style= 'overflow-x: scroll',
                    gatingModuleUIFromGO(GO, objId = "GObj4")

                    )
        ),

         tabItem(tabName = "GatingDash",
                 div(style = 'overflow-x: scroll',
                 box(
                   absolutePanel(id="gating",draggable=TRUE,top=0, left=0,
                                 fixed=FALSE,
                                 style="opacity: 0.8; background-color: white",
                                 height=200,width="auto",
                                 h4("Gating Scheme (draggable)"),
                                 imageOutput("gating")),
                   width = 12, height=225#, height=length(GO$populations)*60
                 ),
                 box(
                       #absolutePanel(id="heatmap",
                                     h4("Population Heatmap (Click on box to see provenance)"),
                                     ggvisOutput("populationHeatmap"),#, top=250, left=0, fixed=FALSE),
                       width = 12)


                 )
                ),

      tabItem(tabName = "PopExpression", box(violinUIFromCDO(PEO), width=10)),
      tabItem(tabName= "DotPlot", box(dotPlotUIFromGO(GO, objId = goObjId2),width=10)),
      tabItem(tabName="Waterfall", box(waterfallOutputUIfromGO(GO), width=10))#,

      )
    )
))
