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

                   #menuItem("Gating", tabName="GatingDash", selected = FALSE),
                   #conditionalPanel("input.sidebarmenu === 'GatingDash'",
                  #                  subsetModuleUICDO(GO)),

                  menuItem("Expression", tabName="PopExpression", selected=FALSE),
                  conditionalPanel("input.sidebarmenu === 'PopExpression'",
                                   subsetModuleUICDO(PEO)),

                  menuItem("dotPlots", tabName="DotPlot", selected=FALSE),
                  conditionalPanel("input.sidebarmenu === 'DotPlot'",
                                   subsetModuleUICDO(GO))


    )),
    dashboardBody(
      tabItems(
        tabItem(tabName="QCDash",
                  qcModuleUIFromQCO(QCO),
                  selected=TRUE),

        # tabItem(tabName = "GatingDash",
        #         box(
        #           absolutePanel(id="heatmap", h4("Population Heatmap (Click on box to see provenance)"),
        #                       ggvisOutput("populationHeatmap"), top=250, left=0),
        #
        #           absolutePanel(id="gating",draggable=TRUE,top=0, left=0,
        #                       fixed=FALSE,
        #                       style="opacity: 0.8; background-color: white",
        #                       height=200,width="auto",
        #                       h4("Gating Scheme (draggable)"),
        #                       imageOutput("gating")), selected=NULL
        #           )
        #         ),

       tabItem(tabName = "PopExpression"),
      tabItem(tabName= "DotPlot", dotPlotUIFromGO(GO))

      )
    )
))
