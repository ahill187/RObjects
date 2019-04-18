#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(googleCharts)
library(shiny)
library(shinythemes)
library(shinydashboard2)
library(devtools)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)
library(maps) # interactive map
library(mapproj)
library(leaflet)
source("./R/Census.R")
source("./R/MapData.R")
source("./R/helper_functions.R")
source("./R/AppLayout.R")
source("./R/DashGraph.R")
source("./R/LeafletMap.R")
source("./R/CountryBaseMap.R")
source("./R/colorSchemes.R")
source("./R/BubbleChartOptions.R")
source("./R/extendShinyjs2.R")

load(file = "./data/cleanedRawData.RData")
load(file = "./data/leafletMapList.RData")
load(file = "./data/appData.RData")
load(file = "./data/tabItemsList.RData")

tab_titles = appData$appLayout$mainTabs$titles
numberOfTabs = appData$appLayout$subTabs$number
valueBoxIcons = list(icon("user", lib="font-awesome"), icon("usd", lib="font-awesome"))
i = 1
appLayout <- AppLayout$new(6, "burdenOfAsthma")
initialize = TRUE
cat("~~~ Starting UI ~~~", fill = T)

ui <- dashboardPage(
    skin = appLayout$dashboardColour,

    # header
    dashboardHeader(title = appData$title, titleWidth =
                        320),
    # sidebar
    dashboardSidebar(
        sidebarMenu(
            id = "selectedTab",
            menuItem(tab_titles[1],tabName = "costTab",icon = icon("dollar sign", lib = "font-awesome"),
                menuSubItem("Map",tabName = appData$tabs$inputId[1],
                            icon = icon("globe americas", lib = "font-awesome")),
                menuSubItem("Graph",tabName = appData$tabs$inputId[2],
                    icon = icon("bar-chart", lib = "font-awesome"))
            ),
            menuItem(
                tab_titles[2],
                tabName = "qalyTab",
                icon = icon("sort numeric up", lib = "font-awesome"),
                menuSubItem(
                    "Map",
                    tabName = appData$tabs$inputId[3],
                    icon = icon("globe americas", lib = "font-awesome")
                ),
                menuSubItem(
                    "Graph",
                    tabName = appData$tabs$inputId[4],
                    icon = icon("bar-chart", lib = "font-awesome")
                )
            ),

            menuItem(
                tab_titles[3],
                tabName = appData$tabs$inputId[5],
                icon = icon("address-book", lib = "font-awesome")
            ),
            menuItem(
                tab_titles[4],
                tabName = appData$tabs$inputId[6],
                icon = icon("balance-scale", lib = "font-awesome")
            )
        )
    ),
    # body
    dashboardBody(tabItems(
                      asList = T,

                      lapply(1:numberOfTabs, function(i) {
                          tabItemsList[[i]]$tabItem()
                      })
                  ),
                  asList = T)
)

server <- function(input, output, session) {
    cat("~~~ Starting server ~~~", fill = T)
    colorScheme = colorSchemes[[appData$appLayout$colorScheme]]
    filename = "./static_data/USMap.RData"
    init = TRUE
    if (init) {
        countryBaseMap <- CountryBaseMap$new("US", filename, init = TRUE)
    } else{
        load(filename)
    }

    observe({
        selectedTabItem <- tabItemsList[[input$selectedTab]]
        if (tabItemsList[[input$selectedTab]]$title == "Graph") {
            selectedTabItem <- tabItemsList[[input$selectedTab]]
            for (i in 1:selectedTabItem$sidebarChoicesNumber) {
                sidebarShownInput <- input[[selectedTabItem$sidebarShownIds[i]]]
                sout(selectedTabItem$sidebarHiddenIds[i])
                if (!is.null(sidebarShownInput) &&
                    sidebarShownInput == "Select") {
                    shinyjs::show (id = selectedTabItem$sidebarHiddenIds[i], anim = TRUE)
                }
                else {
                    shinyjs::hide (id = selectedTabItem$sidebarHiddenIds[i], anim = TRUE)
                }

            }
        }
    })

    lapply(1:numberOfTabs, function(i) {
        outputTypes = tabItemsList[[i]]$outputTypes
        tabItemDash = tabItemsList[[i]]
        sout("Setting up tab components for tab:", i)
        if (is.null(outputTypes)) {

        } else{
            lapply(1:length(outputTypes), function(k) {
                outputType = outputTypes[k]
                if (outputType == "inputSidebar") {

                }
                # OutputType 0: Google Chart
                if (outputType == "googleChartOutput") {
                    googleChartId = tabItemDash$googleChartOutputId
                    tabNumber = reactive({
                        tabItemsList[[input$selectedTab]]$tabNumber
                    })
                    columnOptions = reactive({
                        return(appData$tabs$columnOptions[[tabNumber()]])
                    })
                    sidebarIds = reactive({
                        tabItemsList[[tabNumber()]]$sidebarShownIds
                    })
                    allValues = c("", "", "All States", "All States", "All Years", "", "")
                    # Update the sidebar inputs based on other input values
                    lapply(1:length(allValues), function(index) {
                        observe({
                            labels = appData$tabs$sidebarShownLabels[[tabNumber()]]
                            types = appData$tabs$columnTypes[[tabNumber()]]
                            ids = tabItemsList[[tabNumber()]]$sidebarShownIds
                            columnTypes = appData$tabs$columnTypes[[tabNumber()]]
                            updateOptions = getOptions(
                                isolate(input), sidebarIds(),columnOptions(),
                                types[index], index, types,
                                tabItemsList[[tabNumber()]]$sidebarHiddenChoices[[index]],
                                allValues[index])
                            print(types[index])
                            print(updateOptions)
                            if(updateOptions!=FALSE){
                                updateSelectInput(session, ids[index], label = labels[index],
                                                  choices = updateOptions)}
                        })
                    })

                    output[[googleChartId]] = renderGoogleChart({
                        selectedTabItem = tabItemsList[[input$selectedTab]]
                        sidebarShownIds = selectedTabItem$sidebarShownIds
                        year = input[[sidebarShownIds[5]]]
                        xAxis = input[[sidebarShownIds[1]]]
                        yAxis = input[[sidebarShownIds[2]]]
                        state1 = input[[sidebarShownIds[3]]]
                        state2 = input[[sidebarShownIds[4]]]
                        color = input[[sidebarShownIds[6]]]
                        size = input[[sidebarShownIds[7]]]
                        id = appData$tabs$column0[[tabNumber()]]
                        if (color == "None") {
                            color = NULL
                        }
                        if (size == "None") {
                            size = NULL

                        }
                        if (year == "All Years") {
                            year = c(19, 20, 21, 22, 23, 24, 25)
                        } else {
                            year = as.numeric(year)
                        }
                        googleChart(
                            title = "",
                            elementId = googleChartId,
                            chartType = "bubbleChart",
                            headerTitles = c(id, xAxis, yAxis, color, size),
                            data = rawData$subsetData(
                                rawData$cleanedData,
                                list("State", c(state1, state2)),
                                list("Sex", c("Female", "Male")),
                                list(
                                    "Age",
                                    c(
                                        "15 to 19 years",
                                        "20 to 24 years",
                                        "25 to 29 years",
                                        "30 to 34 years",
                                        "35 to 39 years",
                                        "80 to 84 years"
                                    )
                                ),
                                list("Year", year)
                            ),
                            column0 = id,
                            column1 = xAxis,
                            column2 = yAxis,
                            column3 = color,
                            column4 = size,
                            keyColumns = appData$data$classNames,
                            valueColumns = appData$data$valueNames,
                            transform = TRUE
                        )
                    })
                }
                # Output Type 1: Plotly Output
                if (outputType == "plotlyOutput") {
                    outputId = tabItemDash$graphOutputId
                    output[[outputId]] <- renderPlotly({
                        sout("~~~ Making Graph ~~~")
                        dataSubClassNames = c("Year", "State", "Sex", "Age")
                        dashGraph <- DashGraph$new(dataSubClassNames)
                        p <- reactive({
                            selectedTabItem <- tabItemsList[[input$selectedTab]]
                            if (selectedTabItem$dropdown) {
                                dropdownSelected = input[[selectedTabItem$dropdownId]]
                            } else {
                                dropdownSelected = selectedTabItem$dropdownChoices
                            }
                            arguments = list()

                            count = 3
                            print(count)
                            for (i in 1:length(dataSubClassNames)) {
                                shown = selectedTabItem$sidebarShownIds[i]
                                hidden = selectedTabItem$sidebarHiddenBoxIds[i]
                                if (input[[shown]] == "All" ||
                                    is.null(input[[hidden]])) {
                                    arguments[[count]] <- list(dataSubClassNames[i], "total")
                                } else if (input[[shown]] == "Select") {
                                    arguments[[count]] <- list(dataSubClassNames[i], input[[hidden]])
                                }
                                count = count + 1

                            }
                            arguments[[1]] <- rawData
                            arguments[[2]] <- dropdownSelected
                            print(dropdownSelected)
                            print(arguments)
                            do.call(dashGraph$drawGraph, args = arguments)
                            # dashGraph$drawGraph(rawData, dropdownSelected, list("Year", 19), list("Sex", "Female"),
                            #                     list("State", c("Iowa", "Washington")))
                        })
                        p()
                    })
                }
                # OutputType 2: Download
                else if (outputType == "downloadOutput") {
                    sout("~~~ Download ~~~")
                    outputId = tabItemDash$downloadOutputId
                    output[[outputId]] <- downloadHandler(
                        filename = function() {
                            paste(tabItemDash$pngDownloadName,
                                  Sys.Date(),
                                  ".png",
                                  sep = "")
                        },
                        content = function(file) {
                            ggsave(
                                file,
                                device = "png",
                                width = 11,
                                height = 8.5
                            )
                        }
                    )
                }
                # OutputType 3: Image Output
                else if (outputType == "imageOutput") {
                    output[[tabItemDash$imageId]] <- renderImage({
                        width  <- session$clientData$output_logos_width
                        height <- session$clientData$output_logos_height
                        # Return a list containing the filename
                        sout("All good")
                        list(
                            src = paste0("./static_data/", tabItemDash$imFile),
                            contentType = 'image/png',
                            width = width,
                            alt = "Logos"
                        )
                    }, deleteFile = FALSE)
                }
                # OutputType 4: Leaflet Output
                else if (outputType == "leafletOutput") {
                    cat("~~~ Leaflet Map ~~~", fill = T)
                    mapOutputId = tabItemDash$mapOutputId
                    leafletMap <- reactive({
                        selectedTab <- input$selectedTab
                        leafletMap <- leafletMapList[[selectedTab]]
                        return(leafletMap)
                    })
                    year <- 19

                    output[[mapOutputId]] <- renderLeaflet({
                        leafletMap()$drawMap(year)
                    })
                    cat("~~~ Setting up Info Boxes ~~~", fill = T)

                    mapShapeClick <- paste0(mapOutputId, "_shape_click")
                    changeLayer <-
                        paste0(mapOutputId, "_groups_baselayerchange")
                    value <- reactiveValues(noClickYet = FALSE, layer = 1)
                    valueBoxOutputIds <- tabItemDash$valueBoxOutputIds
                    lapply(1:tabItemDash$valueBoxNumber, function(box) {
                        boxId <- valueBoxOutputIds[box]
                        observeEvent(input[[mapShapeClick]], {
                            print(input[[mapShapeClick]])
                            value$default <- input[[mapShapeClick]]$id
                        })
                        sout("Still working")
                        output[[boxId]] <- renderValueBox({
                            valueName = tabItemDash$valueBoxChoices[box] # column of data to view in box
                            region <- eventReactive(input[[mapShapeClick]],
                                                    ignoreNULL = FALSE, {
                                                        # update the location selectInput on map clicks
                                                        input[[mapShapeClick]]$id
                                                    })
                            layerRegionId <- region()
                            if (is.null(layerRegionId)) {
                                layerRegionId <- "layer_1_region_1"
                            }
                            layerRegionId = strsplit(layerRegionId, "_")
                            layerId = as.numeric(layerRegionId[[1]][2])
                            regionId = as.numeric(layerRegionId[[1]][4])
                            value <-
                                leafletMap()$getLayerValueData(valueName = valueName,
                                                             year = 19,
                                                             layer = layerId)
                            regionName = leafletMap()$regionNames[regionId]
                            subtitles = names(tabItemDash$valueBoxChoices)
                            if (box == tabItemDash$valueBoxNumber) {
                                subtitle = subtitles[box]
                                subtitle = paste(subtitle, regionName)
                            } else{
                                subtitle = subtitles[box]
                            }
                            if (value[regionId] == 0 ||
                                value[regionId] == "No data") {
                                value = "No data"
                            } else {
                                value = paste(leafletMap()$prefix, value)
                            }
                            valueBox(
                                value = value[regionId],
                                subtitle = subtitle,
                                color = colorScheme[box],
                                icon = valueBoxIcons[[layerId]]
                            )
                        })
                    })
                }
            })
        }
    })



}

# Run the application
shinyApp(ui = ui, server = server)
