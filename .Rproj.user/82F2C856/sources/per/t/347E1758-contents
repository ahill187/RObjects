source('./R/helper_functions.R')
source('./R/TabItemDash.R')
source('./R/utils.R')
source('./R/helper_functions.R')

#source('../R/initialize.R')
library(R6)
library(shiny)



TabItemDashGraph <- R6Class(
  "TabItemDashGraph",
  inherit = TabItemDash,
  public = list(

    # Fields -----

    # Generic
    mainBoxColor = "info",
    # Graph
    graphInputId = NULL,
    graphOutputId = NULL,
    googleChartOutputId = NULL,
    # Input for Graph
    dataInput = c("userInput", "csvInput", "functionInput"),
    # Dropdown for Graph
    dropdownId = NULL,
    dropdownChoices = NULL,
    dropdownSelected = NULL,
    dropdown = TRUE,
    # Sidebar
    sidebarChoicesNumber = 4,
    sidebarShownLabels = NULL,
    sidebarShownChoices = c(),
    sidebarHiddenChoices = c(),
    sidebarShownSelected = NULL,
    sidebarHiddenSelected = NULL,
    sidebarShownIds = NULL,
    sidebarHiddenIds = NULL,
    sidebarHiddenBoxIds = NULL,
    # Download
    downloadLabel = "Download",
    downloadInputId = NULL,
    downloadOutputId = NULL,
    pngDownloadName = NULL,
    # Server Output
    outputTypes = c("googleChartOutput", "downloadOutput"),
    outputIds = c(),
    chartType = "googleChart",
    # App Bricks
    brickTypes = c("sidebarInput", "googleChartOutput", "downloadOutput"),


    # Constructor
    initialize = function(
      title,
      inputId,
      mainBoxColor,
      tabNumber,
      dropdownChoices,
      dropdownSelected,
      dropdown = TRUE,
      sidebarChoicesNumber,
      sidebarShownLabels,
      downloadLabel = "Download",
      pngDownloadName,
      dataSubClasses,
      columnOptions,
      columnTypes

    ){
      super$initialize(title, inputId, tabNumber)
      self$mainBoxColor = mainBoxColor
      self$dropdownChoices = dropdownChoices
      self$dropdownSelected = dropdownSelected
      self$dropdown = dropdown
      self$sidebarChoicesNumber = sidebarChoicesNumber
      self$sidebarShownLabels = sidebarShownLabels
      self$downloadLabel = downloadLabel
      self$pngDownloadName = pngDownloadName
      self$makeId("graphInput", "graphInputId")
      self$makeId("graphOutput", "graphOutputId")
      self$makeId("googleChartOutput", "googleChartOutputId")
      self$makeId("dropdown", "dropdownId")
      self$makeId("shown", "sidebarShownIds", self$sidebarChoicesNumber)
      self$makeId("hidden", "sidebarHiddenIds", self$sidebarChoicesNumber)
      self$makeId("hiddenBox", "sidebarHiddenBoxIds", self$sidebarChoicesNumber)
      self$makeId("downloadInput", "downloadInputId")
      self$makeId("downloadOutput", "downloadOutputId")
      self$generateSidebarChoices(columnNames = sidebarShownLabels, columnTypes, columnOptions, dataSubClasses)
    },

    makeRadioButtons = function(){
      sidebarPanelList = list()
      for(i in 1:self$sidebarChoicesNumber){

        showHideButtons = radioButtonsColl(
          inputId = self$sidebarShownIds[i],
          label = self$sidebarShownLabels[i],
          choices = self$sidebarShownChoices[[i]],
          selected = self$sidebarShownSelected[i],
          hiddenId = self$sidebarHiddenIds[i],
          hiddenIdBox = self$sidebarHiddenBoxIds[i],
          hiddenChoices = self$sidebarHiddenChoices[[i]],
          hiddenSelected = self$sidebarHiddenSelected[i]
        )
        #showHideButtons = flatten(showHideButtons)
        sizeOfList = length(sidebarPanelList)
        sidebarPanelList[[sizeOfList+1]] = showHideButtons$show
        sidebarPanelList[[sizeOfList+2]] = showHideButtons$hide
      }
      return(sidebarPanelList)
    },

    makeSelectInput = function(){
      sidebarPanelList = list()
      for(i in 1:self$sidebarChoicesNumber){
        inputElement = selectInput(inputId = self$sidebarShownIds[i],
                                   label = self$sidebarShownLabels[i],
                                   choices = self$sidebarHiddenChoices[[i]],
                                   selected = self$sidebarHiddenChoices[[i]][1])
        sizeOfList = length(sidebarPanelList)
        sidebarPanelList[[sizeOfList+1]] = inputElement
      }

      return(sidebarPanelList)
    },

    makeSidebarPanel = function(chartType){
      if(chartType=="googleChart") {
        sidebarPanelList = self$makeSelectInput()
      } else {
        sidebarPanelList = self$makeRadioButtons()
      }
      return(sidebarPanelList)
    },

    # Override
    tabItem = function(){
      shinydashboard2::tabItem(
        tabName = self$inputId,
        # Box Container
        box(solidHeader=FALSE,
            width = 12,
            status=self$mainBoxColor,
            sidebarLayout(
              # Sidebar Inputs for Graph
              sidebarPanel(
                self$makeSidebarPanel(self$chartType)
              ),
              mainPanel(
                self$makeMainPanel()
              )
            )))
            },

#EFFECTS: given a list of DataSubClass objects, create the sidebar menu based off
        # the options pulled from the csv file
    #' @param dataSubClasses list of type DataSubClass
    generateSidebarChoicesData = function(dataSubClasses){
      if(!is.list(dataSubClasses)){
        stop("dataSubClasses must be a list containing objects of class DataSubClass")
      }
      checkClass = class(dataSubClasses[[1]])
      if(!"DataSubClass" %in% checkClass){
        stop("Objects in list must be of class DataSubClass")
      }
      self$sidebarHiddenChoices = list()
      self$sidebarShownChoices = list()
      for(i in 1:length(dataSubClasses)){
        self$sidebarHiddenChoices = c(self$sidebarHiddenChoices, list(dataSubClasses[[i]]$options))
        self$sidebarShownChoices[[i]] = c("All", "Select")
        self$sidebarShownSelected = c(self$sidebarShownSelected, c("All"))
        self$sidebarHiddenSelected = c(self$sidebarHiddenSelected, c("All"))
      }
    },

  generateSidebarChoices = function(columnNames, columnTypes, columnOptionsList, dataSubClasses){
    if(!is.list(dataSubClasses)){
      stop("dataSubClasses must be a list containing objects of class DataSubClass")
    }
    checkClass = class(dataSubClasses[[1]])
    if(!"DataSubClass" %in% checkClass){
      stop("Objects in list must be of class DataSubClass")
    }
    self$sidebarHiddenChoices = list()
    self$sidebarShownChoices = list()
    dataSubClassNames = names(dataSubClasses)
    i = 1
    for(columnOptions in columnOptionsList){
      if(columnOptions == "generate"){
        index = which(dataSubClassNames==columnTypes[i])
        self$sidebarHiddenChoices = c(self$sidebarHiddenChoices, list(dataSubClasses[[index]]$options))
        self$sidebarShownChoices[[i]] = c("All", "Select")
        self$sidebarShownSelected = c(self$sidebarShownSelected, c("All"))
        self$sidebarHiddenSelected = c(self$sidebarHiddenSelected, c("All"))
      } else {
        self$sidebarHiddenChoices = c(self$sidebarHiddenChoices, list(columnOptions))
      }
      i = i + 1
    }
  },

makeMainPanel = function(){

  # Graph Output
  b = plotlyOutput(outputId = self$graphOutputId)
  # Download Data Button
  c = div(id = self$downloadInputId,
          downloadButton(self$downloadOutputId,
                         self$downloadLabel))
  #d = div(id="series_chart_div", style="width: 900px; height: 500px;")
  # Google Chart Output
  e = googleChartOutput(outputId=self$googleChartOutputId)

  if(self$dropdown){
    # Dropdown Menu for Graph
    a = selectizeInput(inputId=self$dropdownId,
                       label="",
                       options = list(style="z-index:100;"),
                       choices = self$dropdownChoices,
                       selected = self$dropdownSelected)
    return(list(a,c,e))} else {
      return(list(c,e))
    }

}


  ))






