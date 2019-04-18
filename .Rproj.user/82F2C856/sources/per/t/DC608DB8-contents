source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)



TabItemDash <- R6Class(
  "TabItemDash",
  public = list(

    # Fields
    title = NULL,
    inputId = NULL,
    tabNumber = NULL,
    # Constructor
    initialize = function(
      title, 
      inputId, 
      tabNumber
    ){
      self$title = title
      self$inputId = inputId
      self$tabNumber = tabNumber
    },

    tabItem = function(){


    },
    # REQUIRES: iterate is an integer or NULL
    # EFFECTS: given a string, create an id based on the tab number
    #          if iterate is an integer, create id based on tab number
    #          and iteration
    makeId = function(string, id, iterate = NULL){
      if(is.null(iterate)){
        self[[id]] = paste0(string, self$tabNumber)
        if("output" %in% id || "Output" %in% id){
          self$outputIds = c(self$outputIds, paste0(string, self$tabNumber))
        }
      } else {
        if(!is.integer(iterate)){
          sout(iterate, "is not an integer")
          stop("iterate must be an integer, or null")
        }
        ids = c()
        for(i in 1:iterate){
          ids = c(ids, paste0(string, self$tabNumber, i))
        }
        self[[id]] = ids
        if("output" %in% id || "Output" %in% id){
          self$outputIds = c(self$outputIds, ids)
        }
      }
    }

  ))


