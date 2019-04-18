

getOptions = function(input, sidebarIds, columnOptions, type, index, columnTypes,
                      allOptions, allValue){
    if(!is.null(sidebarIds)) {
        print("sidebarIds not null")
        xAxis = input[[sidebarIds[1]]]
        yAxis = input[[sidebarIds[2]]]
        if(type == "color") {
            updateOptions = getOptionsColor(xAxis, yAxis, columnOptions)
        } else if(type == "y-axis") {
            updateOptions = getOptionsAxis(xAxis, columnOptions)
        } else if(type == "size") {
            updateOptions = getOptionsSize(xAxis, yAxis, columnOptions)
        } else if(type == "x-axis") {
            return(FALSE)
        } else {
            updateOptions = getOptionsCustom(columnOptions, type, index, columnTypes, input, sidebarIds,
                                             allOptions, allValue)
        }
        return(updateOptions)
    } else {
        return(FALSE)
    }

}

getOptionsAxis = function(xAxis, columnOptions) {
    axisOptions = columnOptions$yAxis
    updateOptions = axisOptions[-which(axisOptions == xAxis)]
    return(updateOptions)
}

getOptionsColor = function(xAxis, yAxis, columnOptions) {
    options = columnOptions$color
    x = which(options == xAxis)
    y = which(options == yAxis)
    if (length(x) != 0) {
        updateOptions = options[-x]
    } else {
        updateOptions = options
    }
    if (length(y) != 0) {
        updateOptions = updateOptions[-y]
    }
    return(updateOptions)
}

getOptionsSize = function(xAxis, yAxis, columnOptions) {
    sizeOptions = columnOptions$size
    x = which(sizeOptions == xAxis)
    y = which(sizeOptions == yAxis)
    if (length(x) != 0) {
        if (length(y) != 0) {
            updateOptions = sizeOptions[-c(x, y)]
        } else {
            updateOptions = sizeOptions[-x]
        }
    } else if (length(y) != 0) {
        updateOptions = sizeOptions[-y]
    } else {
        updateOptions = sizeOptions
    }
    return(updateOptions)
}

getOptionsCustom = function(columnOptions, type, index, columnTypes, input, sidebarIds,
                            allOptions, allValue) {
    columnType = columnTypes[index]
    for(i in 1:(index-1)) {
        if(columnTypes[i]==columnType) {
            updateOptions = getOptionsSameColumn(i, index, input, sidebarIds, allOptions)
            return(updateOptions)
        }
    }
    for(sidebarId in sidebarIds) {
        value = input[[sidebarId]]
        if(columnType == value) {
            updateOptions = c(allValue)
            return(updateOptions)
        }
    }
    return(FALSE)
}

getOptionsSameColumn = function(i, index, input, sidebarIds, allOptions){
        column1 = input[[sidebarIds[i]]]
        updateOptions = allOptions[-which(allOptions == column1)]
        return(updateOptions)

}



