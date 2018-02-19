
library(shiny)
library(shinyjs)
library(RJSONIO)
library(RCurl)
library(warbleR)
library(parallel)

source('gender.R')

# REST service endpoint.(Representational State Transfer)
httpHandler = function(req) {
  if (req$REQUEST_METHOD == "GET") {
    # handle GET requests
    print(req$QUERY_STRING)
    query <- parseQueryString(req$QUERY_STRING)
    # name <- query$name
  }
  else if (req$REQUEST_METHOD == "POST") {
    # handle POST requests here
    reqInput <- req$rook.input
    print(reqInput)
  }  
  
}

shinyServer(function(input, output, session) {
  v <- reactiveValues(data = NULL)
 
  # This observeEvent works with the part of uploading audio file manually.
   
  observeEvent(input$file1, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    content <- ''
    inFile <- input$file1

    # This part works to check if the file name has .wav in its name...
    #if .wav is not found in its name then it will ask to select a .wav file to upload. 
    
    if (grepl('.wav', tolower(inFile$name)) != TRUE) {
      content <- '<div class="shiny-output-error-validation">Please select a .WAV file to upload.</div>'
    }
    else if (!is.null(inFile)) {
      disable('btnUrl')
      disable('url')
      disable('file1')
      
      withProgress(message='Please wait ..', style='old', value=0, {
        result <- processFile(inFile, input$model)
        
        content <- result$content
        if (!is.null(result$graph1)) {
          output$graph1 <- result$graph1
        }
      })
    }
    
    enable('btnUrl')
    enable('url')
    enable('file1')
    
    v$data <- content
  })
  
  # This observeEvent works with the audio file that user uploads through given URL.
  
  observeEvent(input$btnUrl, {
    content <- ''
    url <- input$url
    
    disable('btnUrl')
    disable('url')
    disable('file1')

    if (url != '' && grepl('http', tolower(url)) && (grepl('vocaroo.com', url))) {
      withProgress(message='Please wait ..', style='old', value=0, {
        result <- processUrl(url, input$model)
        
        content <- result$content
        if (!is.null(result$graph1)) {
          output$graph1 <- result$graph1
        }
      })
    }
    else {
      content <- '<div class="shiny-output-error-validation">Please enter a url to vocaroo.</div>'
    }
    
    enable('btnUrl')
    enable('url')
    enable('file1')
    
    v$data <- content
  })
  
  output$content <- eventReactive(v$data, {
    HTML(v$data)
  })
})

processFile <- function(inFile, model) {
  # Create a unique filename.
  id <- sample(1:100000, 1)
  filePath <- paste0('./temp', sample(1:100000, 1), '/temp', id, '.wav')
  
  logEntry('File uploaded.', paste0('"id": "', id, '", "inFile": "', inFile$datapath, '", "filePath": "', filePath, '"'))
  
  currentPath <- getwd()
  fileName <- basename(filePath)
  path <- dirname(filePath)
  
  # Create directory.
  dir.create(path)
  
  incProgress(0.1, message = 'Uploading clip ..')
  
  # Copy the temp file to our local folder.
  file.copy(inFile$datapath, filePath)

  logEntry('File copied.', paste0('"id": "', id, '", inFile": "', inFile$datapath, '", "filePath": "', filePath, '"'))
  
  # Process.
  result <- process(filePath)

  # Unlink() is used to remove the temp file.
  
  unlink(path, recursive = T)
  
  logEntry('Classification done.', paste0('"id": "', id, '", "filePath": "', path, '", "class": "', result$content5$label, '", "prob": "', round(result$content5$prob * 100), '"'))
  
  list(content=formatResult(result), graph1=result$graph1)
}

processUrl <- function(url, model) {
  origUrl <- url
  
  # Create a unique id for the file.
  id <- sample(1:100000, 1)
  
  if (grepl('vocaroo', tolower(url))) {
    # Create a unique filename.
    fileName <- paste0('temp', id, '.wav')
    
    # Get apiId from url.
    apiId <- gsub('.+/i/(\\w+)', '\\1', url)
    url <- paste0('http://vocaroo.com/media_command.php?media=', apiId, '&command=download_wav')
    print(paste('Downloading', url, sep=' '))
    
    incProgress(0.1, message = 'Downloading clip ..')
    
    logEntry('Downloading url.', paste0('"id": "', id, '", "url": "', origUrl, '", "downloadUrl": "', url, '", "fileName": "', fileName, '"'))
    
    # Download wav file.
    download.file(url, fileName)
    
    # Process.        
    result <- process(fileName)
    graph1 <- result$graph1
   
    logEntry('Classification done.', paste0('"id": "', id, '", "url": "', origUrl, '", "filePath": "', fileName, '", "class": "', result$content5$label, '", "prob": "', round(result$content5$prob * 100), '"'))
    
    # Delete temp file.
    file.remove(fileName)
    
    content <- formatResult(result)
  }
  list(content=content, graph1=graph1)
}  

process <- function(path) {
 
 
  graph1 <- NULL
  
  freq <- list(minf = NULL, meanf = NULL, maxf = NULL)
  
  id <- gsub('.*temp(\\d+)\\.wav', '\\1', path)
  logEntry('Classifying.', paste0('"id": "', id, '", "filePath": "', path, '"'))
  
  tryCatch({
    incProgress(0.3, message = 'Processing voice ..')
    content1 <- gender(path, 1)
    incProgress(0.4, message = 'Analyzing voice 1/4 ..')
    content2 <- gender(path, 2, content1)
    incProgress(0.5, message = 'Analyzing voice 2/4 ..')
    content3 <- gender(path, 3, content1)
    incProgress(0.6, message = 'Analyzing voice 3/4 ..')
    content4 <- gender(path, 4, content1)
    incProgress(0.7, message = 'Analyzing voice 4/4 ..')
    content5 <- gender(path, 5, content1)
    
    incProgress(0.8, message = 'Building graph ..')
    
    wl <- 2048
    ylim <- 280
    thresh <- 5
    
    # Calculate fundamental frequencies.
    freqs <- fund(content1$wave, fmax=ylim, ylim=c(0, ylim/1000), threshold=thresh, plot=F, wl=wl)
    freq$minf <- round(min(freqs[,2], na.rm = T)*1000, 0)
    freq$meanf <- round(mean(freqs[,2], na.rm = T)*1000, 0)
    freq$maxf <- round(max(freqs[,2], na.rm = T)*1000, 0)
    
    graph1 <- renderPlot({
      
      
      fund(content1$wave, fmax=ylim, ylim=c(0, ylim/1000), type='l', threshold=thresh, col='red', wl=wl)
      x <- freqs[,1]
      y <- freqs[,2] + 0.01
      labels <- freqs[,2]
      
      subx <- x[seq(1, length(x), 4)]
      suby <- y[seq(1, length(y), 4)]
      sublabels <- paste(round(labels[seq(1, length(labels), 4)] * 1000, 0), 'hz')
      text(subx, suby, labels = sublabels)
      
      legend(0.5, 0.05, legend=c(paste('Min frequency', freq$minf, 'hz'), paste('Average frequency', freq$meanf, 'hz'), paste('Max frequency', freq$maxf, 'hz')), text.col=c('black', 'darkgreen', 'black'), pch=c(19, 19, 19))

    })
    
  }, warning = function(e) {
    if (grepl('cannot open the connection', e) || grepl('cannot open compressed file', e)) {
      restart(e)
    }
  }, error = function(e) {
    if (grepl('cannot open the connection', e) || grepl('cannot open compressed file', e)) {
      restart(e)
    }
  })
  
  list(content1=content1, content2=content2, content3=content3, content4=content4, content5=content5, graph1=graph1, freq=freq)
}

colorize <- function(tag) {
  result <- tag
  
  result
}

formatResult <- function(result) {
  pitchColor <- '#aa00aa;'
  if (result$content5$label == 'male') {
    pitchColor <- '#0000ff'
  }
  html <- paste0('Overall Result: <span style="font-weight: bold;">', colorize(result$content5$label), '</span> <span class="average-pitch"><i class="fa fa-headphones" aria-hidden="true" title="Average Pitch" style="color: ', pitchColor, '"></i>', result$freq$meanf, ' hz</span><hr>')
  
  html
}

logEntry <- function(message, extra = NULL) {
  try(
    if (!is.null(message) && nchar(message) > 0) {
      body <- paste0('{"application": "Voice Gender", "message": "', message, '"')
      
      if (!is.null(extra)) {
        body <- paste0(body, ', ', extra)
      }
    
      body <- paste0(body, '}')
    
      getURL(paste0('http://logs-01.loggly.com/inputs/', token), postfields=body)
    }
  )
}

restart <- function(e) {
  system('touch ~/app-root/repo/R/restart.txt')
}