library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  conditionalPanel(condition='!output.json',
                   tags$head(tags$script(src = "script.js")
                   			 
                   ),
                   titlePanel('What is Your Voice Gender?'),
                   div(style='margin: 30px 0 0 0;'),
                   mainPanel(width = '100%',
                             useShinyjs(),
                             h4(id='main', 'Upload a .WAV file of your voice or enter a url from ', a(href='http://vocaroo.com', target='_blank', 'vocaroo.com'),' to detect its gender.'),
                             div(style='margin: 20px 0 ,0 ,0;'),
                             
                             inputPanel(
                               div(id='uploadDiv', class='', style='height: 120px; border-right: 1px solid #ccc;',
                                   fileInput('file1', 'Choose WAV File', accept = c('audio/wav'), width = '100%')
                               ),
                               div(id='urlDiv', class='',
                                   strong('Url (vocaroo)'),
                                   textInput('url', NULL, width = '100%'),
                                   actionButton('btnUrl', 'Load Url', class='btn-primary')
                               ),
                               div('Please be patient after uploading or clicking submit.')
                             ),
                             
                             div(style='margin: 20px 0 0 0;'),
                             div(id='result', style='font-size: 22px;', htmlOutput('content')),
                             div(style='margin: 20px 0 0 0;'),
                             
                             conditionalPanel(condition='output.content != null && output.content.indexOf("Please enter") == -1',
                               tabsetPanel(id='graph',
                                 tabPanel('Frequency Graph', plotOutput("graph1", width=1000, height=500))
                                 
                               ),
                               div(style='margin: 20px 0 0 0;')
                             )
                   ))
))