#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# TODO:
# Останавливать сессию, когда пользователь переключается на другую вкладку:
# https://github.com/daattali/advanced-shiny/tree/master/close-window

library(shiny)
library(shinyBS)
library(shinyjs) # To enable|disable sliders

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

myCollapsePanel <- function (title, ..., value = title, style = NULL, icon_id = character(0)) {
  require(shinyBS)
  content <- list(...)
  id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 
                                                                 1, 1e+06))))
  if (is.null(value)) {
    value = title
  }
  if (is.null(style)) {
    style = "default"
  }
  bsTag <- shiny::tags$div(class = paste0("panel panel-", style), 
                           value = value, 
                           shiny::tags$div(class = "panel-heading",
                                           role = "tab", 
                                           id = paste0("heading_", id), 
                                           shiny::tags$h4(class = "panel-title", 
                                                          shiny::tags$a(class = 'accordion-toggle',
                                                                        `data-toggle` = "collapse", 
                                                                        href = paste0("#", id), 
                                                                        title),
                                           {if (length(icon_id) > 0) tags$i(class = 'far fa-question-circle',
                                                                            id = icon_id)})), 
                           shiny::tags$div(id = id, 
                                           class = "panel-collapse collapse", 
                                           role = "tabpanel", 
                                           shiny::tags$div(class = "panel-body", content)))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

tips <- FALSE

hints <- list(
  apanel = list(title = 'Decision boundary (a)',
                #                placement = 'bottom',
                content = paste('Decision boundary (a) specifies how much evidence has to be collected for decision to be made.',
                                'The higher decision boundary is, the more evidence has to be accumulated to complete the process',
                                'and the higher response latency.',
                                'On the contrary, when decision boundary is low, the responses are fast and inaccurate.')),
  vpanel = list(title = 'Drift rate (v)', 
                content = paste('Drift rate (v) measures the quality of the evidence and the rate of its accumulation.', 
                                'Higher drift rate indexes more efficient stimulus processing manifesting in faster and more accurate respinses.',
                                'Drift rate can vary across tasks depending on task difficulty and across individuals - depending on individual differences.',
                                'Extended DDM also allows drift rate to vary across the trials within the same task.',
                                'In this case actual drift rate in a trial is sampled from normal distribution with mean v and standard deviation sv.')),
  t0panel = list(title = 'Nondecision time (t0)', 
                 content = paste('Nondecision time (t0) is the time spent on the processes unrelated to evidence accumulation,',
                                 'such as stimulus encoding and response execution.',
                                 'Nondecision time can vary across trials. Standard practice is to consider nondecision time',
                                 'being sampled from a uniform distribution between t0 and t0 + st0.')),
  zpanel = list(title = 'Starting point (z)', 
                content = paste('Strting point (z) is initial value of accumulator between decision boundaries.', 
                                'The shift of starting point towards one of alternatives reflects decision bias.',
                                'In extended DDM, this parameter is sampled from a uniform distribution with mean z and range sz.')),
  snappanel = list(title = 'Snapshot distribution',
                   placement = 'bottom',
                   content = paste('Press "Make snapshot" to record parameters of current model.',
                                   'Move parameters to explore how response time/accuracy distributions change.',
                                   'The snapshot will appear on the plot along with current model.',
                                   '"Reset snapshot" will turn off snapshot parameters and distributions.')),
  simulate_hint = list(title = 'Simulate responses',
                       placement = 'bottom',
                       content = paste('Simulate diffusion-based decision process with chosen characteristics.',
                                       'The actual diffusion paths - the change of the evidence until criterion reached - will appear on the middle panel.',
                                       'It takes a while to complete the simulations, especially for more trials. "Reset data" interrupts simulation and erases the data.',
                                       'You can download the data after simulations are complete.')),
  load = list(title = 'Load RT data',
                  placement = 'bottom',
                  content = paste('Load your trial-by-trial response data to explore whether it can be explained by DDM.',
                                  'The probability of the data under current DDM parameters (-log-likelihood) appears in right bottom corner.',
                                  'The table must be csv file with columns labeled "rt" (response time, numeric) and "response"',
                                  '(character, either "upper" or "lower").',
                                  'The table is loaded using read.csv command without any additional arguments.',
                                  'The app does not store the uploaded data.')),
  fit_hint = list(title = 'Model fit',
                  placement = 'left',
                  content = paste('-LL (minus log-likelihood) shows how well current DDM parameters explain the data.',
                                  'Move the sliders to find the best fiting set of parameters.',
                                  'Best LL is the highest fit (or lowest discrepancy) achieved so far.',
                                  'When the button is red, push it to get back to the best parameters.'))
)

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    
    tags$head(
      tags$style(HTML("
.panel-heading .accordion-toggle:after {
    /* symbol for 'opening' panels */
    font-family: 'Glyphicons Halflings';  /* essential for enabling glyphicon */
    content: '\\e114';    /* adjust as needed, taken from bootstrap.css */
    float: right;        /* adjust as needed */
    color: grey;         /* adjust as needed */
}
.panel-heading .accordion-toggle.collapsed:after {
    /* symbol for 'collapsed' panels */
    content: '\\e080';    /* adjust as needed, taken from bootstrap.css */
}
                  "))
    ),
    
    br(),

    # bsModal('instruction',
    #         'Как играть в игру',
    #         'start_game',
    #         size = 'large',
    #         'Найдите параметры, при которых модель наилучшим образом объясняет реальные данные'),
    
    # ---------------------------------------------------------------------------------------------
    # Column 1: Hint, parameters and plot
    # Column 2: Game and parameters
    # ---------------------------------------------------------------------------------------------
    fluidRow(
      # Column 1: Hint, parameters and plot
      column(10, 
             # ------------------------------------------------------------------------------------
             # Column 1.1: Parameters
             # Column 1.2: Plot
             # ------------------------------------------------------------------------------------
             fluidRow(
               # ----------------------------------------------------------------------------------
               # Column 1.1: Parameters
               # ------------------------------------------------------------------------------------
               column(5,
                      bsCollapse(id = 'simPanel',
                                 myCollapsePanel('Simulate RT data', icon_id = 'simulate_hint',
                                                 wellPanel(id = 'simpanel',
                                                           fluidRow(
                                                             column(6, actionButton('simulate', 'Simulate data', width = '100%')),
                                                             column(3, h4('N trials:', style = 'text-align: left;')),
                                                             column(3, selectInput('n', label = NULL,
                                                                                   choices = c(30, 80, 130, 250, 360),
                                                                                   selected = 130,
                                                                                   width = '100%'))
                                                           ),
                                                           fluidRow(
                                                             column(6, actionButton('resetsim', 'Reset data', width = '100%')),
                                                             column(6, downloadButton('downloadsim', 'Download data', style = 'width: 100%'))
                                                           ))),
                                 myCollapsePanel('Load RT data', icon_id = 'load',
                                                 wellPanel(id = 'loadpanel',
                                                           fluidRow(
                                                             column(6, fileInput('user_data', label = NULL)),
                                                             column(6, actionButton('resetuserdata', 'Reset data', width = '100%'))
                                                           )
                                                 ))),
                      wellPanel(#id = 'snappanel',
                                fluidRow(
                                  column(6, actionButton('snap', 
                                                         list('Make snapshot',
                                                              tags$i(class = 'far fa-question-circle',
                                                                     id = 'snappanel')), 
                                                         width = '100%')),
                                  column(6, actionButton('reset', 'Reset snapshot', width = '100%'))
                                )
                      ),
                      wellPanel(id = 'collapsebuttons',
                                fluidRow(
                                  column(6, actionButton('collapse', 
                                                         list('Collapse all ', icon('angle-double-up')), 
                                                         width = '100%')),
                                  column(6, actionButton('expand', list(icon('angle-double-down'), ' Expand all'), 
                                                         width = '100%'))
                                )
                      ),
                      bsCollapse(id = 'parametersPanel', multiple = TRUE, 
                                 open = c('a - decision boundary',
                                          'v - drift rate'),
                                 myCollapsePanel('a - decision boundary', icon_id = 'apanel',
                                                 wellPanel(fluidRow(
                                                             column(1, strong('a')),
                                                             column(11, sliderInput("a",
                                                                                    NULL,
                                                                                    min = 0.5,
                                                                                    max = 2, 
                                                                                    value = 1.5,
                                                                                    step = 0.01))))
                                 ),
                                 myCollapsePanel('v - drift rate', icon_id = 'vpanel',
                                                 wellPanel(fluidRow(
                                                             column(1, strong('v')),
                                                             column(11, sliderInput("v",
                                                                                    NULL,
                                                                                    min = -5,
                                                                                    max = 5,
                                                                                    value = 1,
                                                                                    step = 0.1))),
                                                           fluidRow(
                                                             column(1, 
                                                                    strong('sv'),
                                                                    checkboxInput("svslide", "", value = FALSE)),
                                                             column(11, sliderInput("sv",
                                                                                    NULL,
                                                                                    min = 0,
                                                                                    max = 2,
                                                                                    value = 0,
                                                                                    step = 0.1))))
                                                 ),
                                 myCollapsePanel('t0 - nondecision time', icon_id = 't0panel',
                                                 wellPanel(fluidRow(
                                                             column(1, strong('t0')),
                                                             column(11, sliderInput("t0",
                                                                                    NULL,
                                                                                    min = 0.1,
                                                                                    max = 0.5,
                                                                                    value = 0.3,
                                                                                    step = 0.001))),
                                                           fluidRow(
                                                             column(1, 
                                                                    strong('st0'),
                                                                    checkboxInput("st0slide", "", value = FALSE)),
                                                             column(11, sliderInput("st0",
                                                                                    NULL,
                                                                                    min = 0,
                                                                                    max = 0.2,
                                                                                    value = 0,
                                                                                    step = 0.001))))
                                                 ),
                                 myCollapsePanel('z - starting point', icon_id = 'zpanel',
                                                 wellPanel(fluidRow(
                                                             column(1, strong('z')),
                                                             column(11, sliderInput("z",
                                                                                    NULL,
                                                                                    min = 0,
                                                                                    max = 2,
                                                                                    value = 0.75,
                                                                                    step = 0.01))),
                                                           fluidRow(
                                                             column(1, 
                                                                    strong('sz'),
                                                                    checkboxInput("szslide", "", value = FALSE)),
                                                             column(11, sliderInput("sz",
                                                                                    NULL,
                                                                                    min = 0,
                                                                                    max = 0.5,
                                                                                    value = 1,
                                                                                    step = 0.01))),
                                                           fluidRow(
                                                             column(8,
                                                                    checkboxInput('zrelcheck',
                                                                                  'Keep z/a constant',
                                                                                  value = TRUE)),
                                                             column(4,
                                                                    numericInput('zrel', NULL,
                                                                                 value = 0.5,
                                                                                 min = 0, max = 1,
                                                                                 step = 0.01,
                                                                                 width = '80px'),
                                                                    align = 'right')))
                                                 )
                      ),
                      # wellPanel(
                      #   fluidRow(
                      #     column(1, strong('d')),
                      #     column(11, sliderInput("d",
                      #                            "d",
                      #                            min = -0.1,
                      #                            max = 0.1,
                      #                            value = 0,
                      #                            step = 0.02)))),
                      tags$head(
                        tags$style(type = "text/css", ".well {margin-bottom: 5px; padding: 12px}"),
                        tags$style(type = "text/css", ".form-group {margin-bottom: 0px; }"),
                        tags$style(type = "text/css", ".panel-body {padding: 10px}"),
                        tags$style(type = "text/css", '.progress {margin-bottom: 0px;}'))
               ),
               lapply(names(hints), function(x) {
                 bsPopover(x,
                           title = hints[[x]]$title,
                           content = hints[[x]]$content,
                           trigger = 'hover',
                           placement = hints[[x]]$placement)
               }),
               
               # ----------------------------------------------------------------------------------
               # Column 1.2: Plot
               # ----------------------------------------------------------------------------------
               column(7, 
                      plotOutput("diff_plot")))),
      # -------------------------------------------------------------------------------------------
      # Column 2: Game and parameters
      column(2,  
             # actionButton('start_game', tags$p('Начать игру'),
             #              style = 'width:100%; height:50px;
             #                   background-color:#4CAF50; color:white;'),
             # actionButton('end_game', tags$p('Закончить игру'),
             #              style = 'width:100%; height:50px;
             #               background-color:#AF4E4C; color:white;'),
             uiOutput('params'),
             wellPanel(
               textOutput('fit', inline = TRUE), 
               tags$i(class = 'far fa-question-circle',
                      id = 'fit_hint',
                      style = 'container: "body"'),
               textOutput('best_fit'),
               uiOutput('return_best_pars_button')
             ),
             bsCollapse(id = 'infoPanel',
                        myCollapsePanel('References', 
                                        a(id = 'RatcliffEtAl2016',
                                          'Ratcliff et al. (2016)',
                                          href = 'https://www.sciencedirect.com/science/article/pii/S1364661316000255',
                                          target = '_blank'),
                                        bsPopover('RatcliffEtAl2016',
                                                  'Ratcliff, R., Smith, P. L., Brown, S. D., & McKoon, G. (2016). Diffusion decision model: Current issues and history. Trends in cognitive sciences, 20(4), 260-281.',
                                                  placement = 'top'),
                                        br(),
                                        a(id = 'Wagenmakers2009',
                                          'Wagenmakers (2009)',
                                          href = 'https://www.tandfonline.com/doi/abs/10.1080/09541440802205067',
                                          target = '_blank'),
                                        bsPopover('Wagenmakers2009',
                                                  'Wagenmakers, E. J. (2009). Methodological and empirical developments for the Ratcliff diffusion model of response times and accuracy. European Journal of Cognitive Psychology, 21(5), 641-671.',
                                                  placement = 'top'),
                                        br(),
                                        a(id = 'VossEtAl2004', 
                                          'Voss et al. (2004)',
                                          href = 'https://link.springer.com/article/10.3758/BF03196893',
                                          target = '_blank'),
                                        bsPopover('VossEtAl2004',
                                                  'Voss, A., Rothermund, K., & Voss, J. (2004). Interpreting the parameters of the diffusion model: An empirical validation. Memory & cognition, 32(7), 1206-1220.',
                                                  placement = 'top')),
                        myCollapsePanel('Contact info',
                                        p(strong('Ivan Voronin')),
                                        p('Université Laval,',
                                          'Quebec, Canada'),
                                        a('ivan.a.voronin@gmail.com',
                                          href = 'mailto:ivan.a.voronin@gmail.com',
                                          target = '_blank'))),
             actionButton('download_pic', 'Download picture',
                          style = 'width:100%; height:50px;'),
             align = 'center')
    )
  )
)
