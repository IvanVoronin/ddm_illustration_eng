#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Dependencies:
# - shiny
# - shinyjs
# - shinyBS
# - rtdists
# - xtable
# - knitr
# - kableExtra

library(shiny)
library(rtdists)
library(xtable)
library(knitr)
library(kableExtra)

source('plot_diffusion.R')

shinyServer(function(input, output, session) {
  zrel <- 0.5
  
  values <- reactiveValues(model = NULL, 
                           snap = NULL, 
                           data = NULL,
                           data_pars = NULL,
                           history = NULL,
                           i = 0,
                           last_run = NULL,
                           fit = NA,
                           best_fit = NA,
                           best_pars = NULL,
                           game_mode = FALSE,
                           game_i = NA)
  
  pars <- reactive(
    list(
      a = input$a,
      v = input$v,
      t0 = input$t0,
      z = if (input$zrelcheck) input$a * input$zrel else input$z,
      d = 0,
      sz = input$sz,
      sv = input$sv,
      st0 = input$st0,
      s = 1))
  # -----------------------------------------------------------------------------------------------
  # Show modal window at start
  # -----------------------------------------------------------------------------------------------
  welcome_text <- withTags(
    list(b('Diffusion decision model (DDM)'), 'describes the distribution of response speed and accuracy in',
         'two-alternative choice tasks. DDM assumes that the decision process is an accumulation of',
         'noisy evidence until its level is sufficient for making the choice.',
         br(), br(),
         "DDM's parameters feature four distinct characteristics of decision process:",
         ul(
           li(b('drift rate (v)'), 'is the rate of evidence accumulation,'),
           li(b('decision boundary (a)'), 'is the amount of evidence required for making decision,'),
           li(b('starting point (z)'), 'is the preference of a certain choice,'),
           li(b('nondecision time (t0)'), 'is the duration of processes unrelated to evidence accumulation.')),
         
         'The middle panel of the plot renders the parameters of DDM.',
         'The top and bottom panels depict the response time distributions for two choices.',
         'Move the sliders to explore how the parameter change affects the shape of response distributions.',
         'Make a snapshot to compare the response distributions before and after the parameter change.',
         br(), br(),
         'Also, you can simulate the response data using DDM or load your own data.', 
         'The panel on the bottom right side shows the log-likelihood of the data.',
         'Move the sliders to find out which set of parameters provides the most accurate description of the data.',
         br(), br(),
         'Have fun!', 
         br(), br(),
         p('Ivan Voronin', br(),
           a('ivan.a.voronin@gmail.com',
             href = 'mailto:ivan.a.voronin@gmail.com',
             target = '_blank'), br(),
           'Université Laval',
           style = 'text-align:right;'),
         'This work was supported by the Russian Foundation for Basic Research, grant 17-36-01135-OGN.',
         br(), br(),
         'P.S.: The app looks better on bigger screen.', br(),
         'P.P.S.: At the moment the app is run on a free account with limited total runtime. The app will stop after 7 min of inactivity.', br(),
         'P.P.P.S.: Also, the app is available for download ', 
         a('on GitHub.', href = 'https://github.com/IvanVoronin/ddm_illustration_eng'))
    )
  start_dialogue <- modalDialog(
    title = 'Welcome to DDM!',
    renderUI(welcome_text),
    footer = modalButton('Continue'),
    easyClose = TRUE,
    size = 'm'
  )
  showModal(start_dialogue)
  
  # -----------------------------------------------------------------------------------------------
  # Observe controls
  # -----------------------------------------------------------------------------------------------
  observe({
    if (input$z + 0.5 * input$sz >= input$a)
      updateSliderInput(session, 'sz',
                        value = 2 * (input$a - input$z))
    
    if (input$z - 0.5 * input$sz <= 0)
      updateSliderInput(session, 'sz',
                        value = 2 * input$z)
    
    if (input$z >= input$a)
      updateSliderInput(session, 'z',
                        value = input$a)
    
    if (input$szslide)
      shinyjs::enable('sz')
    else {
      shinyjs::disable('sz')
      updateSliderInput(session, 'sz',
                        value = 0)
    }
    
    if (input$svslide)
      shinyjs::enable('sv')
    else {
      shinyjs::disable('sv')
      updateSliderInput(session, 'sv',
                        value = 0)
    }
    
    if (input$st0slide)
      shinyjs::enable('st0')
    else {
      shinyjs::disable('st0')
      updateSliderInput(session, 'st0',
                        value = 0)
    }
    
    if (input$zrelcheck) {
      shinyjs::enable('zrel')
      shinyjs::disable('z')
      updateSliderInput(session, 'z',
                        value = input$zrel * input$a)
    } else {
      shinyjs::enable('z')
      shinyjs::disable('zrel')
      updateNumericInput(session, 'zrel',
                         value = input$z / input$a)
    }
    
    if (length(values$data) > 0) {
      values$fit <- ml_fit(pars(), values$data)
        if (is.na(values$best_fit) || values$fit < values$best_fit) {
          values$best_fit <- isolate(values$fit)
          values$best_pars <- pars()
        }
    } else {
      values$fit <- NA
      values$best_pars <- NULL
      values$best_fit <- NA
    }
    
    values$data_pars <- values$best_pars
#    values$data <- game_bank[[values$game_i]]
    
    # if (length(values$data) > 0) {
    #   values$fit <- ml_fit(pars(), values$data)
    # }
  })
  
  observeEvent(input$reset, {values$snap <- NULL})
  
  observeEvent(input$snap, {values$snap <- isolate(values$model)})
  
  observeEvent(input$collapse, {
    updateCollapse(session, 'parametersPanel', 
                   close = c('a - decision boundary',
                             'v - drift rate',
                             't0 - nondecision time',
                             'z - starting point'))
  })
  observeEvent(input$expand, {
    updateCollapse(session, 'parametersPanel', 
                   open = c('a - decision boundary',
                            'v - drift rate',
                            't0 - nondecision time',
                            'z - starting point'))
  })
  
  observeEvent(input$user_data, {
    values$data <- read.csv(input$user_data$datapath)
  })
  
  observeEvent(input$resetsim, {
    values$history <- list()
    values$data <- NULL
    values$last_run <- NULL
    values$i <- NA
  })
  observeEvent(input$n, {
    values$history <- list()
    values$data <- NULL
    values$i <- NA
  })
  
  
  observeEvent(input$resetuserdata, {
    values$data <- NULL
    reset('user_data')
  })
  
  observeEvent(input$return_best_pars, {
    if (length(values$best_pars) > 0) {
      updateSliderInput(session, 'a',
                        value = values$best_pars$a)
      updateSliderInput(session, 'v',
                        value = values$best_pars$v)
      updateSliderInput(session, 'sv',
                        value = values$best_pars$sv)
      updateSliderInput(session, 't0',
                        value = values$best_pars$t0)
      updateSliderInput(session, 'st0',
                        value = values$best_pars$st0)
      updateSliderInput(session, 'z',
                        value = values$best_pars$z)
      updateSliderInput(session, 'sz',
                        value = values$best_pars$sz) 
    }
  })
  
  observeEvent(input$simulate, {
    shinyjs::disable('a')
    shinyjs::disable('v')
    shinyjs::disable('sv')
    shinyjs::disable('t0')
    shinyjs::disable('st0')
    shinyjs::disable('z')
    shinyjs::disable('sz')
    shinyjs::disable('zrel')
    shinyjs::disable('simulate')
    shinyjs::disable('n')
    shinyjs::disable('snap')
    shinyjs::disable('reset')
    
    values$snap <- NULL
    values$i <- 0
    values$history <- list()
    values$data <- data.frame(rt = numeric(0),
                              response = character(0))
    values$data_pars <- isolate(pars())
    
    observe({
      i <- isolate(values$i)
      values$i <- i + 1
      if (!is.na(i) && i < as.numeric(isolate(input$n))) {
        values$last_run <- run_diffusion(isolate(pars()), maxtime = 10)
        values$data <- rbind(isolate(values$data),
                             data.frame(rt = values$last_run$rt,
                                        response = values$last_run$resp))
        values$history <- c(isolate(values$history),
                            list(values$last_run$history))
        values$best_fit <- NA
        
        invalidateLater(ifelse(isolate(input$n) < 200, 250, 150), session)
      }
      
      if (is.na(i) || (i + 1) == as.numeric(input$n)) {
        shinyjs::enable('a')
        shinyjs::enable('v')
        shinyjs::enable('sv')
        shinyjs::enable('t0')
        shinyjs::enable('st0')
        shinyjs::enable('z')
        shinyjs::enable('sz')
        shinyjs::enable('zrel')
        shinyjs::enable('simulate')
        shinyjs::enable('n')
        shinyjs::enable('snap')
        shinyjs::enable('reset')
      }
    })
  })
  
  # Stop the app when the window closed
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  # observeEvent(input$start_game, {
  #   if (!values$game_mode) {
  #     values$game_mode <- TRUE
  #     values$game_i <- 1
  #     values$data <- game_bank[[values$game_i]]
  #     values$fit <- ml_fit(pars(), values$data)
  #     values$best_fit <- isolate(values$fit)
  #     values$best_pars <- pars()
  #     values$snap <- NULL
  #     shinyjs::disable('snap')
  #     shinyjs::disable('reset')
  #     updateButton(session, 'start_game',
  #                  label = 'Finish the game',
  #                  style = 'warning')
  #   }
  # })
    
  # observeEvent(input$end_game, {
  #   values$game_mode <- FALSE
  #   values$game_i <- NA
  #   values$fit <- NA
  #   values$best_fit <- NA
  #   shinyjs::enable('snap')
  #   shinyjs::enable('reset')})

  # -----------------------------------------------------------------------------------------------
  # Write output
  # -----------------------------------------------------------------------------------------------
  # Hint
  output$hint <- renderUI({
    withTags(div(
      textarea(id = 'hint',
               class = 'shiny-text-output shiny-bound-output',
               style = 'min-width: 100%; min-height: 80px; resize: none; border: none',
               readonly = '',
               HTML(hints[[hovered_element()]]))
    ))
  })
  
  # Simulated data
  output$downloadsim <- downloadHandler(
    filename = function() paste0('rt_data_', nrow(values$data), '_trials.csv'),
    content = function(file) {
      write.csv(values$data, file)
    }
  )
  
  # The plot
  output$diff_plot <- renderPlot({
    (values$model <- plot_diffusion(
      pars = pars(),
      snap = values$snap,
      data = values$data,
      lang = 'eng',
      n = if (!is.na(values$i) && values$i > 0) 
        as.numeric(input$n) else numeric(0)))
    if (length(values$last_run) > 0) {
      history <- values$last_run$history
      history$acc[nrow(history)] <- switch(values$last_run$resp, 
                                           upper = pars()$a,
                                           lower = 0)
      lines(history, 
            col = rgb(0, 0, 0, 0.3))
      points(history[nrow(history), 't'],
             history[nrow(history), 'acc'],
             pch = 19)
    }
    # if (length(values$history) > 0) {
    #   i <- values$history[[length(values$history)]]
    #   i$acc[nrow(i)] <- ifelse(i$acc[nrow(i)] >= pars()$a, pars()$a, 0)
    #   lines(i, col = rgb(0, 0, 0, 0.3))
    #   points(i[nrow(i), 't'], 
    #          i[nrow(i), 'acc'],
    #          pch = 19)
    # }
  },
  height = 700,
  execOnResize = TRUE)
  
  # Download button
  observeEvent(input$download_pic, {
    showModal(modalDialog(
      title = 'Download picture',
      numericInput('width', value = 10, label = 'Picture width'),
      numericInput('height', value = 7, label = 'Picture height'),
      actionButton('download_now',
                   label = 'Download')
    ))
  })
  
  observeEvent(input$download_now, {
    pdf(file = 'pic.pdf',
        width = input$width,
        height = input$height)
    (values$model <- plot_diffusion(
      pars = pars(),
      snap = values$snap,
      data = values$data,
      lang = 'eng',
      n = if (!is.na(values$i) && values$i > 0) 
        as.numeric(input$n) else numeric(0)))
    if (length(values$last_run) > 0) {
      history <- values$last_run$history
      history$acc[nrow(history)] <- switch(values$last_run$resp, 
                                           upper = pars()$a,
                                           lower = 0)
      lines(history, 
            col = rgb(0, 0, 0, 0.3))
      points(history[nrow(history), 't'],
             history[nrow(history), 'acc'],
             pch = 19)
    }
    dev.off()
  })
  
  # Parameter table
  output$params <- renderUI({
    dat <- data.frame(Model = unlist(pars()))
    rownames(dat) <- names(pars())
    
    top <- data.frame(Model = c(values$model$quartiles$upper,
                               values$model$perc$upper))
    rownames(top) <- c('Q1', 'Q2', 'Q3', '%')
    bot <- data.frame(Model = c(values$model$quartiles$lower,
                               values$model$perc$lower))
    rownames(bot) <- c('Q1', 'Q2', 'Q3', '%')

    if (length(values$snap$pars > 0)) {
      dat$Snap <- unlist(values$snap$pars)
      top$Snap <- c(values$snap$quartiles$upper,
                       values$snap$perc$upper)
      bot$Snap <- c(values$snap$quartiles$lower,
                       values$snap$perc$lower)
    }

    if (length(values$data) > 0) {
      dat$Data <- if (length(values$data_pars) == 0) rep(NA, 9) else unlist(values$data_pars)
      top_data <- values$data$rt[values$data$response == 'upper']
      bot_data <- values$data$rt[values$data$response == 'lower']
      top$Data <- c(quantile(top_data, probs = c(0.25, 0.5, 0.75)), 
                    length(top_data) / nrow(values$data))
      bot$Data <- c(quantile(bot_data, probs = c(0.25, 0.5, 0.75)), 
                    length(bot_data) / nrow(values$data))
    }
    dat <- dat[c('a', 'v', 'sv', 't0', 'st0', 'z', 'sz'), , drop = FALSE]
    
    outpTable <- as.matrix(rbind(dat, top, bot))
    rownames(outpTable) <- c('a', 'v', 'sv', 't0', 'st0', 'z', 'sz',
                              'Q1', 'Q2', 'Q3', '%', 'Q1', 'Q2', 'Q3', '%')
        
    kable(outpTable,
          format = 'html',
          digits = 2) %>%
      kable_styling(bootstrap_options = c('hover', 'condensed'),
                    full_width = FALSE) %>%
      pack_rows('Parameters', 1, 7,
                label_row_css = 'border-top: 2px solid #ddd;') %>%
      pack_rows('Top distribution', 8, 11,
                label_row_css = 'border-top: 2px solid #ddd;') %>%
      pack_rows('Bottom distribution', 12, 15,
                label_row_css = 'border-top: 2px solid #ddd;') %>%
      HTML
  })
  
  output$fit <- renderText(sprintf('-LL = %0.3f', 
                                   values$fit))
  output$best_fit <- renderText(sprintf('Best -LL = %0.3f', 
                                        values$best_fit))

  output$return_best_pars_button <- renderUI({
    actionButton('return_best_pars', 
                 if (is.na(values$best_fit)) {
                   'No data loaded yet'
                 } else if (values$best_fit < values$fit) {
                   'Return best parameters'
                 } else {
                   'Best fit so far'
                 }, 
                 width = '100%',
                 style = paste('color: white;', 
                               if (is.na(values$best_fit)) {
                                 'background-color: grey;'
                               } else if (values$best_fit < values$fit) {
                                 'background-color: red;'
                               } else {
                                 'background-color:green;'
                               }))
  })
  
})
