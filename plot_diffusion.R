# TODO:
# Посмотреть, как сделать это более плавным
# Подсвечивать области/характеристики распределений, которые сильнее
# всего меняются при изменении того или иного параметра
# Все, что относится к распределениям, должно быть написано в области с распределениями
# Определиться с цветами
# Подписать v, sv, st0, sz
# Сделать игру на основе графика (найти такие значения параметров, 
# при которых теоретическое распределение накладывается на эмпирическое)
# - сравнить распределения при высокой скорости дрейфа и узкой границей критерия
#   Чем они различаются?
# - Сравнить распределения с узкой границей критерия и со смещением стартовой точки
#   (когда одинаковое расстояние до одной альтернативы и разное до другой)
# - Сравнить распределения в высоким и низким уровнем разброса
#   скорости дрейфа, времени вспомогательных процессов и стартовой точки
# - Высокий и низкий разброс времени ответа, мало и много ощибок,
#   быстрые и медленные ошибки
# Добавить на картинку таблицу со значениями параметров и с описательными
# snapshot_pars - каждый раз пересчитывает заново, это плохо

plot_diffusion_default <- function(
    pars, 
    n = numeric(0),
    maxtime = 2.5,
    snap = NULL,
    data = NULL,
    annotation = TRUE,
    lang = 'rus'
) {
  # -----------------------------------------------------------------------------------------------
  # Load libraries and misc functions
  # -----------------------------------------------------------------------------------------------
  require(rtdists)
  `%blend%` <- function(x, y) {
    x <- col2rgb(x, alpha = nchar(x)==9)
    y <- col2rgb(y, alpha = nchar(y)==9)
    hex <- sprintf('%02X', as.hexmode((x + y) / 2))
    paste0(c('#', hex), collapse = '')
  }
  
  `%bleach%` <- function(x, y) {
    x <- col2rgb(x, alpha = nchar(x)==9)
    y <- col2rgb(y, alpha = nchar(y)==9)
    hex <- sprintf('%02X', as.hexmode(x - y %% 255))
    paste0(c('#', hex), collapse = '')
  }
  
  # -----------------------------------------------------------------------------------------------
  # Plot parameters
  # -----------------------------------------------------------------------------------------------
  # maximum a
  maxa <- 2 

  # limits for a diffusion plot  
  xlim <- c(0, maxtime)
  ylim <- c(-0.07, maxa + 0.07)
  
  # y-limit for distribution plots
  ylim_dist <- c(0, 3)

  # Plot margins, plot separator
  lmar <- 3.1
  rmar <- 3.1
  vsep <- 1.8
  
  # Colors for diffusion plot
  zcol <- '#006600FF'
  t0col <- '#660066FF'
  
  # -----------------------------------------------------------------------------------------------
  # Compute distributions
  # -----------------------------------------------------------------------------------------------
  # This is probably slow part
  
  x <- seq(xlim[1], xlim[2], length = 200)
  
  # Density
  yu <- do.call(
    'ddiffusion', 
    c(rt = list(x),
      response = 'upper',
      pars,
      precision = 2)
  )
  
  yl <- do.call(
    'ddiffusion', 
    c(rt = list(x),
      response = 'lower',
      pars,
      precision = 2)
  )
  
  # Quartiles
  qu <-do.call(
    'qdiffusion', 
    c(p = list(c(0.25, 0.5, 0.75)),
      response = 'upper',
      pars,
      scale_p = TRUE,
      precision = 2)
  )
  
  ql <-do.call(
    'qdiffusion', 
    c(p = list(c(0.25, 0.5, 0.75)),
      response = 'lower',
      pars,
      scale_p = TRUE,
      precision = 2)
  )
  
  # Density at median
  ymedu <- do.call(
    'ddiffusion', 
    c(rt = qu[2],
      response = 'upper',
      pars,
      precision = 2)
  )
  
  ymedl <- do.call(
    'ddiffusion', 
    c(rt = ql[2],
      response = 'lower',
      pars,
      precision = 2)
  )
  
  
  # Snapshot distribution
  if (length(snap) > 0) {
#    snap$pars <- replace(pars, names(snap$pars), snap$pars)
    
    # Density
    yu_snap <- snap$density$upper
    
    yl_snap <- snap$density$lower
    
    # Quartiles
    qu_snap <-snap$quartiles$upper
    
    ql_snap <-snap$quartiles$lower
    
    # Density at median
    ymedu_snap <- snap$ymed$upper
    
    ymedl_snap <- snap$ymed$lower
  }

  # -----------------------------------------------------------------------------------------------
  # Upper distribution
  # -----------------------------------------------------------------------------------------------
  par(
    mar = c(vsep, lmar, 2.1, rmar), 
    bty = 'n',
    mgp = c(3, 0.4, 0)
  )
  
  plot(
    0, 0, type = 'n',
    axes = FALSE,
    xlim = xlim,
    ylim = ylim_dist,
    ylab = '', xlab = ''
  )
  
  if (length(n) == 0 && length(data) > 0) {
    n <- nrow(data)
  }
  
  if (length(data) > 0 & sum(data$response == 'upper') > 0) {
    rts <- data$rt[data$response == 'upper']
    rts <- rts[rts <= maxtime]
    H <- hist(
      rts, 
      breaks = seq(
        xlim[1],
        xlim[2],
        0.05
      ),
      plot = FALSE
    )
    
    H$counts <- H$counts / (0.05 * n)
    plot(
      H, 
      freq = TRUE,
      add = TRUE,
      ylim = ylim_dist,
      col = rgb(0, 0, 0, 0.03)
    )
    
#    points(rts, rep(-0.05, length(rts)),
#           pch = '|')
  }
  lines(x, yu,
       lwd = 2, lty = 1)
  
  # Axes
  axis(
    1, pos = 0, 
    tcl = -0.3,
    line = 0
  )
  
  axis(
    2, pos = 0,
    tcl = 0,
    at = ylim_dist,
    labels = FALSE
  )
  
  if (length(n) > 0 && n > 0) {
    ticks <- seq(ylim_dist[1], ylim_dist[2], length.out = 5)[-1]
    labels <- floor(ticks * n * 0.05)
    ticks <- labels / (n * 0.05)
    
    axis(
      2, pos = 0,
      tcl = -0.2,
      las = 1,
      at = ticks,
      labels = labels
    )
  }
  
  # Median
  segments(
    qu[2], 0,
    qu[2], ymedu,
    lwd = 2, 
    lty = 1
  )
  
  axis(
    4,
    at = 0,
    labels = switch(
      lang, 
      rus = 't (сек.)',
      eng = 't (sec.)'
    ),
    pos = maxtime * 1.01,
    tick = FALSE,
    las = 1,
    padj = 0.5,
    cex.axis = 1.5
  )
  
  if (annotation) {
    title(
      ylab = switch(
        lang, 
        rus = 'Количество наблюдений',
        eng = 'Number of observations'
      ),
      line = 0.7,
      cex.lab = 1.5
    )

    # Fast and slow responses
    sru <- which(yu > 0 & x < qu[1])
    polygon(
      x = c(
        x[sru],
        rev(x[sru])
      ),
      y = c(
        yu[sru],
        rep(0, length(sru))
      ),
      lty = 0,
      col = '#FF0000FF' %bleach% '#000000DD')
    
    fru <- which(yu > 0 & x > qu[3])
    polygon(
      x = c(
        x[fru],
        rev(x[fru])
      ),
      y = c(
        yu[fru],
        rep(0, length(fru))
      ),
      lty = 0,
      col = '#0000FFFF' %bleach% '#000000DD'
    )
  }
  
  # Snapshot distribution
  if (length(snap) > 0) {
    lines(
      x, yu_snap,
      lwd = 2, lty = 3
    )
    # Median
    segments(
      qu_snap[2], 0,
      qu_snap[2], ymedu_snap,
      lwd = 2, 
      lty = 3
    )
  }
  
  # Legend
  if (annotation) {
    if (length(snap) > 0) {
      leg <- legend(
        maxtime, ylim_dist[2],
        legend = switch(
          lang,
          rus = c(
            'Кривая распределения, медиана',
            'Быстрые ответы',
            'Медленные ответы',
            'Снимок распределения, медиана'
          ),
          eng = c(
            'RT distribution, median',
            'Fast responses',
            'Slow responses',
            'Snapshot distribution, median'
          )
        ),
        lty = c(1, 0, 0, 3),
        lwd = 2,
        pch = c(NA, 15, 15, NA),
        col = c(
          rgb(0, 0, 0, 1),
          rgb(1, 0, 0, 0.2),
          rgb(0, 0, 1, 0.2),
          rgb(0, 0, 0, 1)
        ),
        pt.cex = 2,
        box.lty = 0,
        cex = 1.5,
        xjust = 1,
        yjust = 1
      )
    } else {
      leg <- legend(
        maxtime, ylim_dist[2],
        legend = switch(
          lang, 
          rus = c(
            'Кривая распределения, медиана',
            'Быстрые ответы',
            'Медленные ответы'
          ),
          eng = c(
            'RT distribution, median',
            'Fast responses',
            'Slow responses'
          )
        ),
        lty = c(1, 0, 0),
        lwd = 2,
        pch = c(NA, 15, 15),
        col = c(
          rgb(0, 0, 0, 1),
          rgb(1, 0, 0, 0.2),
          rgb(0, 0, 1, 0.2)
        ),
        pt.cex = 2,
        box.lty = 0,
        cex = 1.5,
        xjust = 1,
        yjust = 1
      )
    }
  }
  
  # -----------------------------------------------------------------------------------------------
  # Lower distribution
  # -----------------------------------------------------------------------------------------------
  par(mar = c(2.1, lmar, vsep, rmar))

  plot(
    0, 0, type = 'n',
    axes = FALSE,
    xlim = xlim,
    ylim = rev(ylim_dist),
    ylab = '', xlab = ''
  )
  
  if (length(data) > 0 & sum(data$response == 'lower') > 0) {
    rts <- data$rt[data$response == 'lower']
    rts <- rts[rts <= maxtime]
    H <- hist(
      rts, 
      breaks = seq(
        xlim[1],
        xlim[2],
        0.05
      ),
      plot = FALSE
    )
    
    if (length(n) == 0)
      n <- nrow(data)
    
    H$counts <- H$counts / (0.05 * n)
    
    plot(
      H, 
      add = TRUE,
      col = rgb(0, 0, 0, 0.03)
    )
    
#    points(rts, rep(-0.05, length(rts)),
#           pch = '|')
  }
  
  lines(
    x, yl,
    lwd = 2, lty = 1
  )
  
  # Axes
  axis(
    3, pos = 0, 
    tcl = -0.3
  )
  
  axis(
    2, pos = 0, 
    tcl = 0,
    at = rev(ylim_dist),
    labels = FALSE
  )
  
  if (length(n) > 0 && n > 0) {
    # ticks <- seq(ylim_dist[1], ylim_dist[2], length.out = 5)[-1]
    # labels <- floor(ticks * n * 0.05)
    # ticks <- labels / (n * 0.05)
    
    axis(
      2, pos = 0,
      tcl = -0.2,
      las = 1,
      at = ticks,
      labels = labels
    )
  }
  
  # Median
  segments(
    ql[2], 0,
    ql[2], ymedl,
    lwd = 2, 
    lty = 1
  )
  axis(
    4,
    at = 0,
    labels = switch(
      lang, 
      rus = 't (сек.)',
      eng = 't (sec.)'
    ),
    pos = maxtime * 1.01,
    tick = FALSE,
    las = 1,
    padj = 0.5,
    cex.axis = 1.5
  )
  
  if (annotation) {
    title(
      ylab = switch(
        lang, 
        rus = 'Количество наблюдений',
        eng = 'Amount of observations'
      ),
      line = 0.7,
      cex.lab = 1.5
    )
    
    # Fast and slow responses
    srl <- which(yl > 0 & x < ql[1])
    polygon(
      x = c(x[srl], rev(x[srl])),
      y = c(yl[srl], rep(0, length(srl))),
      lty = 0,
      col = '#FF0000FF' %bleach% '#000000DD'
    )
    
    frl <- which(yl > 0 & x > ql[3])
    polygon(
      x = c(x[frl], rev(x[frl])),
      y = c(yl[frl], rep(0, length(frl))),
      lty = 0,
      col = '#0000FFFF' %bleach% '#000000DD'
    )
  }
  
  # Snapshot distribution
  if (length(snap) > 0) {
    lines(
      x, yl_snap,
      lwd = 2, lty = 3
    )
    # Median
    segments(
      ql_snap[2], 0,
      ql_snap[2], ymedl_snap,
      lwd = 2, 
      lty = 3
    )
  }
  
  if (length(data) > 0) {
    nall <- nrow(data)
    nup <- sum(data$response == 'upper')
    nlo <- sum(data$response == 'lower')
    
    labs <- c(
      substitute(
        paste('n', '=', n), 
        list(n = nall)
      ),
      substitute(
        paste('n'['upper'], '=', n, '; ', perc, '%'),
        list(n = nup, perc = sprintf('%0.1f', 100 * nup / nall))
      ),
      substitute(
        paste('n'['lower'], '=', n, '; ', perc, '%'),
        list(n = nlo, perc = sprintf('%0.1f', 100 * nlo / nall))
      )
    )
    
    maxheight <- max(-sapply(labs, strheight, cex = 1.5))
    maxwidth <- max(sapply(labs, strwidth, cex = 1.5))
    
    text(
      maxtime - maxwidth, ylim_dist[2] - 3:1 * 1.1 * maxheight,
      labels = do.call(expression, labs),
      adj = c(0, 1),
      cex = 1.5
    )
  }
  
  # -----------------------------------------------------------------------------------------------
  # Diffusion region
  # -----------------------------------------------------------------------------------------------
  par(
    mar = c(0, lmar, 0, rmar),
    mgp = c(3, 1, 0)
  )
  plot(
    0, 0,
    type = 'n',
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    xlab = '',
    ylab = ''
  )
  
  axis(
    2,
    at = c(0, maxa),
    labels = FALSE,
    pos = 0,
    tcl = 0
  )
  
  lines(
    x = c(xlim[1], xlim[2]), 
    y = c(0, 0),
    lwd = 2,
    type = 'l'
  )
  
  axis(
    2,
    at = c(0, pars$z, pars$a),
    labels = c('0', 'z', 'a'),
    pos = 0,
    tick = FALSE,
    las = 1,
    padj = 0.5,
    cex.axis = 1.5
  )
  
  if (annotation) {
    title(
      ylab = switch(
        lang,
        rus = 'Значение аккумулятора',
        eng = 'Level of evidence'
      ), 
      line = 0.7,
      cex.lab = 1.5
    )
  }
  
  # -----------------------------------------------------------------------------------------------
  # Snap model
  # -----------------------------------------------------------------------------------------------
  lty <- 3
  
  if (length(snap) > 0) {
    # a
    if (snap$pars$a != pars$a) {
      lines(
        x = c(xlim[1], xlim[2]), 
        y = c(snap$pars$a, snap$pars$a),
        lwd = 2,
        type = 'l',
        lty = lty
      )
    }
    
    # z
    if (snap$pars$z != pars$z) {
      lines(
        x = c(0, snap$pars$t0 + 0.5 * snap$pars$st0), 
        y = c(snap$pars$z, snap$pars$z),
        lwd = 3,
        type = 'b',
        col = zcol,
        lty = lty
      )
    }
    
    # sz
    if ((snap$pars$sz != 0) && 
        ((snap$pars$sz != pars$sz) | (snap$pars$z != pars$z))) {
      
      segments(
        0, snap$pars$z + 0.5 * snap$pars$sz,
        snap$pars$t0 + snap$pars$st0, 
        snap$pars$z + 0.5 * snap$pars$sz,
        lwd = 1.5,
        col = zcol,
        lty = lty
      )
      
      segments(
        0, snap$pars$z - 0.5 * snap$pars$sz,
        snap$pars$t0 + snap$pars$st0, 
        snap$pars$z - 0.5 * snap$pars$sz,
        lwd = 1.5,
        col = zcol,
        lty = lty
      )
    }

    # t0
    if (snap$pars$t0 != pars$t0) {
      segments(
        snap$pars$t0, 0,
        snap$pars$t0, maxa,
        lwd = 3,
        lty = lty,
        col = t0col
      )
    }
    
    # st0
    if ((snap$pars$st0 != pars$st0) && (snap$pars$st0 != 0)) {
      segments(
        snap$pars$t0 + snap$pars$st0, 0,
        snap$pars$t0 + snap$pars$st0, maxa,
        lwd = 1.5,
        col = t0col,
        lty = lty
      )
    }
    
    # v
    if ((snap$pars$v != pars$v) |
        (snap$pars$t0 != pars$t0) |
        (snap$pars$z != pars$z)) {
      arrows(
        x0 = snap$pars$t0 + 0.5 * snap$pars$st0,
        y0 = snap$pars$z,
        x1 = snap$pars$t0 + 0.5 * snap$pars$st0 + 0.5,
        y1 = snap$pars$z + snap$pars$v * 0.5,
        lwd = 3, lty = lty,
        length = 0.15, angle = 20
      )
    }
    
    # sv
    if ((snap$pars$sv != 0) && 
        ((snap$pars$sv != pars$sv) | 
         (snap$pars$v != pars$v) |
         (snap$pars$t0 != pars$t0) |
         (snap$pars$z != pars$z))) {
      arrows(
        x0 = snap$pars$t0 + 0.5 * snap$pars$st0,
        y0 = snap$pars$z,
        x1 = snap$pars$t0 + 0.5 * snap$pars$st0 + 0.5,
        y1 = snap$pars$z + 0.5 * (snap$pars$v + snap$pars$sv * c(-2, -1, 1, 2)),
        lwd = 3, lty = lty,
        col = c(
          rgb(0, 0, 0, 0.2),
          rgb(0, 0, 0, 0.5),
          rgb(0, 0, 0, 0.5),
          rgb(0, 0, 0, 0.2)
        ),
        length = 0.15, angle = 20
      )
    }
  }
  
  # -----------------------------------------------------------------------------------------------
  # Main model
  # -----------------------------------------------------------------------------------------------
  # a
  # -----------------------------------------------------------------------------------------------
  lines(
    x = c(xlim[1], xlim[2]), 
    y = c(pars$a, pars$a),
    lwd = 2,
    type = 'l'
  )

  text(
    maxtime, c(-0.1, pars$a + 0.1),
    switch(
      lang,
      rus = c(
        'Вторая альтернатива/неправильный ответ',
        'Первая альтернатива/правильный ответ'
      ),
      eng = c(
        'Second alternative/incorrect response',
        'First alternative/correct response'
      )
    ),
    pos = 2,
    cex = 1.5
  )
  
  
  # -----------------------------------------------------------------------------------------------
  # sz
  # -----------------------------------------------------------------------------------------------
  if (pars$sz > 0) {
    polygon(
      x = c(
        0, pars$t0 + pars$st0, 
        pars$t0 + pars$st0, 0
      ),
      y = c(
        pars$z - 0.5 * pars$sz, pars$z - 0.5 * pars$sz, 
        pars$z + 0.5 * pars$sz, pars$z + 0.5 * pars$sz
      ),
      lty = 0,
      col = zcol %bleach% '#000000DD'
    )
    segments(
      0, pars$z + 0.5 * pars$sz,
      pars$t0 + pars$st0, pars$z + 0.5 * pars$sz,
      lwd = 1.5,
      col = zcol
    )
    segments(
      0, pars$z - 0.5 * pars$sz,
      pars$t0 + pars$st0, pars$z - 0.5 * pars$sz,
      lwd = 1.5,
      col = zcol
    )
    
    if (annotation) {
      arrows(
        x0 = c(0.1, 0.1),
        x1 = c(0.1, 0.1),
        y0 = c(pars$z - 0.5 * pars$sz - 0.15, pars$z + 0.5 * pars$sz + 0.15),
        y1 = c(pars$z - 0.5 * pars$sz, pars$z + 0.5 * pars$sz),
        length = 0.07,
        lwd = 1.2,
        angle = 20
      )
      text(
        x = 0.1,
        y = pars$z + 0.5 * pars$sz + 0.21,
        labels = 'sz',
        las = 1,
        adj = c(0.5, 0),
        cex = 1.5
      )
    }
  }
  
  # -----------------------------------------------------------------------------------------------
  # z
  # -----------------------------------------------------------------------------------------------
  lines(
    x = c(0, pars$t0 + 0.5 * pars$st0), 
    y = c(pars$z, pars$z),
    lwd = 3,
    type = 'b',
    col = zcol
  )
  
  # -----------------------------------------------------------------------------------------------
  # st0
  # -----------------------------------------------------------------------------------------------
  if (pars$st0 > 0) {
    polygon(
      x = c(
        pars$t0, pars$t0, 
        pars$t0 + pars$st0, pars$t0 + pars$st0
      ),
      y = c(0, maxa, maxa, 0),
      lty = 0,
      col = t0col %bleach% '#000000DD'
    )
    segments(
      pars$t0 + pars$st0, 0,
      pars$t0 + pars$st0, maxa,
      lwd = 1.5,
      col = t0col
    )
    
    arrows(
      x0 = c(pars$t0 - 0.06, pars$t0 + pars$st0 + 0.06),
      x1 = c(pars$t0, pars$t0 + pars$st0),
      y0 = c(0.1, 0.1),
      y1 = c(0.1, 0.1),
      length = 0.07,
      lwd = 1.2,
      angle = 20
    )
    text(
      x = pars$t0 + pars$st0 + 0.11,
      y = 0.1,
      labels = 'st0',
      las = 1,
      adj = c(0.5, 0.5),
      cex = 1.5
    )
  }
  
  # -----------------------------------------------------------------------------------------------
  # t0
  # -----------------------------------------------------------------------------------------------
  segments(
    pars$t0, 0,
    pars$t0, maxa,
    lwd = 3,
    col = t0col
  )
  axis(
    1,
    at = pars$t0,
    labels = 't0',
    tick = FALSE,
    pos = 0,
    padj = 0,
    las = 1,
    cex.axis = 1.5
  )
  
  # -----------------------------------------------------------------------------------------------
  # The area of starting points
  # -----------------------------------------------------------------------------------------------
  if (pars$st0 > 0 | pars$sz > 0) {
    par(lend = 0)
    polygon(
      x = c(
        pars$t0, pars$t0, 
        pars$t0 + pars$st0, pars$t0 + pars$st0
      ),
      y = c(
        pars$z - 0.5 * pars$sz, pars$z + 0.5 * pars$sz,
        pars$z + 0.5 * pars$sz, pars$z - 0.5 * pars$sz
      ),
      lty = 1,
      border = 'red',
      lwd = 3
    )
    
    if (annotation) {
      legend(
        'right',
        legend = switch(
          lang,
          rus = 'Область положений начальной точки',
          eng = 'Area of starting points'
        ),
        pch = 22,
        col = 'red',
        pt.bg = (zcol %bleach% '#000000DD') %blend% (t0col %bleach% '#000000DD'),
        lwd = 2,
        lty= 0,
        pt.cex = 3,
        box.lty = 0,
        cex = 1.5
      )
      
    }
  }
  
  # -----------------------------------------------------------------------------------------------
  # v
  # -----------------------------------------------------------------------------------------------
  segments(
    pars$t0 + 0.5 * pars$st0, pars$z,
    pars$t0 + 0.5 * pars$st0 + 0.55, pars$z,
    lwd = 1,
    col = 'grey',
    lty = 2
  )
  segments(
    pars$t0 + 0.5 * pars$st0 + 0.5, 
    pars$z + pars$v * 0.5,
    pars$t0 + 0.5 * pars$st0 + 0.55, 
    pars$z + pars$v * 0.5,
    lwd = 1,
    col = 'grey',
    lty = 2
  )
  if (abs(pars$v * 0.5) > 0.2) {
    arrows(
      pars$t0 + 0.5 * pars$st0 + 0.525,
      y0 = pars$z + pars$v * 0.5,
      y1 = pars$z,
      length = 0.07,
      lwd = 1.2,
      angle = 20,
      code = 3
    )
    text(
      x = pars$t0 + 0.5 * pars$st0 + 0.55,
      y = pars$z + pars$v * 0.5 * 0.5,
      labels = 'v',
      las = 1,
      adj = c(0, 0.5),
      cex = 1.5
    )
  } else {
    arrows(
      pars$t0 + 0.5 * pars$st0 + 0.525,
      y0 = c(pars$z + pars$v * 0.5, pars$z),
      y1 = c(pars$z + pars$v * 0.5, pars$z) + 
        {if (pars$v > 0) c(0.1, -0.1) else c(-0.1, 0.1)},
      length = 0.07,
      lwd = 1.2,
      angle = 20,
      code = 1
    )
    text(
      x = pars$t0 + 0.5 * pars$st0 + 0.525,
      y = pars$z + pars$v * 0.5 + 
        ifelse(pars$v > 0, 0.2, -0.2), 
      labels = 'v',
      las = 1,
      adj = c(0.5, 0.5),
      cex = 1.5
    )
  }
  
  arrows(
    x0 = pars$t0 + 0.5 * pars$st0,
    y0 = pars$z,
    x1 = pars$t0 + 0.5 * pars$st0 + 0.5,
    y1 = pars$z + pars$v * 0.5,
    lwd = 3,
    length = 0.15, angle = 20
  )
  
  # -----------------------------------------------------------------------------------------------
  # sv
  # -----------------------------------------------------------------------------------------------
  if (pars$sv > 0) {
    arrows(
      x0 = pars$t0 + 0.5 * pars$st0,
      y0 = pars$z,
      x1 = pars$t0 + 0.5 * pars$st0 + 0.5,
      y1 = pars$z + 0.5 * (pars$v + pars$sv * c(-2, -1, 1, 2)),
      lwd = 3,
      col = c(
        rgb(0, 0, 0, 0.2),
        rgb(0, 0, 0, 0.5),
        rgb(0, 0, 0, 0.5),
        rgb(0, 0, 0, 0.2)
      ),
      length = 0.15, angle = 20
    )
  }
  
  # -----------------------------------------------------------------------------------------------
  # Return distributions
  # -----------------------------------------------------------------------------------------------
  return(list(
    pars = pars,
    x = x,
    density = list(
      upper = yu,
      lower = yl),
    quartiles = list(
      upper = qu,
      lower = ql),
    ymed = list(
      upper = ymedu,
      lower = ymedl),
    perc = list(
      upper = do.call(
        'pdiffusion', 
        c(
          rt = 100,
          response = 'upper',
          pars,
          precision = 2
        )
      ),
      lower = do.call(
        'pdiffusion', 
        c(
          rt = 100,
          response = 'lower',
          pars,
          precision = 2
        )
      )
    ),
    data = data
  ))
}

plot_diffusion <- function(
    pars, 
    n = numeric(0),
    maxtime = 2.5,
    snap = NULL,
    data = NULL,
    annotation = TRUE,
    lang = 'rus'
) {
  layout(
    matrix(c(1, 3, 2), ncol = 1), 
    widths = 1, heights = c(1, 1, 1),
    respect = FALSE
  )
  
  plot_diffusion_default(
    pars = pars,
    n = n,
    maxtime = maxtime,
    snap = snap,
    data = data,
    annotation = annotation,
    lang = lang
  )
}

run_diffusion <- function(
    pars,
    dt = 0.001,      # time step (sec)
    maxtime = 100
) { # time limit for a run (sec)      
  list2env(pars, envir = environment())
  
  t <- runif(1, t0, t0 + st0)
  acc <- runif(1, z - 0.5 * sz, z + 0.5 * sz)
  vt <- rnorm(1, v, sv) # actual drift rate in that trial
  history <- data.frame(t = t, acc = acc)
  
  while (acc > 0 & acc < a) { # Run diffusion process (as described by Wagenmakers)
    t <- t + dt
    if (t > maxtime) {
      break
    }
    acc <- acc + vt * dt + s * rnorm(1, 0, s * sqrt(dt))
    history <- rbind(history, c(t, acc))
  }
  
  if (acc >= a)
    resp <- 'upper'
  else if (acc <= 0)
    resp <- 'lower'
  else
    resp <- 'nonresponse'
  
  list(
    pars = pars,
    history = history,
    rt = round(t, 3),
    resp = resp
  )
}

# Lerche et al., 2017
ml_fit <- function(pars, data) {
  require(rtdists)
  
  rt_up <- data$rt[data$response == 'upper']
  if (length(rt_up) == 0) rt_up <- NA
  
  rt_lo <- data$rt[data$response == 'lower']
  if (length(rt_lo) == 0) rt_lo <- NA
  
  -(sum(log(
    do.call(
      'ddiffusion', 
      c(
        list(
          rt = rt_up,
          response = 'upper'
        ),
        pars
      )
    )
  )) + 
    sum(log(
      do.call(
        'ddiffusion', 
        c(
          list(
            rt = rt_lo,
            response = 'lower'
          ),
          pars
        )
      )
    )))
}

# plot_diffusion(game_pars[[3]],
#                data = game_bank[[3]],
#                n = 1000)

# Here I'm making three sets of parameters to simulate RT distributions
game_pars <- list(
  list(
    a = 2,
    v = 2,
    t0 = 0.2,
    z = 1,
    d = 0,
    sz = 0,
    sv = 0,
    st0 = 0,
    s = 1),
  list(
    a = 1.5,
    v = 2,
    t0 = 0.5,
    z = 1,
    d = 0,
    sz = 0,
    sv = 1,
    st0 = 0,
    s = 1),
  list(
    a = 2,
    v = -1,
    t0 = 0.4,
    z = 1,
    d = 0,
    sz = 0.3,
    sv = 1,
    st0 = 0.2,
    s = 1)
)

game_bank <- lapply(
  game_pars, 
  function(pars) {
    do.call('rdiffusion', c(pars, n = 1000))
  }
)

game_fits <- Map(ml_fit, game_pars, game_bank)
