#== Density plot function ============
plot_density2_stacked <- function(
  data,
  ax_min,
  ax_max,
  ax_tickpos,
  ax_lab = "",
  plt = c(0.15, 0.95, 0.15, 0.95),
  new = TRUE,
  switch_axes = FALSE
) {
  # Initialise
  par(plt=plt, new=new)
  if (!switch_axes) {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(ax_min,ax_max), ylim=c(0,1),
         xlab="", ylab="",
         xaxs="i",yaxs="i")
    box()
  } else {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(0,1), ylim=c(ax_min,ax_max),
         xlab="", ylab="",
         xaxs="i",yaxs="i")
    box()
  }
  
  
  if (!switch_axes) {
    
    nnet <- length(names(data))

    plt_ymin  <- plt[3]    
    plt_ymax  <- plt[4]
    plt_delta <- (plt_ymax-plt_ymin)/nnet
    
    for (k_net in names(data)) {
      
      kpos <- which(names(data) == k_net)
      
      cur_plt_ymin <- plt_ymax - kpos    *plt_delta
      cur_plt_ymax <- plt_ymax - (kpos-1)*plt_delta
# 
#       if (new == FALSE && kpos == 1) {
#         par(plt=c(plt[1], plt[2], cur_plt_ymin, cur_plt_ymax), new=FALSE)   
#       } else {
#         par(plt=c(plt[1], plt[2], cur_plt_ymin, cur_plt_ymax), new=TRUE)   
#       }
#       
      par(plt=c(plt[1], plt[2], cur_plt_ymin, cur_plt_ymax), new=TRUE)
      plot(0,0,
           type="n",
           axes=FALSE,
           xlim=c(ax_min,ax_max), ylim=c(0,1),
           xlab=ifelse(kpos == length(names(data)), ax_lab, ""), ylab="",
           xaxs="i",yaxs="i")
      
      # Grid
      for (kx in ax_tickpos) lines(c(kx, kx), c(0, 1), col="#eeeeee")
      
      # Density area
      polygon(
        c(data[[k_net]]$pot$x, rev(data[[k_net]]$pot$x)),
        c(rep(0, length(data[[k_net]]$pot$y)), data[[k_net]]$pot$y),
        col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]),
        border=NA
      )

      #box()
      lines(c(ax_min,ax_max), c(0,0), 
            col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]))
    }
    # Axes
    axis(1, at=ax_tickpos)
    
  } else {
    nnet <- length(names(data))
    
    plt_xmin  <- plt[1]    
    plt_xmax  <- plt[2]
    plt_delta <- (plt_xmax-plt_xmin)/nnet
    
    for (k_net in names(data)) {
      
      kpos <- which(names(data) == k_net)
      
      cur_plt_xmin <- plt_xmax - kpos    *plt_delta
      cur_plt_xmax <- plt_xmax - (kpos-1)*plt_delta
      # 
      #       if (new == FALSE && kpos == 1) {
      #         par(plt=c(plt[1], plt[2], cur_plt_ymin, cur_plt_ymax), new=FALSE)   
      #       } else {
      #         par(plt=c(plt[1], plt[2], cur_plt_ymin, cur_plt_ymax), new=TRUE)   
      #       }
      #       
      par(plt=c(cur_plt_xmin, cur_plt_xmax, plt[3], plt[4]), new=TRUE)

      plot(0,0,
           type="n",
           axes=FALSE,
           xlim=c(0,1), ylim=c(ax_min,ax_max),
           xlab="", ylab=ifelse(kpos == length(names(data)), ax_lab, ""),
           xaxs="i",yaxs="i")
      
      # Grid
      for (ky in ax_tickpos) lines(c(0, 1), c(ky, ky), col="#eeeeee")
      
      # Density area
      polygon(
        c(rep(0, length(data[[k_net]]$cost$x)), data[[k_net]]$cost$x),
        c(data[[k_net]]$cost$y, rev(data[[k_net]]$cost$y)),
        col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]),
        border=NA
      )
      
      #box()
      lines(c(0,0), c(ax_min,ax_max),
            col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]))
    }
    
    # Axes
    axis(2, at=ax_tickpos)
    
    box()
  }
  
  
}
#== Density plot function ============
plot_density2 <- function(
  data,
  ax_min,
  ax_max,
  ax_tickpos,
  ax_lab = "",
  plt = c(0.15, 0.95, 0.15, 0.95),
  new = TRUE,
  switch_axes = FALSE
) {
  # Initialise
  par(plt=plt, new=new)
  
  if (!switch_axes) {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(ax_min,ax_max), ylim=c(0,1),
         xlab=ax_lab, ylab="",
         xaxs="i",yaxs="i")
    
    # Grid
    for (kx in ax_tickpos) lines(c(kx, kx), c(0, 1), col="#eeeeee")
    
    # Density line 
    for (k_net in names(data)) {
      lines(data[[k_net]]$pot$x, data[[k_net]]$pot$y, col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]),lwd=0.5)
    }

    # Axes
    axis(1, at=ax_tickpos)
    
    box()
    
  } else {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(0,1), ylim=c(ax_min,ax_max),
         xlab="", ylab=ax_lab,
         xaxs="i",yaxs="i")
    
    # Grid
    for (ky in ax_tickpos) lines(c(0, 1), c(ky, ky), col="#eeeeee")
    
    # Density line
    for (k_net in names(data)) {
      lines(data[[k_net]]$cost$x, data[[k_net]]$cost$y, col=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]),lwd=0.5)
    }
    
    # Axes
    axis(2, at=ax_tickpos)
    
    box()
  }
  
  
}

#== Density plot function ============
plot_density <- function(
  data,
  range,
  threshold,
  ax_min,
  ax_max,
  ax_tickpos,
  ax_lab = "",
  plt = c(0.15, 0.95, 0.15, 0.95),
  new = TRUE,
  cex_axis=1,
  switch_axes = FALSE
) {
  # Initialise
  par(plt=plt, new=new)
  
  if (!switch_axes) {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(ax_min,ax_max), ylim=c(0,range$ymax*1.05),
         xlab=ax_lab, ylab="",
         xaxs="i",yaxs="i",
         cex.axis=cex_axis, cex=cex_axis)
    
    # Grid
    for (kx in ax_tickpos) lines(c(kx, kx), c(0, range$ymax*1.05), col="#eeeeee")
    
    # 50% of studies
    rect(range$x[1], 0,
         range$x[2], range$ymax*1.05,
         col="#cccccc66", border=NA)
    lines(c(ax_min,ax_max), c(range$y,range$y), col="#777777", lty=2)
    
    # Density line and threshold
    # rect(data$x[min(which(data$y> threshold))], 0,
    #      data$x[max(which(data$y> threshold))], 1.05,
    #      col="#cccccc66", border=NA)
    #lines(c(ax_min,ax_max), c(threshold,threshold), col="#777777", lty=2)
    
    lines(data$x, data$y, col="red",lwd=3)
    
    # rect(data$x[min(which(data$y> threshold))]-ax_max*0.025, 0.03, 
    #      data$x[min(which(data$y> threshold))]+ax_max*0.025, 0.16,
    #      col="#ffffff99", border="#99999999")
    # text(data$x[min(which(data$y> threshold))], 0.1, paste(round(data$x[min(which(data$y> threshold))], digits=0)), cex=0.8, col="#777777")
    # 
    # rect(data$x[max(which(data$y> threshold))]-ax_max*0.025, 0.03, 
    #      data$x[max(which(data$y> threshold))]+ax_max*0.025, 0.16,
    #      col="#ffffff99", border="#99999999")
    # text(data$x[max(which(data$y> threshold))], 0.1, paste(round(data$x[max(which(data$y> threshold))], digits=0)), cex=0.8, col="#777777")
    
    #text(ax_max - ax_max*0.025, threshold+0.05, paste0(threshold*100, "%"), adj = c(1,0), cex=0.8, col="#777777")
    
    #text(ax_max - ax_max*0.025, range$y+0.05, paste0(threshold*100, "%"), adj = c(1,0), cex=0.8, col="#777777")
    
    # Axes
    axis(1, at=ax_tickpos, cex.axis=cex_axis, cex=cex_axis)
    
    box()
    
  } else {
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(0,range$ymax*1.05), ylim=c(ax_min,ax_max),
         xlab="", ylab=ax_lab,
         xaxs="i",yaxs="i",
         cex.axis=cex_axis)
    
    # Grid
    for (ky in ax_tickpos) lines(c(0, range$ymax*1.05), c(ky, ky), col="#eeeeee")
    
    # 50% of studies
    rect(0,               range$x[1],
         range$ymax*1.05, range$x[2],
         col="#cccccc66", border=NA)
    lines(c(range$y,range$y), c(ax_min,ax_max), col="#777777", lty=2)
    
    # # Density line and threshold
    # rect(0, data$x[min(which(data$y> threshold))], 
    #      1.05, data$x[max(which(data$y> threshold))], 
    #      col="#cccccc66", border=NA)
    # lines(c(threshold,threshold), c(ax_min,ax_max), col="#777777", lty=2)
    
    lines(data$y, data$x, col="blue",lwd=3)
    
    # rect(0.05, data$x[min(which(data$y> threshold))]-ax_max*0.025, 
    #      0.25, data$x[min(which(data$y> threshold))]+ax_max*0.025, 
    #      col="#ffffff99", border="#99999999")
    # text(0.15, data$x[min(which(data$y> threshold))], paste(round(data$x[min(which(data$y> threshold))], digits=0)), cex=0.8, col="#777777")
    # 
    # rect(0.05, data$x[max(which(data$y> threshold))]-ax_max*0.025, 
    #      0.25, data$x[max(which(data$y> threshold))]-ax_max*0.025, 
    #      col="#ffffff99", border="#99999999")
    # text(0.15, data$x[max(which(data$y> threshold))], paste(round(data$x[max(which(data$y> threshold))], digits=0)), cex=0.8, col="#777777")
    
    #text(threshold+0.05, ax_max - ax_max*0.025, paste0(threshold*100, "%"), adj = c(0,1), cex=0.8, col="#777777")
    
    #text(range$y+0.05, ax_max - ax_max*0.025, paste0(threshold*100, "%"), adj = c(0,1), cex=0.8, col="#777777")
    
    # Axes
    axis(2, at=ax_tickpos, cex=cex_axis, cex.axis=cex_axis)
    
    box()
  }

  
}

#== Boxplot function ===========
plot_boxplot <- function(
  data,
  period,
  ax_min,
  ax_max,
  plt = c(0.15, 0.95, 0.15, 0.95),
  new = TRUE,
  switch_axes = FALSE
) {
  
  # Initialise
  par(plt=plt, new=new)
  myperiod <- period 
  
  if (!switch_axes) {
    # Initialise
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(ax_min,ax_max), ylim=c(0,1),
         xlab="", ylab="",
         xaxs="i",yaxs="i")
    
    var <- paste(data$variable[1])
    
    # Boxplots potential
    # 1.5°C
    ypos   <- 0.25
    offset <- 0.1
    iam_data <- data %>% 
      select(model,scenario,region,tempcat,period,variable,value) %>% 
      mutate(value = value/1000) %>% 
      compute_stats_tempcat(var) %>% 
      filter(period == myperiod, tempcat == "1.5°C scenario")

    lines(c(iam_data$min, iam_data$max), c(ypos,ypos))
    rect(iam_data$q15, ypos-offset, 
         iam_data$q85, ypos+offset, 
         col="#074594ff")
    lines(c(iam_data$med, iam_data$med), c(ypos-offset, ypos+offset))
    points(c(iam_data$mean, ypos), pch=21, col="#000000", bg="#ffffff")
    
    # Likely 2.0°C
    ypos   <- 0.5
    offset <- 0.1
    iam_data <- data %>% 
      select(model,scenario,region,tempcat,period,variable,value) %>% 
      mutate(value = value/1000) %>% 
      compute_stats_tempcat(var) %>% 
      filter(period == myperiod, tempcat == "Likely 2.0°C scenario")
    
    lines(c(iam_data$min, iam_data$max), c(ypos,ypos))
    rect(iam_data$q15, ypos-offset, 
         iam_data$q85, ypos+offset, 
         col="#3f91c5ff")
    lines(c(iam_data$med, iam_data$med), c(ypos-offset, ypos+offset))
    points(c(iam_data$mean, ypos), pch=21, col="#000000", bg="#ffffff") 
  } else {
    # Initialise
    plot(0,0,
         type="n",
         axes=FALSE,
         xlim=c(0,1), ylim=c(ax_min,ax_max),
         xlab="", ylab="",
         xaxs="i",yaxs="i")
    
    var <- paste(data$variable[1])
    
    # Boxplots costs
    # 1.5°C
    xpos   <- 0.25
    offset <- 0.1
    iam_data <- data %>% 
      select(model,scenario,region,tempcat,period,variable,value) %>% 
      compute_stats_tempcat(var) %>% 
      filter(period == myperiod, tempcat == "1.5°C scenario")
    lines(c(xpos,xpos), c(iam_data$min, iam_data$max))
    rect(xpos-offset, iam_data$q15, 
         xpos+offset, iam_data$q85, 
         col="#074594ff")
    lines(c(xpos-offset, xpos+offset), c(iam_data$med, iam_data$med))
    #points(c(xpos, iam_data$mean), pch=21, col="#000000", bg="#ffffff")
    
    # Likely 2.0°C
    xpos   <- 0.5
    offset <- 0.1
    iam_data <- data %>% 
      select(model,scenario,region,tempcat,period,variable,value) %>% 
      compute_stats_tempcat(var) %>% 
      filter(period == myperiod, tempcat == "Likely 2.0°C scenario")
    lines(c(xpos,xpos), c(iam_data$min, iam_data$max))
    rect(xpos-offset, iam_data$q15, 
         xpos+offset, iam_data$q85, 
         col="#3f91c5ff")
    lines(c(xpos-offset, xpos+offset), c(iam_data$med, iam_data$med))
    #points(c(xpos, iam_data$mean), pch=21, col="#000000", bg="#ffffff")
  }
  
}



#== Generate potential data =====
generate_potentials <- function(data, technology, isteps) {
  # Adjust the maximum here to change the scale
  df <- data.frame(v=isteps)
  
  # Get a list of resources, or define it yourself
  resources <- unique(
    data[data$measurement=="max" & data$variable!="cost",]$variable
  )
  resources <- resources[!is.na(resources)]
  
  # # max
  # pot_max <- countranges(df, 
  #                        filter(data, `Data categorisationyear` == 2050 & `Data categorisationsystem boundaries` == "Global"),
  #                        resources,
  #                        "max"
  # )
  
  # range
  pot_range <- countranges(df, 
                           data,
                           resources,
                           "max"
  )
  
  return(pot_range)
}

#== Generate cost data =====
generate_costs <- function(data, technology, steps) {
  # Adjust the maximum here to change the scale
  df <- data.frame(v=steps)

  # Get cost label  
  costs <- unique(
    data[data$measurement=="max" & 
           data$variable=="cost",
         ]$variable
  )
  
  # Generate ranges
  cost_range <- countranges(df, 
                            data,
                            costs,
                            "range"
  )
  
  return(cost_range)
}

#== Synthesis plot function ==========
plot_synthesis_part2 <- function(
  data_pot,  # Bottom-up data potential
  data_cost, # Bottom-up data costs
  data_iam, # IAM data
  data_rev, # Previous review data 
  data_ej,  # Expert judgment data
  #--- Data options ---
  threshold, # Threshold for density plots (same for both)
  period,
  pot_scalecol  = 1,
  cost_scalecol = 1,
  #--- Plot options ---
  do_bu  = TRUE,
  do_iam = TRUE,
  do_rev = TRUE,
  alpha  = "99",
  #--- Other options ---
  xlab  = "Potential [Gt(CO2)]",
  ylab  = "Costs [$US/t(CO2)]",
  title = "[NET name]",
  file  = NULL,
  DEBUG = FALSE
) 
{
  
  #-- Initialise -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Initialising...\n")
  # Create empty image file if needed
  if (!is.null(file)) {
    if(substr(file, nchar(file)-2, nchar(file)) =="svg") svglite::svglite(file=file, width=6.5, height=6.5)
    if(substr(file, nchar(file)-2, nchar(file)) =="png") png(filename=file, res = 600, width=4000, height=4000)
  }
  
  # Define data
  pot_steps  <- data_pot$v
  cost_steps <- data_cost$v
  
  # Define data boundaries
  if (DEBUG) cat("[plot_heatmap]   - Defining boundaries\n")
  xmin <- 0
  xmax <- max(pot_steps)
  ymin <- 0
  ymax <- max(cost_steps)
  if (DEBUG) cat("[plot_heatmap]     > xmin: ",xmin,", xmax: ",xmax,", ymin: ",ymin,", ymax:",ymax,"\n")
  
  # Define axis tick positions
  if (DEBUG) cat("[plot_heatmap]   - Defining axes tick positions\n")
  axes_xtick_pos <- seq(0,max(pot_steps), max(pot_steps)/4)
  axes_ytick_pos <- seq(0,max(cost_steps),max(cost_steps)/4)
  
  # Define colours
  if (DEBUG) cat("[plot_heatmap]   - Defining colours\n")
  white2red  <- colorRampPalette(c("white", "red"))
  white2blue <- colorRampPalette(c("white", "blue"))
  pot_cols   <- paste0(white2red(length(pot_steps)),   alpha)
  cost_cols  <- paste0(white2blue(length(cost_steps)), alpha)
  
  # Define plot positions
  if (DEBUG) cat("[plot_heatmap]   - Defining plot positions\n")
  plt_pos <- list(
    "main"  = c(0.30, 0.90, 0.30, 0.90),
    "dpot"  = c(0.30, 0.90, 0.15, 0.30),
    "dcost" = c(0.15, 0.30, 0.30, 0.90),
    "bpot"  = c(0.30, 0.90, 0.90, 0.99),
    "bcost" = c(0.90, 0.99, 0.30, 0.90)
  )
  
  
  #-- Processing data -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Processing data...\n")
  # Potential
  if (DEBUG) cat("[plot_heatmap]   - Computing rectangles coordinates for potentials\n")
  pot_rects <- data.frame(xmin  = lag(pot_steps, default=pot_steps[2]-pot_steps[1]), 
                          xmax  = pot_steps, 
                          ymin  = rep(ymin, length(pot_steps)), 
                          ymax  = rep(ymax, length(pot_steps)), 
                          colid = pmin(length(pot_steps), pmax(1, round(data_pot$pcnt*(max(pot_steps)*pot_scalecol)/100, digit=0))))
  pot_rects$colidnorm <- round(pot_rects$colid/max(pot_rects$colid)*length(pot_steps), digits=0)
  pot_rects$fill      <- pot_cols[pot_rects$colidnorm]
  
  # Cost
  if (DEBUG) cat("[plot_heatmap]   - Computing rectangles coordinates for costs\n")
  cost_rects <- data.frame(xmin  = rep(xmin, length(cost_steps)),
                           xmax  = rep(xmax, length(cost_steps)),
                           ymin  = lag(cost_steps, default=cost_steps[2]-cost_steps[1]),
                           ymax  = cost_steps,
                           colid = pmin(length(cost_steps), pmax(1, round(data_cost$pcnt*(max(cost_steps)*cost_scalecol)/100, digit=0))))
  cost_rects$colidnorm <- round(cost_rects$colid/max(cost_rects$colid)*length(cost_steps), digits=0)
  cost_rects$fill      <- cost_cols[cost_rects$colidnorm]
  
  if (DEBUG) cat("[plot_heatmap]   - Computing density for potentials\n")
  pot_d   <- density(unlist(sapply(1:nrow(data_pot), function(x) rep(data_pot$v[x], data_pot$value[x]))))

  pot_dx     <- (pot_d$x - lag(pot_d$x))[2]
  pot_d_ymax <- max(pot_d$y)
  pot_d_yseq <- seq(0.5*pot_d_ymax, pot_d_ymax, (pot_d_ymax - 0.5*pot_d_ymax)/1000)
  
  pot_d_res    <- sapply(pot_d_yseq, function(x) sum(pot_d$y[which(pot_d$y >= x)]*pot_dx))
  pot_d_threshold <- max(pot_d_yseq[which(abs(pot_d_res - threshold) == min(abs(pot_d_res - threshold)))])
  pot_d_xrange <- pot_d$x[range(which(pot_d$y >= pot_d_threshold))]
  
  #pot_d_threshold <- pot_d_threshold/max(pot_d$y)
  #pot_d$y <- pot_d$y/max(pot_d$y)
  
  
  if (DEBUG) cat("[plot_heatmap]   - Computing density for costs\n")
  cost_d   <- density(unlist(sapply(1:nrow(data_cost), function(x) rep(data_cost$v[x], data_cost$value[x]))))
  
  cost_dx     <- (cost_d$x - lag(cost_d$x))[2]
  cost_d_ymax <- max(cost_d$y)
  cost_d_yseq <- seq(0.5*cost_d_ymax, cost_d_ymax, (cost_d_ymax - 0.5*cost_d_ymax)/1000)
  
  cost_d_res    <- sapply(cost_d_yseq, function(x) sum(cost_d$y[which(cost_d$y >= x)]*cost_dx))
  cost_d_threshold <- max(cost_d_yseq[which(abs(cost_d_res - threshold) == min(abs(cost_d_res - threshold)))])
  cost_d_xrange <- cost_d$x[range(which(cost_d$y >= cost_d_threshold))]
  
  #cost_d_threshold <- cost_d_threshold/max(cost_d$y)
  #cost_d$y <- cost_d$y/max(cost_d$y)
  
  
  #-- Plotting data -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Plotting data...\n")
  
  #-- Main plot (central heatmap) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Main plot (central heatmap)\n")
  # Initialise
  par(mar = c(0,0,0,0),
      las = 1,
      plt = plt_pos$main)
  plot(0,0,
       type="n",
       axes=FALSE,
       xlim=c(xmin,xmax), ylim=c(ymin,ymax),
       xlab="", ylab="",
       xaxs="i",yaxs="i")
  
  # Grid
  for (kx in axes_xtick_pos) lines(c(kx, kx),     c(ymin, ymax), col="#eeeeee")
  for (ky in axes_ytick_pos) lines(c(xmin, xmax), c(ky, ky),     col="#eeeeee")
  
  # Bottom-up studies
  for (kr in 1:nrow(pot_rects)) {
    rect(pot_rects$xmin[kr], pot_rects$ymin[kr], pot_rects$xmax[kr], pot_rects$ymax[kr], col=pot_rects$fill[kr],  border=NA)
  }
  for (kr in 1:nrow(cost_rects)) {
    rect(cost_rects$xmin[kr],cost_rects$ymin[kr],cost_rects$xmax[kr],cost_rects$ymax[kr],col=cost_rects$fill[kr], border=NA)
  }
  
  # Density data + threshold
  # rect(
  #   pot_d$x[min(which(pot_d$y> threshold))], cost_d$x[min(which(cost_d$y> threshold))],
  #   pot_d$x[max(which(pot_d$y> threshold))], cost_d$x[max(which(cost_d$y> threshold))],
  #   lwd=3, lty=3 
  # )
  rect(
    pot_d_xrange[1], cost_d_xrange[1],
    pot_d_xrange[2], cost_d_xrange[2],
    lwd=1, lty=1 
  )
  
  # Expert judgment
  if (!is.null(data_ej)) {
    rect(
      data_ej$pot_min, data_ej$cost_min,
      data_ej$pot_max, data_ej$cost_max,
      lwd=2, lty=1,
      angle = 45, density = 8,
      border="#000000"
    )
  }
  
  # Former review(s)
  if (!is.null(data_rev) && do_rev) {
    for (kr in nrow(data_rev)) {
      rect(
        data_rev$totalPotential.min[kr], data_rev$cost.min[kr],
        data_rev$totalPotential.max[kr], data_rev$cost.max[kr],
        lwd=2, lty=2, border="#ffffffff")    
      
    }
  }
  
  # # IAM data
  # if (!is.null(data_iam) && do_iam) {
  #   iam_data <- data_iam %>% 
  #     select(model,scenario,tempcat,period,variable,value) %>% 
  #     spread(variable, value) %>% 
  #     rename(price=`Price|Carbon`) %>% 
  #     rename(potential=`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
  #     mutate(potential=potential/1000)
  #   
  #   points(iam_data$potential[which(iam_data$period == 2050 & iam_data$potential != 0)], 
  #          iam_data$price[which(iam_data$period == 2050 & iam_data$potential != 0)], 
  #          pch=21, col="#777777ff", bg="#ffffff00")
  #   points(iam_data$potential[which(iam_data$period == 2100 & iam_data$potential != 0)], 
  #          iam_data$price[which(iam_data$period == 2100 & iam_data$potential != 0)], 
  #          pch=19, col="#777777ff", cex=0.5)
  # }
  
  # Add title
  par(xpd=FALSE)
  text(xmax/2, ymax+(ymax-ymin)*1.05, title, cex=2)
  par(xpd=FALSE)
  
  box()
  
  #-- Bottom plot (density plot of potentials) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Bottom plot (density plot of potentials)\n")
  plot_density(
    pot_d, 
    list(x=pot_d_xrange, y=pot_d_threshold, ymax=max(pot_d$y)),
    threshold, 0, xmax, axes_xtick_pos,
    ax_lab = "", #  "Potential [Gt(CO2)]",
    plt = plt_pos$dpot,
    cex_axis=p_cex_axis,
    switch_axes = FALSE
  )
  
  #-- Left-side plot (density plot of costs) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Left-side plot (density plot of costs)\n")
  plot_density(
    cost_d, 
    list(x=cost_d_xrange, y=cost_d_threshold, ymax=max(cost_d$y)),
    threshold, 0, ymax, axes_ytick_pos,
    ax_lab = "", # "Costs [US$/t(CO2)]",
    plt = plt_pos$dcost,
    cex_axis=p_cex_axis,
    switch_axes = TRUE
  )
  
  
  if (!is.null(data_iam) && do_iam) {
    #-- Top plot (boxplots of potentials) -------------------------
    if (DEBUG) cat("[plot_heatmap]   - Top plot (boxplots of potentials)\n")
    plot_boxplot(
      data_iam %>% 
        filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass"), 
      period, 0, xmax,
      plt = plt_pos$bpot
    )
    
    # #-- Right-side plot (boxplots of potentials) -------------------------
    # if (DEBUG) cat("[plot_heatmap]   - Right-side plot (boxplots of potentials)\n")
    # plot_boxplot(
    #   data_iam%>% 
    #     filter(variable == "Price|Carbon"), 
    #   2050, 0, 400,
    #   plt = plt_pos$bcost,
    #   switch_axes = TRUE
    # )
  }
  
  
  #-- Close file --------------
  if (DEBUG) cat("[plot_heatmap] Closing file (if needed)...\n")
  if (!is.null(file)) {
    dev.off()
  }

}

plot_synthesis_part2_DAC <- function(
  data_pot,  # Bottom-up data potential
  data_cost, # Bottom-up data costs
  data_iam, # IAM data
  data_rev, # Previous review data 
  data_ej,  # Expert judgment data
  #--- Data options ---
  threshold, # Threshold for density plots (same for both)
  period,
  pot_scalecol  = 1,
  cost_scalecol = 1,
  #--- Plot options ---
  do_bu  = TRUE,
  do_iam = TRUE,
  do_rev = TRUE,
  alpha  = "99",
  #--- Other options ---
  xlab  = "Potential [Gt(CO2)]",
  ylab  = "Costs [$US/t(CO2)]",
  title = "[NET name]",
  file  = NULL,
  DEBUG = FALSE
) 
{
  
  #-- Initialise -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Initialising...\n")
  # Create empty image file if needed
  if (!is.null(file)) {
    if(substr(file, nchar(file)-2, nchar(file)) =="svg") svglite::svglite(file=file, width=6.5, height=6.5)
    if(substr(file, nchar(file)-2, nchar(file)) =="png") png(filename=file, res = 600, width=4000, height=4000)
  }
  
  # Define data
  #pot_steps  <- data_pot$v
  cost_steps <- data_cost$v
  
  # Define data boundaries
  if (DEBUG) cat("[plot_heatmap]   - Defining boundaries\n")
  xmin <- 0
  xmax <- 100 # Arbitrary value
  ymin <- 0
  ymax <- max(cost_steps)
  if (DEBUG) cat("[plot_heatmap]     > xmin: ",xmin,", xmax: ",xmax,", ymin: ",ymin,", ymax:",ymax,"\n")
  
  # Define axis tick positions
  if (DEBUG) cat("[plot_heatmap]   - Defining axes tick positions\n")
  axes_xtick_pos <- seq(0,xmax, xmax/4)
  axes_ytick_pos <- seq(0,max(cost_steps),max(cost_steps)/4)
  
  # Define colours
  if (DEBUG) cat("[plot_heatmap]   - Defining colours\n")
  white2red  <- colorRampPalette(c("white", "red"))
  white2blue <- colorRampPalette(c("white", "blue"))
  #pot_cols   <- paste0(white2red(length(pot_steps)),   alpha)
  cost_cols  <- paste0(white2blue(length(cost_steps)), alpha)
  
  # Define plot positions
  if (DEBUG) cat("[plot_heatmap]   - Defining plot positions\n")
  plt_pos <- list(
    "main"  = c(0.30, 0.90, 0.30, 0.90),
    "dpot"  = c(0.30, 0.90, 0.15, 0.30),
    "dcost" = c(0.15, 0.30, 0.30, 0.90),
    "bpot"  = c(0.30, 0.90, 0.90, 0.99),
    "bcost" = c(0.90, 0.99, 0.30, 0.90)
  )
  
  
  #-- Processing data -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Processing data...\n")
  # # Potential
  # if (DEBUG) cat("[plot_heatmap]   - Computing rectangles coordinates for potentials\n")
  # pot_rects <- data.frame(xmin  = lag(pot_steps, default=pot_steps[2]-pot_steps[1]), 
  #                         xmax  = pot_steps, 
  #                         ymin  = rep(ymin, length(pot_steps)), 
  #                         ymax  = rep(ymax, length(pot_steps)), 
  #                         colid = pmin(length(pot_steps), pmax(1, round(data_pot$pcnt*(max(pot_steps)*pot_scalecol)/100, digit=0))))
  # pot_rects$colidnorm <- round(pot_rects$colid/max(pot_rects$colid)*length(pot_steps), digits=0)
  # pot_rects$fill      <- pot_cols[pot_rects$colidnorm]
  
  # Cost
  if (DEBUG) cat("[plot_heatmap]   - Computing rectangles coordinates for costs\n")
  cost_rects <- data.frame(xmin  = rep(xmin, length(cost_steps)),
                           xmax  = rep(xmax, length(cost_steps)),
                           ymin  = lag(cost_steps, default=cost_steps[2]-cost_steps[1]),
                           ymax  = cost_steps,
                           colid = pmin(length(cost_steps), pmax(1, round(data_cost$pcnt*(max(cost_steps)*cost_scalecol)/100, digit=0))))
  cost_rects$colidnorm <- round(cost_rects$colid/max(cost_rects$colid)*length(cost_steps), digits=0)
  cost_rects$fill      <- cost_cols[cost_rects$colidnorm]
  
  # if (DEBUG) cat("[plot_heatmap]   - Computing density for potentials\n")
  # pot_d   <- density(unlist(sapply(1:nrow(data_pot), function(x) rep(data_pot$v[x], data_pot$value[x]))))
  # pot_d$y <- pot_d$y/max(pot_d$y)
  
  if (DEBUG) cat("[plot_heatmap]   - Computing density for costs\n")
  cost_d   <- density(unlist(sapply(1:nrow(data_cost), function(x) rep(data_cost$v[x], data_cost$value[x]))))
  
  cost_dx     <- (cost_d$x - lag(cost_d$x))[2]
  cost_d_ymax <- max(cost_d$y)
  cost_d_yseq <- seq(0.5*cost_d_ymax, cost_d_ymax, (cost_d_ymax - 0.5*cost_d_ymax)/1000)
  
  cost_d_res    <- sapply(cost_d_yseq, function(x) sum(cost_d$y[which(cost_d$y >= x)]*cost_dx))
  cost_d_threshold <- max(cost_d_yseq[which(abs(cost_d_res - threshold) == min(abs(cost_d_res - threshold)))])
  cost_d_xrange <- cost_d$x[range(which(cost_d$y >= cost_d_threshold))]
  
  # cost_d_threshold <- cost_d_threshold/max(cost_d$y)
  # cost_d$y <- cost_d$y/max(cost_d$y)
  
  
  #-- Plotting data -----------------------------------------------
  if (DEBUG) cat("[plot_heatmap] Plotting data...\n")
  
  #-- Main plot (central heatmap) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Main plot (central heatmap)\n")
  # Initialise
  par(mar = c(0,0,0,0),
      las = 1,
      plt = plt_pos$main)
  plot(0,0,
       type="n",
       axes=FALSE,
       xlim=c(xmin,xmax), ylim=c(ymin,ymax),
       xlab="", ylab="",
       xaxs="i",yaxs="i")
  
  # Grid
  for (kx in axes_xtick_pos) lines(c(kx, kx),     c(ymin, ymax), col="#eeeeee")
  for (ky in axes_ytick_pos) lines(c(xmin, xmax), c(ky, ky),     col="#eeeeee")
  
  # Bottom-up studies
  # for (kr in 1:nrow(pot_rects)) {
  #   rect(pot_rects$xmin[kr], pot_rects$ymin[kr], pot_rects$xmax[kr], pot_rects$ymax[kr], col=pot_rects$fill[kr],  border=NA)
  # }
  for (kr in 1:nrow(cost_rects)) {
    rect(cost_rects$xmin[kr],cost_rects$ymin[kr],cost_rects$xmax[kr],cost_rects$ymax[kr],col=cost_rects$fill[kr], border=NA)
  }
  
  # Density data + threshold
  # rect(
  #   pot_d$x[min(which(pot_d$y> threshold))], cost_d$x[min(which(cost_d$y> threshold))],
  #   pot_d$x[max(which(pot_d$y> threshold))], cost_d$x[max(which(cost_d$y> threshold))],
  #   lwd=3, lty=3 
  # )
  
  # Expert judgment
  if (!is.null(data_ej)) {
    rect(
      data_ej$pot_min, data_ej$cost_min,
      data_ej$pot_max, data_ej$cost_max,
      lwd=2, lty=1,
      angle = 45, density = 8,
      border="#000000"
    )
  }
  
  # Former review(s)
  if (!is.null(data_rev) && do_rev) {
    for (kr in nrow(data_rev)) {
      rect(
        data_rev$totalPotential.min[kr], data_rev$cost.min[kr],
        data_rev$totalPotential.max[kr], data_rev$cost.max[kr],
        lwd=2, lty=2, border="#ffffffff")    
      
    }
  }
  
  # # IAM data
  # if (!is.null(data_iam) && do_iam) {
  #   iam_data <- data_iam %>% 
  #     select(model,scenario,tempcat,period,variable,value) %>% 
  #     spread(variable, value) %>% 
  #     rename(price=`Price|Carbon`) %>% 
  #     rename(potential=`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
  #     mutate(potential=potential/1000)
  #   
  #   points(iam_data$potential[which(iam_data$period == 2050 & iam_data$potential != 0)], 
  #          iam_data$price[which(iam_data$period == 2050 & iam_data$potential != 0)], 
  #          pch=21, col="#777777ff", bg="#ffffff00")
  #   points(iam_data$potential[which(iam_data$period == 2100 & iam_data$potential != 0)], 
  #          iam_data$price[which(iam_data$period == 2100 & iam_data$potential != 0)], 
  #          pch=19, col="#777777ff", cex=0.5)
  # }
  
  # Add title
  par(xpd=FALSE)
  text(xmax/2, ymax+(ymax-ymin)*1.05, title, cex=2)
  par(xpd=FALSE)
  
  box()
  
  #-- Bottom plot (density plot of potentials) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Bottom plot (density plot of potentials)\n")
  par(plt=plt_pos$dpot, new=new)
  plot(0,0,
       type="n",
       axes=FALSE,
       xlim=c(0, xmax), ylim=c(0,1.05),
       xlab= "", ylab="", #"Potential [Gt(CO2)]", 
       xaxs="i",yaxs="i",
       cex.axis=p_cex_axis)
  # Grid
  for (kx in axes_xtick_pos) lines(c(kx, kx), c(0, 1.05), col="#eeeeee")
  # Axes
  axis(1, at=axes_xtick_pos, cex.axis=p_cex_axis)
  
  text(50, 0.5, "NA", col = "grey", cex = 3)
  
  box()
  
  #-- Left-side plot (density plot of costs) -------------------------
  if (DEBUG) cat("[plot_heatmap]   - Left-side plot (density plot of costs)\n")
  plot_density(
    cost_d, 
    list(x=cost_d_xrange, y=cost_d_threshold, ymax=max(cost_d$y)),
    threshold, 0, ymax, axes_ytick_pos,
    ax_lab = "", #  "Costs [US$/t(CO2)]",
    plt = plt_pos$dcost,
    cex_axis=p_cex_axis,
    switch_axes = TRUE
  )
  
  
  if (!is.null(data_iam) && do_iam) {
    #-- Top plot (boxplots of potentials) -------------------------
    if (DEBUG) cat("[plot_heatmap]   - Top plot (boxplots of potentials)\n")
    plot_boxplot(
      data_iam %>% 
        filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass"), 
      period, 0, xmax,
      plt = plt_pos$bpot
    )
    
    # #-- Right-side plot (boxplots of potentials) -------------------------
    # if (DEBUG) cat("[plot_heatmap]   - Right-side plot (boxplots of potentials)\n")
    # plot_boxplot(
    #   data_iam%>% 
    #     filter(variable == "Price|Carbon"), 
    #   2050, 0, 400,
    #   plt = plt_pos$bcost,
    #   switch_axes = TRUE
    # )
  }
  
  
  #-- Close file --------------
  if (DEBUG) cat("[plot_heatmap] Closing file (if needed)...\n")
  if (!is.null(file)) {
    dev.off()
  }
  
}