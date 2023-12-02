

# lib_l <- matrix(c(1,15,21,35,41,55), ncol = 2, byrow = T) #when no. of NA is 5
lib_l <- matrix(c(1,15,28,42,55,69), ncol = 2, byrow = T) #when no. of NA is 12


##### Determine the best E #####
TestE <- function(time_series,
                  d_name,
                  E_range,
                  nn = "e+1",
                  lib_type = "half",
                  lib_l = NULL,
                  pred_l = NULL,
                  E_only = FALSE,
                  out_dir){
  ts.l <- length(time_series)
  
  # Select library reconstruction strategies
  if(lib_type == "half"){
    # Half time series used to reconstruct library
    lib_l <- c(1, floor(ts.l/2))
    pred_l <- c(floor(ts.l/2) + 1, ts.l)
  }else if(lib_type == "full"){
    # Full time series used to reconstruct library
    lib_l <- c(1, ts.l)
    pred_l <- c(1, ts.l)
  }else if(lib_type == "manual"){
    lib_l <- lib_l
    pred_l <- pred_l
  }else{
    warning("Invalid lib_type option!")
  }
  
  lib_l <- lib_l
  pred_l <- lib_l
  
  # Do simplex projection to determine the best E
  simplex.out <- simplex(time_series, lib_l, pred_l, num_neighbors = nn, E = E_range, silent = T)
  
  if(E_only){
    return(simplex.out[which.min(simplex.out$rmse),"E"])
  }else{
    return(simplex.out)
  }
}





##### Visualize the best E #####
Eplot <- function(time_series,
                  d_name,
                  E_range,
                  nn = "e+1",
                  lib_type = "half",
                  lib_l = NULL,
                  pred_l = NULL){
  ts.l <- length(time_series)
  
  # Select library reconstruction strategies
  if(lib_type == "half"){
    # Half time series used to reconstruct library
    lib_l <- c(1, floor(ts.l/2))
    pred_l <- c(floor(ts.l/2) + 1, ts.l)
  }else if(lib_type == "full"){
    # Full time series used to reconstruct library
    lib_l <- c(1, ts.l)
    pred_l <- c(1, ts.l)
  }else if(lib_type == "manual"){
    lib_l <- lib_l
    pred_l <- pred_l
  }else{
    warning("Invalid lib_type option!")
  }
  
  lib_l <- lib_l
  pred_l <- lib_l
  
  # Do simplex projection to determine the best E
  simplex.out <- simplex(time_series, lib_l, pred_l, num_neighbors = nn, E = E_range, silent = T)
  
  prefix <- "Ahg"
  if(d_name=="BLRP"){prefix <- "AhgpsbD "}
  
  g1 <- ggplot(data = simplex.out, aes(x = E, y = as.numeric(rho))) +
    geom_line() +
    scale_x_continuous(breaks = seq(1,length(E_range),1)) +
    theme_bw(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          plot.title = element_text(size = 7),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2)) +
    labs(title = bquote(paste(italic(.(prefix)), italic(.(d_name)), sep="")),
         x = expression(paste("Embedding dimention (", italic(E), ")")),
         y = expression(paste("Forecast skill (", italic(rho), ")")))
  
  g2 <- ggplot(data = simplex.out, aes(x = E, y = as.numeric(mae))) +
    geom_line() +
    scale_x_continuous(breaks = seq(1,length(E_range),1)) +
    theme_bw(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          plot.title = element_text(size = 7),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2)) +
    labs(title = bquote(paste(italic(.(prefix)), italic(.(d_name)), sep="")),
         x = expression(paste("Embedding dimention (", italic(E), ")")),
         y = "Forecast skill (MAE)")
  
  g3 <- ggplot(data = simplex.out, aes(x = E, y = as.numeric(rmse))) +
    geom_line() +
    scale_x_continuous(breaks = seq(1,length(E_range),1)) +
    theme_bw(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          plot.title = element_text(size = 7),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2)) +
    labs(title = bquote(paste(italic(.(prefix)), italic(.(d_name)), sep="")),
         x = expression(paste("Embedding dimention (", italic(E), ")")),
         y = "Forecast skill (RMSE)")
  
  g <- list(g1, g2, g3)
  
  return(g)
}





##### make_surrogate_seasonal (spar = 0.5) #####
make_surrogate_seasonal <- function(ts, num_surr = num_surr, T_period = 12, spar)
{
  if (is.data.frame(ts))
  {
    ts <- ts[[1]]
  }
  
  if (any(!is.finite(ts)))
    stop("input time series contained invalid values")
  
  n <- length(ts)
  I_season <- suppressWarnings(matrix(1:T_period, nrow = n, ncol = 1))
  
  # Calculate seasonal cycle using smooth.spline
  seasonal_F <- smooth.spline(c(I_season - T_period, I_season, 
                                I_season + T_period), 
                              c(ts, ts, ts), spar = spar)
  seasonal_cyc <- predict(seasonal_F, I_season)$y
  seasonal_resid <- ts - seasonal_cyc
  
  set.seed(1)
  matrix(unlist(
    lapply(seq(num_surr), function(i) {
      seasonal_cyc + sample(seasonal_resid, n)
    })
  ), ncol = num_surr)
}





###### CCM with seasonal surrogate function #####
CCMwSurrogate <- function(x, y, Ex, Ey, lag, num_surr, spar){
  #x_xmap_y_mean <- rep(NA, length=length(lag))
  y_xmap_x_mean <- rep(NA, length=length(lag))
  
  lib_l <- lib_l
  pred_l <- lib_l
  
  for(i in 1:length(lag)){
    #x_xmap_y <- ccm(d, lib = lib_l, pred = pred_l, E = Ex, lib_column = x, target_column = y, lib_sizes = 45, tp = lag[i], silent = T)
    y_xmap_x <- ccm(d, lib = lib_l, pred = pred_l, E = Ey, lib_column = y, target_column = x, lib_sizes = 45, tp = lag[i], silent = T)
    #x_xmap_y_mean[i] <- ccm_means(x_xmap_y)$rho
    y_xmap_x_mean[i] <- ccm_means(y_xmap_x)$rho
  }
  
  # # Seasonal surrogate 1 (x xmap y)
  # num_surr <- num_surr
  # surr_y_1 <- make_surrogate_seasonal(d[1:15,y], T_period = 12, num_surr = num_surr, spar = spar)
  # surr_y_2 <- make_surrogate_seasonal(d[28:42,y], T_period = 12, num_surr = num_surr, spar = spar)
  # surr_y_3 <- make_surrogate_seasonal(d[55:69,y], T_period = 12, num_surr = num_surr, spar = spar)
  # surr_y <- rbind(surr_y_1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
  #                 surr_y_2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #                 surr_y_3)
  # 
  # # Do CCM for all surrogate time series
  # x_xmap_surr_y <- matrix(NA, nrow = length(lag), ncol = num_surr)
  # for (i in 1:num_surr) {
  #   for(j in 1:length(lag)){
  #     x_xmap_surr_y[j,i] <- ccm_means(ccm(cbind(d[,x], surr_y[,i]), lib = lib_l, pred = pred_l, E = Ex, lib_sizes = 45, tp=lag[j], silent = T))$rho
  #   }
  # }
  # # Extract quantiles
  # x_xmap_surr_y_qs <- t(apply(x_xmap_surr_y, 1, function(rhos) quantile(rhos, p = c(0.025, 0.5, 0.975))))
  # 
  # # Combine resutls
  # x_xmap_y_summary <- cbind(lag, x_xmap_y_mean, x_xmap_surr_y_qs)
  # colnames(x_xmap_y_summary) <- c("lag_1", "rho_1", "lower2.5_1", "median_1", "upper97.5_1")
  
  # Seasonal surrogate 2 (y xmap x)
  surr_x_1 <- make_surrogate_seasonal(d[1:15,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x_2 <- make_surrogate_seasonal(d[28:42,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x_3 <- make_surrogate_seasonal(d[55:69,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x <- rbind(surr_x_1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                  surr_x_2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  surr_x_3)
  
  # Do CCM for all surrogate time series
  y_xmap_surr_x <- matrix(NA, nrow = length(lag), ncol = num_surr)
  for (i in 1:num_surr) {
    for(j in 1:length(lag)){
      y_xmap_surr_x[j,i] <- ccm_means(ccm(cbind(d[,y], surr_x[,i]), lib = lib_l, pred = pred_l, E = Ey, lib_sizes = 45, tp=lag[j], silent = T))$rho
    }
  }
  # Extract quantiles
  y_xmap_surr_x_qs <- t(apply(y_xmap_surr_x, 1, function(rhos) quantile(rhos, p = c(0.025, 0.5, 0.975))))
  
  # Combine resutls
  y_xmap_x_summary <- cbind(lag, y_xmap_x_mean, y_xmap_surr_x_qs)
  colnames(y_xmap_x_summary) <- c("lag_2", "rho_2", "lower2.5_2", "median_2", "upper97.5_2")
  
  # Return all result
  #return(as.data.frame(cbind(x_xmap_y_summary, y_xmap_x_summary)))
  return(as.data.frame(y_xmap_x_summary))
}





##### CCM plot function #####
CCMplot <- function(ccm_res, x_name, y_name, spar, outdir){
  
  g <- ggplot(data = ccm_res, aes(x = lag_2)) + 
    # Add 95% confidence intervals
    #geom_ribbon(aes(ymin = lower2.5_1, ymax = upper97.5_1), alpha = 0.3, fill = "gray30") +
    geom_ribbon(aes(ymin = lower2.5_2, ymax = upper97.5_2), alpha = 0.3, fill = "black") +
    #geom_line(aes(y=rho_1, colour = "rho_1")) +
    geom_line(aes(y=rho_2, colour = "rho_2")) +
    geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.3) +
    scale_colour_manual(values=c("black"), 
                        labels=c(bquote(paste(italic(.(y_name))," xmap ", 
                                              italic(.(x_name)), sep="")))) +
    scale_x_continuous(breaks=seq(ccm_res$lag_2[1], ccm_res$lag_2[nrow(ccm_res)],1)) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    coord_cartesian(ylim=c(0,1)) +
    theme_bw() + 
    theme(legend.position=c(0.5,1.15),
          legend.key.height=unit(.1, "cm"),
          legend.key.width = unit(.4, "cm"),
          legend.direction="vertical",
          legend.background = element_rect(fill="transparent"),
          legend.spacing.x = unit(0.05, 'cm'),
          legend.text=element_text(size=7,hjust=0.5), 
          legend.title = element_blank(),
          axis.title=element_text(size=7), 
          axis.text=element_text(size=7),
          plot.margin=unit(c(0, 1, 1, 1),"mm"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.2),
          plot.title = element_text(size=6)) + 
    labs(title="",
         x=expression(paste("Time to prediction (",italic(tp),")")), 
         y=expression(paste("Cross map skill (", italic(rho), ")")))
  
  return(g)
}





##### Check convergence #####
Convergence <- function(data, x, y, Ex, Ey, tpx, tpy, num_surr, spar){
  
  libx <- seq(Ex+1, 45, by = 1)
  liby <- seq(Ey+1, 45, by = 1)
  lib_l <- lib_l
  pred_l <- lib_l
  
  # x_xmap_y <- ccm(data, lib = lib_l, pred = pred_l, E = Ex, lib_column = x, target_column = y, 
  #                 lib_sizes = libx, tp = tpx, silent = T)
  y_xmap_x <- ccm(data, lib = lib_l, pred = pred_l, E = Ey, lib_column = y, target_column = x, 
                  lib_sizes = liby, tp = tpy, silent = T)
  # x_xmap_y_mean <- ccm_means(x_xmap_y)$rho
  y_xmap_x_mean <- ccm_means(y_xmap_x)$rho
  
  # # Seasonal surrogate 1 (x xmap y)
  # num_surr <- num_surr
  # surr_y_1 <- make_surrogate_seasonal(data[1:8,y], T_period = 8, num_surr = num_surr, spar = spar)
  # surr_y_2 <- make_surrogate_seasonal(data[14:21,y], T_period = 8, num_surr = num_surr, spar = spar)
  # surr_y_3 <- make_surrogate_seasonal(data[27:34,y], T_period = 8, num_surr = num_surr, spar = spar)
  # surr_y_4 <- make_surrogate_seasonal(data[40:47,y], T_period = 8, num_surr = num_surr, spar = spar)
  # surr_y <- rbind(surr_y_1, NA, NA, NA, NA, NA, surr_y_2, NA, NA, NA, NA, NA, 
  #                 surr_y_3, NA, NA, NA, NA, NA, surr_y_4)
  # 
  # # Do CCM for all surrogate time series
  # x_xmap_surr_y <- matrix(NA, nrow = length(libx), ncol = num_surr)
  # for (i in 1:num_surr) {
  #   x_xmap_surr_y[,i] <- ccm_means(ccm(cbind(data[,x], surr_y[,i]), lib = lib_l, pred = pred_l, E = Ex,
  #                                     lib_column = 1, target_column = 2,
  #                                     lib_sizes = libx, tp=tpx, silent=T))$rho
  # }
  # 
  # # Extract quantiles
  # x_xmap_surr_y_qs <- t(apply(x_xmap_surr_y, 1, function(rhos) quantile(rhos, p = c(0.025, 0.5, 0.975),na.rm=T)))
  # 
  # # Combine resutls
  # x_xmap_y_summary <- cbind(libx, x_xmap_y_mean, x_xmap_surr_y_qs)
  # colnames(x_xmap_y_summary) <- c("lib_x", "rho_x", "lower2.5_x", "median_x", "upper97.5_x")
  
  # Seasonal surrogate 2 (y xmap x)
  surr_x_1 <- make_surrogate_seasonal(d[1:15,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x_2 <- make_surrogate_seasonal(d[28:42,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x_3 <- make_surrogate_seasonal(d[55:69,x], T_period = 12, num_surr = num_surr, spar = spar)
  surr_x <- rbind(surr_x_1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                  surr_x_2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  surr_x_3)
  
  # Do CCM for all surrogate time series
  y_xmap_surr_x <- matrix(NA, nrow = length(liby), ncol = num_surr)
  for (i in 1:num_surr) {
    y_xmap_surr_x[,i] <- ccm_means(ccm(cbind(data[,y], surr_x[,i]), lib = lib_l, pred = pred_l, E = Ey,
                                       lib_column = 1, target_column = 2,
                                       lib_sizes = liby, tp=tpy, silent=T))$rho
  }
  
  # Extract quantiles
  y_xmap_surr_x_qs <- t(apply(y_xmap_surr_x, 1, function(rhos) quantile(rhos, p = c(0.025, 0.5, 0.975),na.rm=T)))
  
  # Combine resutls
  y_xmap_x_summary <- cbind(liby, y_xmap_x_mean, y_xmap_surr_x_qs)
  colnames(y_xmap_x_summary) <- c("lib_y", "rho_y", "lower2.5_y", "median_y", "upper97.5_y")
  
  # Return all result
  # return(list(as.data.frame(x_xmap_y_summary), as.data.frame(y_xmap_x_summary)))
  return(list(as.data.frame(y_xmap_x_summary)))
}




##### Convergence plot function #####
Convergence_plot <- function(conv_res, x_name, y_name, tpx, tpy, spar, outdir){
  
  # g1 <- ggplot(data = conv_res[[1]], aes(x = lib_x)) + 
  #   geom_ribbon(aes(ymin = lower2.5_x, ymax = upper97.5_x), 
  #               alpha = 0.3, fill = "gray30") +
  #   geom_line(aes(y = rho_x), colour = "gray30") +
  #   scale_x_continuous(breaks=seq(0,30,10)) +
  #   scale_y_continuous(breaks=seq(0,1,0.2)) +
  #   coord_cartesian(ylim=c(0,1)) +
  #   theme_bw() + 
  #   theme(legend.position="none", 
  #         plot.margin=unit(c(1, 1, 1, 1), "mm"), 
  #         plot.title=element_text(size=7,hjust=0.5,margin=margin(b=2)), 
  #         axis.title=element_text(size=7), 
  #         axis.text=element_text(size=7)) + 
  #   labs(title = c(paste0(x," xmap ", y, "\n (tp = ",tpx,")")), 
  #        x = "Library size", 
  #        y = expression(paste("Cross map skill (", italic(rho), ")")))
  # ggsave(paste0("04_Convergence_out/spar", spar, "/Convergence_",x,"xmap",y,"_tp=",tpx, "_spar", spar,".pdf"), 
  #        g1, height = 45, width = 38, units = "mm")
  
  g2 <- ggplot(data = conv_res[[1]], aes(x = lib_y)) + 
    geom_ribbon(aes(ymin = lower2.5_y, ymax = upper97.5_y), 
                alpha = 0.3, fill = "black") +
    geom_line(aes(y = rho_y), colour = "black") +
    scale_x_continuous(breaks=seq(0,max(conv_res[[1]]$lib_y),10)) +
    scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) +
    theme_bw() + 
    theme(legend.position="none", 
          plot.margin=unit(c(0, 1, 1, 1), "mm"), 
          axis.title=element_text(size=7), 
          axis.text=element_text(size=7),
          plot.title = element_text(size=7, margin=margin(-8,0,5,0), hjust = 0.5, vjust = -4),
          plot.subtitle = element_text(size=7, hjust = 0.5, vjust = -1)) + 
    labs(title=bquote(paste(italic(.(y_name))," xmap ", 
                            italic(.(x_name)), sep="")),
         subtitle=bquote(paste("(tp = ", .(tpy), ")", sep="")),
         x="Library size", 
         y=expression(paste("Cross map skill (", italic(rho), ")")))
  return(g2)
}

