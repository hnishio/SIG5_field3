

##### Visualization of Mar-Sep comparison #####
diff_vis_MarSep <- function(df, var, condition){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 25; seq1 <- seq(0,20,10)
    ymin2 <- -7; ymax2 <- 17; seq2 <- seq(-5,15,5)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin2 <- -2.5; ymax2 <- 1.5; seq2 <- seq(-2,1,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 5.1; seq1 <- seq(0,5,2.5)
    ymin2 <- -0.7; ymax2 <- 4.5; seq2 <- seq(0,4,1)
  }
  
  if(condition=="Sun"){
    subtitle <- "sun"
  }else if(condition=="Shade"){
    subtitle <- "shade"
  }

  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = `mu1_2.5%`, ymax = `mu1_97.5%`), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = `mu1_50%`), col = "#FF1493") +
    geom_point(aes(y = data_Mar), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = `mu2_2.5%`, ymax = `mu2_97.5%`), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = `mu2_50%`), col = "#522A17") +
    geom_point(aes(y = data_Sep), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0.3,0.3,0.1,0.3), "cm")) +
    labs(title = bquote(paste(italic(.(var)), " (", .(subtitle), ")", sep = "")),
         y = "Relative\ntranscript\nabundance")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = `diff_2.5%`, ymax = `diff_97.5%`), alpha = 0.5) +
    geom_line(aes(y = `diff_50%`)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.text = element_text(size = 7),
          plot.title = element_blank(),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0,0.3,0.3,0.3), "cm")) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Sep. - Mar.")
  
  g2 <- g2 + annotate("text", x = df$time[df$signif_diff], 
                      y = ymax2-(ymax2-ymin2)*0.1, label = "*", size = 3)
  
  return(list(g1, g2))
}





##### Visualization of Sun-Shade comparison #####
diff_vis_SunShade <- function(df, var, season){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 25; seq1 <- seq(0,20,10)
    ymin2 <- -6.2; ymax2 <- 2; seq2 <- seq(-6,2,2)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin2 <- -1.8; ymax2 <- 0.5; seq2 <- seq(-1.5,0.5,0.5)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 5.1; seq1 <- seq(0,5,2.5)
    ymin2 <- -2.8; ymax2 <- 1.5; seq2 <- seq(-2,1,1)
  }
  
  if(season=="Mar"){
    subtitle <- "March/Spring"
  }else if(season=="Sep"){
    subtitle <- "September/Autumn"
  }
  
  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = `mu1_2.5%`, ymax = `mu1_97.5%`), alpha = 0.3, fill = "orange") +
    geom_line(aes(y = `mu1_50%`), col = "orange") +
    geom_point(aes(y = data_Mar), col = "orange", alpha = 0.6) +
    geom_ribbon(aes(ymin = `mu2_2.5%`, ymax = `mu2_97.5%`), alpha = 0.3, fill = "gray30") +
    geom_line(aes(y = `mu2_50%`), col = "gray30") +
    geom_point(aes(y = data_Sep), col = "gray30", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0.3,0.3,0.1,0.3), "cm")) +
    labs(title = bquote(paste(italic(.(var)), " (", .(subtitle), ")", sep = "")),
         y = "Relative\ntranscript\nabundance")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = `diff_2.5%`, ymax = `diff_97.5%`), alpha = 0.5) +
    geom_line(aes(y = `diff_50%`)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.text = element_text(size = 7),
          plot.title = element_blank(),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0,0.3,0.3,0.3), "cm")) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Sun - Shade")
  
  g2 <- g2 + annotate("text", x = df$time[df$signif_diff], 
                      y = ymax2-(ymax2-ymin2)*0.1, label = "*", size = 3)
  
  return(list(g1, g2))
}





##### Visualization of local comparison #####
diff_vis_local <- function(df, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 30; seq1 <- seq(0,30,10)
    ymin2 <- -1; ymax2 <- 6.5; seq2 <- seq(0,6,2)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 5; seq1 <- seq(0,5,1)
    ymin2 <- -1.3; ymax2 <- 2.2; seq2 <- seq(-1,2,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- -0.2; ymax1 <- 8; seq1 <- seq(0,8,2)
    ymin2 <- -2.6; ymax2 <- 3.2; seq2 <- seq(-2,3,1)
  }
  
  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    annotate("rect", xmin = 36, xmax = 39, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = `mu1_2.5%`, ymax = `mu1_97.5%`), alpha = 0.3, fill = "black") +
    geom_line(aes(y = `mu1_50%`), col = "black") +
    geom_point(aes(y = data_A), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = `mu2_2.5%`, ymax = `mu2_97.5%`), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = `mu2_50%`), col = "orangered") +
    geom_point(aes(y = data_W), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = `mu3_2.5%`, ymax = `mu3_97.5%`), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = `mu3_50%`), col = "cyan3") +
    geom_point(aes(y = data_C), col = "cyan3", alpha = 0.6) +
    #geom_ribbon(aes(ymin = mu4_2.5, ymax = mu4_97.5), alpha = 0.3, fill = "lavenderblush4") +
    #geom_line(aes(y = mu4_50), col = "lavenderblush4") +
    #geom_point(aes(y = data_L), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,36,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 7),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 7, face = "italic"),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0.3,0.3,0.1,0.3), "cm")) +
    labs(title = var,
         y = "Relative\ntranscript\nabundance")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50") +
    annotate("rect", xmin = 36, xmax = 39, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = `diff2_2.5%`, ymax = `diff2_97.5%`), alpha = 0.5, fill = "orangered") +
    geom_line(aes(y = `diff2_50%`), col = "orangered") +
    geom_ribbon(aes(ymin = `diff3_2.5%`, ymax = `diff3_97.5%`), alpha = 0.5, fill = "cyan3") +
    geom_line(aes(y = `diff3_50%`), col = "cyan3") +
    #geom_line(aes(y = diff4_50), col = "lavenderblush4") +
    #geom_ribbon(aes(ymin = diff4_2.5, ymax = diff4_97.5), alpha = 0.5, fill = "lavenderblush4") +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,36,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7),
          axis.text = element_text(size = 7),
          plot.title = element_blank(),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0,0.3,0.3,0.3), "cm")) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Difference\nagainst\nambient")
  
  g2 <- g2 + annotate("text", x = df$time[df$signif_diff2], 
                      y = ymax2-(ymax2-ymin2)*0.07, label = "*", size = 3, color = "orangered")
  ifelse(sum(df$signif_diff2)==0, ysignif <- 0.07, ysignif <- 0.14)
  g2 <- g2 + annotate("text", x = df$time[df$signif_diff3], 
                      y = ymax2-(ymax2-ymin2)*ysignif, label = "*", size = 3, color = "cyan3")
  
  return(list(g1, g2))
}





##### Visualization of SSM prediction (cyclic) #####
Pred_vis_cyclic <- function(df1, df2, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 20; seq1 <- seq(0,20,10)
    ymin3 <- -4; ymax3 <- 12; seq3 <- seq(-4,12,4) 
    # ymin4 <- -0.3; ymax4 <- 0.3; seq4 <- seq(-0.3,0.3,0.1)
    # ymin5 <- -0.005; ymax5 <- 0.005; seq5 <- seq(-0.005,0.005,0.001) 
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.6; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin3 <- -0.4; ymax3 <- 2.4; seq3 <- seq(0,2,1)
    # ymin4 <- -0.1; ymax4 <- 0.05; seq4 <- seq(-0.1,-0.05,0.05)
    # ymin5 <- -0.001; ymax5 <- 0.001; seq5 <- seq(-0.001,0.001,0.0005) 
    # ymin6 <- -0.2; ymax6 <- 0.3; seq6 <- seq(0,0.3,0.1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- -0.1; ymax1 <- 4.5; seq1 <- seq(0,4,2)
    ymin3 <- -0.2; ymax3 <- 1.8; seq3 <- seq(0,1.5,0.5)
    # ymin4 <- -0.02; ymax4 <- 0.08; seq4 <- seq(0,0.08,0.04)
    # ymin5 <- -0.001; ymax5 <- 0.001; seq5 <- seq(-0.001,0.001,0.0005) 
    # ymin6 <- -0.6; ymax6 <- 2.2; seq6 <- seq(-0.5,2,0.5)
  }
  
  
  g1 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarSun_2.5, ymax = alpha_MarSun_97.5), alpha = 0.3, fill = "chocolate") +
    geom_line(aes(y = alpha_MarSun_50), col = "chocolate") +
    geom_point(aes(y = data_MarSun), col = "chocolate", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepSun_2.5, ymax = alpha_SepSun_97.5), alpha = 0.3, fill = "dodgerblue4") +
    geom_line(aes(y = alpha_SepSun_50), col = "dodgerblue4") +
    geom_point(aes(y = data_SepSun), col = "dodgerblue4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6), limits = c(8,32)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    coord_cartesian(clip = 'off') +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0, 5, 0, 0),"mm")) +
    labs(title = bquote(paste(italic(.(var)), " transcript (sun)", sep="")),
         x = "Time relative to initial dawn (h)", 
         tag = "A")
  
  g2 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarShade_2.5, ymax = alpha_MarShade_97.5), alpha = 0.3, fill = "chocolate") +
    geom_line(aes(y = alpha_MarShade_50), col = "chocolate") +
    geom_point(aes(y = data_MarShade), col = "chocolate", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepShade_2.5, ymax = alpha_SepShade_97.5), alpha = 0.3, fill = "dodgerblue4") +
    geom_line(aes(y = alpha_SepShade_50), col = "dodgerblue4") +
    geom_point(aes(y = data_SepShade), col = "dodgerblue4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6), limits = c(8,32)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    coord_cartesian(clip = 'off') +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0),"mm")) +
    labs(title = bquote(paste(italic(.(var)), " transcript (shade)", sep="")),
         x = "Time relative to initial dawn (h)", 
         tag = "B")
  
  g3 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin3, ymax = ymax3, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = mu_2.5, ymax = mu_97.5), alpha = 0.5) +
    geom_line(aes(y = mu_50)) +
    # geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6), limits = c(8,32)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq3) +
    coord_cartesian(clip = 'off') +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 7),
          plot.margin = unit(c(0, 0, 0, 0),"mm")) +
    labs(x = "Time relative to initial dawn (h)", 
         title = "Circadian trend",
         tag = "C")
  
  
  # ast_pos <- which(df$`97.5%` < 0 | df$`2.5%` > 0)
  # df$ast[ast_pos] <- "*"
  # df$ast[-ast_pos] <- ""
  # df$ast_y[df$par=="Bernoulli"] <- max(df$`97.5%`[df$par=="Bernoulli"])*1.1
  # df$ast_y[df$par=="Gamma"] <- max(df$`97.5%`[df$par=="Gamma"])*1.1
  
  df2_1 <- df2[1,]
  
  g4 <- ggplot(data = df2_1, aes(x=var, y=`50%`)) +
    geom_hline(yintercept = 0, col='gray50', linetype='dashed') +
    geom_pointrange(aes(ymin=`2.5%`, ymax=`97.5%`), 
                    color='grey5', fill='grey95', size=0.3, fatten = 0.2) +
    scale_x_discrete(labels = "Temperature") +
    #geom_text(aes(label = `50%_round`), size = 7/ggplot2::.pt, vjust = -1) +
    #geom_text(data=df, aes(x=var, y=`50%`, label=ast), size=7/ggplot2::.pt, vjust = -0.1) +
    #coord_flip() +
    #facet_wrap(~ var, scales = "free_y") +
    #coord_cartesian(ylim=ylim) +
    theme_classic(base_size = 7) +
    theme(axis.title=element_blank(), 
          axis.text=element_text(size=7),
          plot.margin = unit(c(0, 3, 0, 0),"mm"))
  
  df2_2 <- df2[2,]
  g5 <- ggplot(data = df2_2, aes(x=var, y=`50%`)) +
    geom_hline(yintercept = 0, col='gray50', linetype='dashed') +
    geom_pointrange(aes(ymin=`2.5%`, ymax=`97.5%`), 
                    color='grey5', fill='grey95', size=0.3, fatten = 0.2) +
    scale_x_discrete(labels = "Irradiance") +
    #geom_text(aes(label = `50%_round`), size = 7/ggplot2::.pt, vjust = -1) +
    #geom_text(data=df, aes(x=var, y=`50%`, label=ast), size=7/ggplot2::.pt, vjust = -0.1) +
    #coord_flip() +
    #facet_wrap(~ var, scales = "free_y") +
    #coord_cartesian(ylim=ylim) +
    theme_classic(base_size = 7) +
    theme(axis.title=element_blank(), 
          axis.text=element_text(size=7),
          plot.margin = unit(c(0, 3, 0, 0),"mm"))
  
  g_coef <- ggplot(data = df2_2, aes(x=var, y=`50%`)) + theme_void(base_size = 7) +
    #geom_text(aes(label = "Regression coefficients"), size = 7/ggplot2::.pt, vjust = -0.1, hjust = 0.5) +
    theme(plot.title = element_text(size = 7, vjust = 3),
          plot.tag = element_text(face = "bold", size = 10),
          plot.margin = unit(c(0, 0, 0, 0),"mm")) +
    labs(title = "Regression coefficients")
    
  if(var=="AhgCCA1"){
    df2_1 <- df2[1,]
    g6 <- ggplot(data = df2_1, aes(x=var, y=`50%`)) +
      theme_void(base_size = 7) +
      theme(axis.title=element_blank(), 
            axis.text=element_blank(),
            plot.margin = unit(c(0, 3, 0, 0),"mm"))
  }
  
  if(var=="AhgSIG5"){
    df2_3 <- df2[3,]
    g6 <- ggplot(data = df2_3, aes(x=var, y=`50%`)) +
      geom_hline(yintercept = 0, col='gray50', linetype='dashed') +
      geom_pointrange(aes(ymin=`2.5%`, ymax=`97.5%`), 
                      color='grey5', fill='grey95', size=0.3, fatten = 0.2) +
      scale_x_discrete(labels = expression(italic("AhgCCA1"))) +
      #geom_text(aes(label = `50%_round`), size = 7/ggplot2::.pt, vjust = -1) +
      #geom_text(data=df, aes(x=var, y=`50%`, label=ast), size=7/ggplot2::.pt, vjust = -0.1) +
      #coord_flip() +
      #facet_wrap(~ var, scales = "free_y") +
      #coord_cartesian(ylim=ylim) +
      theme_classic(base_size = 7) +
      theme(axis.title=element_blank(), 
            axis.text=element_text(size=7),
            plot.margin = unit(c(0, 3, 0, 0),"mm"))
  }
    
    if(var=="AhgpsbD BLRP"){
    df2_3 <- df2[3,]
    g6 <- ggplot(data = df2_3, aes(x=var, y=`50%`)) +
      geom_hline(yintercept = 0, col='gray50', linetype='dashed') +
      geom_pointrange(aes(ymin=`2.5%`, ymax=`97.5%`), 
                      color='grey5', fill='grey95', size=0.3, fatten = 0.2) +
      scale_x_discrete(labels = expression(italic("AhgSIG5"))) +
      #geom_text(aes(label = `50%_round`), size = 7/ggplot2::.pt, vjust = -1) +
      #geom_text(data=df, aes(x=var, y=`50%`, label=ast), size=7/ggplot2::.pt, vjust = -0.1) +
      #coord_flip() +
      #facet_wrap(~ var, scales = "free_y") +
      #coord_cartesian(ylim=ylim) +
      theme_classic(base_size = 7) +
      theme(axis.title=element_blank(), 
            axis.text=element_text(size=7),
            plot.margin = unit(c(0, 3, 0, 0),"mm"))
    }
    
    return(list(g1, g2, g3, g4, g5, g6, g_coef))
  
}





##### Visualization of SSM prediction (cyclic) with constant input #####
Pred_vis_cyclic_constant <- function(df, var, cond){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 20; seq1 <- seq(0,20,10)
  }else if(var=="AhgSIG5"){
    ymin1 <- -1; ymax1 <- 3.1; seq1 <- seq(0,3,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- -0.3; ymax1 <- 3; seq1 <- seq(0,3,1)
  }
  
  
  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarSun_2.5, ymax = alpha_MarSun_97.5), alpha = 0.3, fill = "chocolate") +
    geom_line(aes(y = alpha_MarSun_50), col = "chocolate") +
    geom_ribbon(aes(ymin = alpha_SepSun_2.5, ymax = alpha_SepSun_97.5), alpha = 0.3, fill = "dodgerblue4") +
    geom_line(aes(y = alpha_SepSun_50), col = "dodgerblue4") +
    scale_x_continuous(breaks=seq(12,30,6), limits = c(8,32)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    coord_cartesian(clip = 'off') +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0, 5, 0, 0),"mm")) +
    labs(title = bquote(paste(italic(.(var)), " (sun, ", .(cond), ")", sep="")),
         x = "Time relative to initial dawn (h)", 
         tag = "A")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarShade_2.5, ymax = alpha_MarShade_97.5), alpha = 0.3, fill = "chocolate") +
    geom_line(aes(y = alpha_MarShade_50), col = "chocolate") +
    geom_ribbon(aes(ymin = alpha_SepShade_2.5, ymax = alpha_SepShade_97.5), alpha = 0.3, fill = "dodgerblue4") +
    geom_line(aes(y = alpha_SepShade_50), col = "dodgerblue4") +
    scale_x_continuous(breaks=seq(12,30,6), limits = c(8,32)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    coord_cartesian(clip = 'off') +
    theme_classic(base_size = 7) +
    theme(legend.position = "none",
          plot.title = element_text(size = 7),
          plot.tag = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0),"mm")) +
    labs(title = bquote(paste(italic(.(var)), " (shade, ", .(cond), ")", sep="")),
         x = "Time relative to initial dawn (h)", 
         tag = "B")
  
  return(list(g1, g2))
  
}





##### Figure legend #####

### Sun-Shade
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Condition=c(rep("Sun",5), rep("Shade",5)))
df_legend$Condition = factor(df_legend$Condition, levels=c("Sun", "Shade"))

# plot code
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("orange", "gray30")) +
  scale_fill_manual(values=c("orange", "#gray30"))

legend_SunShade1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_SunShade2 <- as_ggplot(get_legend(g))



### Mar-Sep
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Season=c(rep("March (Spring)",5), rep("September (Autumn)",5)))
df_legend$Season = factor(df_legend$Season, levels=c("March (Spring)", "September (Autumn)"))

# For STM
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Season, color=Season)) + 
  geom_line(aes(color=Season), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("#FF1493", "#522A17")) +
  scale_fill_manual(values=c("#FF1493", "#522A17"))

legend_MarSep1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_MarSep2 <- as_ggplot(get_legend(g))

# For DLM
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Season, color=Season)) + 
  geom_line(aes(color=Season), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("chocolate", "dodgerblue4")) +
  scale_fill_manual(values=c("chocolate", "dodgerblue4"))

legend_MarSep1_dlm <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_MarSep2_dlm <- as_ggplot(get_legend(g))



### Local
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Condition=c(rep("Ambient",2), rep("Warm",2), rep("Cool",2), rep("Low light",4)))
df_legend$Condition = factor(df_legend$Condition, levels=c("Ambient", "Warm", "Cool", "Low light"))
df_legend <- df_legend[df_legend$Condition!=unique(df_legend$Condition)[4],]

# plot code
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("black", "orangered", "cyan3")) +
  scale_fill_manual(values=c("black", "orangered", "cyan3"))

legend_local1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_local2 <- as_ggplot(get_legend(g))

