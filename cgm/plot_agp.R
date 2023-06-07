plot_agp <- function(cgm_df,
                     ts_freq = (24*60/5),
                     low_range_cutoff = 70,
                     high_range_cutoff = 180,
                     hypo_cutoff = 54,
                     hyper_cutoff = 250,
                     title,
                     x_limits = NULL,
                     y_label = "Sensor Glucose (15-min readings)"){
  
  cgm_df <- cgm_df %>% 
    dplyr::select(timestamp,sensorglucose)
  print(title)
  if(is.null(x_limits)){
    
    x_limits = c(min(cgm_df$timestamp,na.rm=TRUE),
                 max(cgm_df$timestamp,na.rm=TRUE))
  }
  
  p <- cgm_df %>% 
    
   
    ggplot(data=.,aes(x=timestamp,
                      y=sensorglucose)) +
    geom_path() +
    ggtitle(paste0("Patient: ",title)) +
    scale_y_continuous(limits = c(0,300),breaks=seq(0,300,by=50))
  
  
  p <- p +  theme_bw() +
    xlab("Timestamp") +
    ylab(y_label) +
    scale_x_datetime(date_labels = "%d-%b (%H:%M)",limits = x_limits) + 
    annotate("rect", xmin = min(cgm_df$timestamp,na.rm=TRUE), xmax = max(cgm_df$timestamp,na.rm=TRUE), 
             ymin = low_range_cutoff, ymax = high_range_cutoff, 
             fill = "lightgreen", alpha = 0.1, color = "darkgreen") +
    geom_hline(yintercept = c(hypo_cutoff,hyper_cutoff),
               color="orange") 
  
  if(sum(cgm_df$sensorglucose>hyper_cutoff,na.rm=TRUE) > 0){
    p <- p + geom_ribbon(aes(ymin = hyper_cutoff, 
                             ymax = ifelse(sensorglucose>hyper_cutoff,
                                           sensorglucose,
                                           NA)), 
                         fill = "red", alpha = .5, color = "orange")
    
  }
  
  if(sum(cgm_df$sensorglucose < hypo_cutoff,na.rm=TRUE) > 0){
    p <- p + geom_ribbon(aes(ymax = hypo_cutoff, 
                             ymin = ifelse(sensorglucose < hypo_cutoff,
                                           sensorglucose,
                                           NA)), 
                         fill = "red", alpha = .5, color = "orange")
    
  }
  
  
  
  
  
  return(p)
  
  
}