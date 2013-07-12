plotdata = function(data, input) {
  filter = NA
  filter = data$exposure %in% input$exposure  
  filter = filter & data$precontrol %in% input$precontrol
  filter = filter & data$IVMset %in% input$IVMset 
  filter = filter & data$coverage %in% input$coverage
  
  pastrounds = input$pastrounds
  dataframe_pastrounds= data.frame()
  
  filter_pastrounds = filter & data$futurerounds == 0
  filter_pastrounds = filter_pastrounds & data$pastrounds %in% seq(0, pastrounds, 1)
  filter_pastrounds = filter_pastrounds & data$treatment_interval == "Future annual treatment"

  dataframe_pastrounds = subset(data, filter_pastrounds)
  dataframe_pastrounds$treatment_interval = "Past rounds"
  
  pastrounds = input$pastrounds
  filter_futurerounds = filter & data$pastrounds == pastrounds
  filter_futurerounds = filter_futurerounds & data$treatment_interval %in% input$treatment_interval
  dataframe_futurerounds = subset(data, filter_futurerounds)
  
  plotdata = rbind(dataframe_pastrounds, dataframe_futurerounds)
}

interpolation_records <- function(data, interval) {
  data = subset(data, data$treatment_interval %in% c(paste(c("Future", interval, "treatment"), collapse=' '), "Past rounds"))
  exact = subset(data, data$elimination_probability == 0.99)
  result = data.frame()
  # if there is no exact match, interpolate one!
  if (nrow(exact) == 0) {
    fewer = subset(data, data$elimination_probability < 0.99)
    more = subset(data, data$elimination_probability > 0.99)
    
    # no x1y1 and/or x2y2 data available. return.
    if ((nrow(fewer) == 0) | (nrow(more) == 0)) {
      return(NA)
    } 
    
    # get closest P(elim)
    fewer = subset(fewer, fewer$elimination_probability == max(fewer$elimination_probability))
    more = subset(more, more$elimination_probability == min(more$elimination_probability))
    
    # next, get closest future rounds (sometimes we have more results so 
    # lets use futurerounds to get only the closest one)
    # -
    # also, use [1, ] for selecting the first record available after that, since there
    # might be 2 records when future and past round both yield a result. 
    # It doesn't matter which one is taken so take [1, ] as here there is at least 1 result
    fewer = subset(fewer, fewer$futurerounds == max(fewer$futurerounds))[1, ]
    more = subset(more, more$futurerounds == min(more$futurerounds))[1, ]
    
    result = rbind(result, fewer)
    result = rbind(result, more)
  }
  else {
    # exact match, get the x!
    result = rbind(result, exact)
  }
  
  result
}

interpolate_barplot <- function(data, interval) {
  r = interpolation_records(data, interval)
  if (class(r) != 'data.frame') {
    return(NA)
  }
  
  sx = NA
  if (nrow(r) == 2) {
    dx = r[2, ]$x - r[1, ]$x
    dy = (r[2, ]$elimination_probability * 100) - (r[1, ]$elimination_probability * 100)
    sy = 99
    sx = r[1, ]$x + ((dx/dy) * (sy - (r[1, ]$elimination_probability * 100)))
    
    sx = sx / 4 - r[2, ]$pastrounds
  }
  else {
    sx = r[1, ]$x / 4 - r[1, ]$pastrounds
  }
  round(sx, digits=3)
}

interpolate_plot_vline <- function(data, interval) {
  r = interpolation_records(data, interval)
  
  if (class(r) != 'data.frame') {
    return(NA)
  }
  
  sx = NA
  if (nrow(r) == 2) {
    dx = r[2, ]$x - r[1, ]$x
    dy = (r[2, ]$elimination_probability * 100) - (r[1, ]$elimination_probability * 100)
    sy = 99
    sx = r[1, ]$x + ((dx/dy) * (sy - r[1, ]$elimination_probability * 100))
  }
  else {
    sx = r[1, ]$x
  }
  sx
}

generatePlot = function(data, input) {
  plot = NA
  if(length(input$treatment_interval) != 0){
    filtered = plotdata(data, input)
    source("lib//calculate_coords.R")
  
    finaldata = calculate_coords(filtered, input$pastrounds)
    lim_x=max(finaldata$x)
    lim_y=max(finaldata$y)
    begin_x = 2013 - max(finaldata$pastrounds)
    end_x = 2013 + max(finaldata$futurerounds)
    begin_y = 0
    end_y = 100
    
    treatment_intervals = levels(factor(finaldata$treatment_interval))
    
    annual    = interpolate_plot_vline(finaldata, "annual")
    semi      = interpolate_plot_vline(finaldata, "semiannual")
    quarterly = interpolate_plot_vline(finaldata, "quarterly")
    
    elim_exp = c(quarterly, semi, annual) 
		 elim_exp_line = c("quarterly elimination", "semi elimination", "annual elimination")
    # used to remove vlines that are not in checked treatments
    eqnames = c("Future quarterly treatment", "Future semiannual treatment", "Future annual treatment")
		
		vlines = data.frame(xint=elim_exp, grp=elim_exp_line, eqname=eqnames)
    # not all scenario's reach P(elim) = 0.99. Remove the NA warning
    # dont show P(elim) vlines for unchecked treatments (occurs when using pastrounds for interpolation)
    vlines = vlines[!is.na(vlines$xint) & vlines$eqname %in% treatment_intervals[treatment_intervals != "Past rounds"], ]
    
    hlines = data.frame(yint=c(
      lim_y/end_y * 99, 
      lim_y/end_y * 95, 
      lim_y/end_y * 90)
    )
    
    # fill with NA's for equal length (redmine issue #25)
    lim_x_seq = seq(0, lim_x, 8)
    end_x_seq = seq(begin_x, end_x, 2)
    lim_x_seq = c(lim_x_seq, rep.int(NA, length(end_x_seq) - length(lim_x_seq)))
    
    plot = ggplot(finaldata, aes(x=x, y=y, colour=treatment_interval)) + geom_line() +
        scale_x_discrete(breaks=lim_x_seq, labels=end_x_seq) +
        scale_y_discrete(breaks=seq(0 , lim_y, lim_y/end_y*5), labels=seq(begin_y, end_y, 5)) +
        xlab("Years") + ylab("Elimination probability") + 
        geom_hline(data=hlines, aes(yintercept=yint), colour="darkgray", linetype="dashed", size=0.4)
                
    if (nrow(vlines) > 0) {
      plot = plot + geom_vline(data=vlines, aes(xintercept=xint, colour=grp), linetype="longdash", size=0.4)
    }
  }
  
  plot
}

generateBarplot = function(data, input) {
  plot = NA
  if(length(input$treatment_interval) != 0){
    filtered = plotdata(data, input)
    source("lib//calculate_coords.R")
    
    finaldata = calculate_coords(filtered, input$pastrounds)
    
    plot = c(interpolate_barplot(finaldata, "quarterly"),
    interpolate_barplot(finaldata, "semiannual"),
    interpolate_barplot(finaldata, "annual"))
    
    plot = data.frame(duration=plot, treatment_interval=c("quarterly", "semiannualy", "annualy"))
		plot$treatment_interval <- factor(plot$treatment_interval,levels=plot$treatment_interval) # use factor to keep the same order
    plot = plot[!is.na(plot$duration), ]
		
    plot = ggplot(data=plot, aes(x=reorder(treatment_interval, duration), y=duration, fill=treatment_interval, ymin=-1, ymax=max(duration)+1)) + 
    # plot = ggplot(data=plot, aes(x=treatment_interval, y=duration, order=NA, fill=treatment_interval, ymin=-1, ymax=max(duration)+1)) + 
    geom_bar(width=0.5, stat="identity") + 
    xlab("Treatment Interval") + 
    ylab("Duration (Years)") +
    theme(legend.position="") +
    # coord_flip() +
    geom_text(aes(label=duration), color="black", vjust=-0.50)
  }
  plot
}