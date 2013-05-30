# filter data on "annual, semi, quarterly"
myline=function(wormdata,interval){
  coverage = wormdata$coverage == "Coverage 50% (past) & 50% (future)"
  #cat(sprintf("<set name=\"%s\" value=\"%f\" ></set>\n", df$timeStamp, df$Price))
  treatment = wormdata$inputfile == sprintf("Future %s treatment", interval)
  exposure = wormdata$alpha == "Variance in fly bite exposure 0.29"
  past = wormdata$pastrounds == 0
  future = wormdata$futurerounds == 20
  rbr = wormdata$rbr == "Pre-control CMFL 5 mf/ss"
  set = wormdata$IVMset == "IVM assumption set 1"
  
  filter= coverage & treatment & exposure & rbr & set & past
  
  xd = wormdata$futurerounds[filter]
  yd = sort(wormdata$p_elim[filter], decreasing=FALSE)
  
  skip = 1
  if(interval == "annual"){
    skip = 4
  }else if(interval == "semiannual"){
    skip = 2
  }else if(interval == "quarterly"){
    skip = 1
  }
  
  xd = skip * which(xd==xd) - 1
  yd = 80 * yd
  
  list(xd=xd,yd=yd)
}

# get data
wormdata = read.csv("wormsimtotal.txt", sep=" ")

x = 0:80
y = 0:80

dataannual = myline(wormdata, "annual")
annualline = smooth.spline(dataannual$xd, dataannual$yd, spar=0.001)
datasemiannual = myline(wormdata, "semiannual")
semiannualline = smooth.spline(datasemiannual$xd, datasemiannual$yd, spar=0.001)
dataquarterly = myline(wormdata, "quarterly")
quarterlyline = smooth.spline(dataquarterly$xd, dataquarterly$yd, spar=0.001)


plot(x,y, type='n', xaxt='n', yaxt='n', xlab="jaar", ylab="Eliminatie %")

lines(annualline, col = "red")
lines(semiannualline, col = "orange")
lines(quarterlyline, col = "green")

axis(1, at=seq(0, 80, 4), labels=seq(2012, 2032, 1))
axis(2, at=seq(0,80,8), labels=seq(0,100,10))