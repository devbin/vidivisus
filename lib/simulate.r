simulate = function(param1, param2, param3)
{
	.jinit("lib/")
    jOb <- .jnew("Simulator")
    
    #start simulation
    simulationData <- .jcall(jOb, "Ljava/lang/String;", "simulate", param1, param2, param3)
    
    #View(data.frame(simulationData))
    
    #output table/plot
    data <- read.csv(text=simulationData, header=TRUE, sep=" ")
}
