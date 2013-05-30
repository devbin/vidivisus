getXSkip = function(x, pastrounds)
{
  interval = x[['treatment_interval']]
  data_pastrounds = as.numeric(x[['pastrounds']])
  data_futurerounds = as.numeric(x[['futurerounds']])
  
  skip = 0
  if((data_pastrounds == pastrounds) && (data_futurerounds > 0)){
    if(interval == "Future annual treatment"){
      skip = 4
    }else if(interval == "Future semiannual treatment"){
      skip = 2
    }else if(interval == "Future quarterly treatment"){
      skip = 1
    }
  }
  skip
}

calculate_x = function(data, pastrounds)
{
  max_x = max(data$futurerounds) + max(data$pastrounds)
  
  data$x = (data$pastrounds * 4) + (data$futurerounds * apply(data, 1, getXSkip, pastrounds=pastrounds))
  data$y = (data$elimination_probability * max(data$x))
  data
} 