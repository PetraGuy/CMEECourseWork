question_12b = function() {
  t = 200
  
  v = 0.1
  comm_max = initialise_max(100)
  comm_min = initialise_min(100)
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  y1 =  cbind(rich_max, rich_min)
  
 
  v = 0.9
  comm_max = initialise_max(100)
  comm_min = initialise_min(100)
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  y2 =  cbind(rich_max, rich_min)
  

  v = 0.1
  comm_max = initialise_max(500)
  comm_min = initialise_min(500)
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  y3 =  cbind(rich_max, rich_min)
  
 
  v = 0.1
  comm_max = initialise_max(1000)
  comm_min = initialise_min(1000)
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  y4 =  cbind(rich_max, rich_min)
  
  x = (1:t)
  par(mfrow=c(2,2))
  
  matplot (x, y1, pch = 19, col = 1:2, main = "v = 0.1, size = 100")
  
  matplot (x, y2, pch = 19, col = 1:2, main = "v = 0.9, size = 100")
 
  matplot (x, y3, pch = 19, col = 1:2, main = "v = 0.1, size = 500")
  
  matplot (x, y4, pch = 19, col = 1:2, main = "v = 0.1, size = 1000")
  
  
}
