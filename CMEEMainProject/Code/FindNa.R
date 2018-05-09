nacols = function(df){
  colnames(df)[unlist( lapply(df, function(x) anyNA(x) )) ]
}
