

temp = Imputed_df[-c(1,6,7,8,12)]
temp = scale(temp)
temp = cbind(temp, Imputed_df[c(6:8,12)])

   

repeated_kmeans = function(dataset){ 
  metrics_df= data.frame()
  accuracy_vector = vector()
  ratio = vector()
  species_no = vector()
  for (i in 1:10){
    kmeans_result = kmeans(dataset[-1], 7, 20)
    ratio[i] = round(kmeans_result$tot.withinss/kmeans_result$totss, digits = 2)
    kmeans_conf = table(Imputed_df$Species, kmeans_result$cluster)
    accuracy_vector[i] = accuracy(kmeans_conf)
    species_no[i] = kmeans_conf[1,1]
  }
  metrics_df = cbind(ratio, accuracy_vector, species_no)
  return(metrics_df)
}

accs = repeated_kmeans(Imputed_df)
