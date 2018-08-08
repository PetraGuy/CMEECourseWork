

repeated_hclust = function(dataset){
  conf = list()
  metrics_list = list()
  accuracy_vector = vector()
  clust_methods = c("euclidean", "maximum","manhattan","canberra","minkowski")
for (i in 1:length(clust_methods)){
  method = clust_methods[i]
  distance = dist(dataset[-1], method = method)
  clusters = hclust(distance, method = "complete")
  members = cutree(clusters, k = 7)
  conf[[i]] = table(dataset$Species, members)
  accuracy_vector[i] = accuracy(conf[[i]])
}
  accuracy_df = rbind(clust_methods, accuracy_vector)
  metrics_list[[1]]=accuracy_df
  metrics_list[[2]]=conf
  names(metrics_list)=c("Accuracy", "Confusion Matrix")
  return(metrics_list)

}

clust_methods = c("euclidean", "maximum","manhattan","canberra","minkowski")


# repeated_hclust = function(dataset,method){ 
#   browser()
#   metrics_list = list()
#   accuracy_vector = vector()
#   ratio = vector()
#   species_no = data.frame(matrix(ncol = 7))
#   colnames(species_no) = speciesnames
#   for (i in 1:10){
#     hclust_conf = hierachical_cluster(dataset, method)
#     accuracy_vector[i] = accuracy(hclust_conf)
#     species = diag(hclust_conf)
#     species_no = rbind(species_no, species)
#   }
#   metrics_list[[1]] = accuracy_vector
#   metrics_list[[2]] = species_no[-1,]
#   return(metrics_list)
# }



Imputed_sets = create_train_test(Imputed_df)
Imputed_train = Imputed_sets[[1]]
Imputed_test = Imputed_sets[[2]]
tree = rpart(Species~., Imputed_train, method = "class", control = rpart.control(cp = 0.00001))
rpart.plot(tree, box.palette = "Blues")
pred_tree = predict(tree, Imputed_test, type = "class")
confusion_tree = table(Imputed_test$Species, pred)
accuracy(table(Imputed_test$Species, pred))
sensitivity(table(Imputed_test$Species, pred))
precision(table(Imputed_test$Species, pred))
cbind(table(Imputed_test$Species, pred),summary(Imputed_test$Species))



