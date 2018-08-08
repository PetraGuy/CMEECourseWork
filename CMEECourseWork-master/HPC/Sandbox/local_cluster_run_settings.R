# if statements for local running
for (iter in 1:4) {
  set.seed(iter)
  outfile = paste("pg5117_cluster_", iter, ".rda", sep = "")
  
  if (iter < 2) {
    cluster_run(
      speciation_rate = 0.002125,
      size = 10,
      wall_time = 180,
      interval_rich = 1,
      interval_oct = 1,
      burn_in_generation = 10,
      output_file_name = outfile
    )
  }
  
  if ((1 < iter)  &&  (iter < 3)) {
    cluster_run(
      speciation_rate = 0.002125,
      size = 10,
      wall_time = 180,
      interval_rich = 1,
      interval_oct = 1,
      burn_in_generation = 10,
      output_file_name = outfile
    )
  }
  
  if ((2 < iter) && (iter < 4)) {
    cluster_run(
      speciation_rate = 0.002125,
      size = 10,
      wall_time = 180,
      interval_rich = 1,
      interval_oct = 1,
      burn_in_generation = 10,
      output_file_name = outfile
    )
  }
  if ((3 < iter) && (iter < 5)) {
    cluster_run(
      speciation_rate = 0.002125,
      size = 10,
      wall_time = 180,
      interval_rich = 1,
      interval_oct = 1,
      burn_in_generation = 10,
      output_file_name = outfile
    )
  }
}
