

set.seed(iter)

if (iter < 25) {
  cluster_run(
    speciation_rate = 0.002125,
    size = 500,
    wall_time = 3,
    interval_rich = 1,
    interval_oct = 50,
    burn_in_generation = 4500,
    output_file_name = outfile
  )
}

if ((26 < iter)  &&  (iter < 50)) {
  cluster_run(
    speciation_rate = 0.002125,
    size = 1000,
    wall_time = 3,
    interval_rich = 1,
    interval_oct = 100,
    burn_in_generation = 8000,
    output_file_name = outfile
  )
}

if ((51 < iter) && (iter < 75)) {
  cluster_run(
    speciation_rate = 0.002125,
    size = 2500,
    wall_time = 3,
    interval_rich = 1,
    interval_oct = 250,
    burn_in_generation = 20000,
    output_file_name = outfile
  )
}
if ((76 < iter) && (iter < 101)) {
  cluster_run(
    speciation_rate = 0.002125,
    size = 5000,
    wall_time = 3,
    interval_rich = 1,
    interval_oct = 500,
    burn_in_generation = 40000,
    output_file_name = outfile
  )
}
