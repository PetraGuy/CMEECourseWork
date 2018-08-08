#!/usr/bin/env Rscript
#HPC Week

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy November 2017


rm(list = ls())
graphics.off()

#iter = as.numeric(Sys.getenv(("PBS_ARRAY_INDEX")))



initialise_min = function(x) {
  comm = rep(1, x)
  return(comm)
}

choose_two = function(x) {
  two = sample(x, 2)
  return(two)
}


neutral_step_speciation = function(x, v) {
  p = runif(1)
  if (v < p) {
    index = choose_two(length(x))
    x[index[1]] = x[index[2]]
  }
  else {
    newspecies =  max(x) + 1
    index = sample((length(x)), size = 1, replace = TRUE)
    x[index] = newspecies
  }
  return(x)
}

species_richness = function(x) {
  r = length(unique(x))
  return(r)
}

neutral_generation_speciation = function(x, v) {
  n =  round(length(x) / 2)
  for (i in 1:n) {
    x = neutral_step_speciation(x, v)
  }
  return(x)
}


species_abundance = function(x) {
  abundance = as.numeric(sort(table(x), decreasing = TRUE))
  return(abundance)
}

octaves = function(x) {
  oct = tabulate(floor(log2(x)) + 1)
  return(oct)
}

sum_vect = function(x, y) {
  if (length(x) < length(y))    {
    short = x
    long = y
    newshort = c(x, rep(0, length(long) - length(short)))
    sum = newshort + long
  }   else if (length(x) > length(y)) {
    short = y
    long = x
    newshort = c(y, rep(0, length(long) - length(short)))
    sum = newshort + long
  }   else  {
    sum = x + y
  }
  return(sum)
}




cluster_run = function(speciation_rate,
                       size,
                       wall_time,
                       interval_rich,
                       interval_oct,
                       burn_in_generation,
                       output_file_name) {
  # Organise times
  wall_time = wall_time*60
  start = as.numeric(proc.time()[3])
  
  # initialise community
  comm = initialise_min(size)
  rich = vector()
  n = 1
  index = 1
  octets = list()
  
  while (as.numeric(proc.time()[3]) - start < wall_time) {
    # update community
    comm = neutral_generation_speciation(comm, speciation_rate)
    # burn-in phase
    if (n < burn_in_generation) {
      # sample
      if (n %% interval_rich == 0) {
        # calculate richness and append
        rich = c(rich, species_richness(comm))
      }
    }
    # after burn-in
    else {
      abundance = species_abundance(comm)
      # sample
      if (n %% interval_oct == 0) {
        octets[[index]] = octaves(abundance)
        index = index + 1
      }
    }
    n = n + 1
    
  }
  run_time = (as.numeric(proc.time()[3]) - start)/60
  
  #find the average of the octaves which are list elements of octets
  len = length(octets)
  sum = vector()
  for (a in 1:len) {
    sum = sum_vect(sum, octets[[a]])
    
  }
  
  state = sum / len
  
  save(
    speciation_rate,
    size,
    interval_rich,
    interval_oct,
    burn_in_generation,
    run_time,
    rich,
    octets,
    state,
    file = output_file_name
  )
  
}


for (iter in 1:12){
set.seed(iter)
outfile = paste("../Results/pg5117_cluster_", iter, ".rda", sep = "")

if (iter < 4) {
  cluster_run(
    speciation_rate = 0.2125,
    size = 100,
    wall_time = 5,
    interval_rich = 5,
    interval_oct = 10,
    burn_in_generation = 10,
    output_file_name = outfile
  )
}

if ((4 < iter)  &&  (iter < 9)) {
  cluster_run(
    speciation_rate = 0.2125,
    size = 100,
    wall_time = 5,
    interval_rich = 5,
    interval_oct = 10,
    burn_in_generation = 10,
    output_file_name = outfile
  )
}

if ((9 < iter) && (iter < 13)) {
  cluster_run(
    speciation_rate = 0.2125,
    size = 100,
    wall_time = 5,
    interval_rich = 5,
    interval_oct = 10,
    burn_in_generation = 10,
    output_file_name = outfile
  )
}
if ((13 < iter) && (iter < 17)) {
  cluster_run(
    speciation_rate = 0.2125,
    size = 100,
    wall_time = 5,
    interval_rich = 1,
    interval_oct = 10,
    burn_in_generation = 10,
    output_file_name = outfile
  )
}
}