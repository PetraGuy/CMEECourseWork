#!/usr/bin/env Rscript
#HPC Week

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy November 2017

#### There are errors in this code - community calcualtions are in the wrong place, plus others, see
#### cluster_run for corrections

rm(list = ls())
graphics.off()

iter = as.numeric(Sys.getenv(("PBS_ARRAY_INDEX")))



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
  start = proc.time()[3]
  comm = initialise_min(size)
  rich = vector()
  n = 0
  octets = list()
  mydata = list()
  while (proc.time()[3] - start < wall_time) {
    if (n < burn_in_generation) {
      comm = neutral_generation_speciation(comm, speciation_rate)
      
      if (n %% interval_rich == 0) {
        rich = c(rich, species_richness(comm))
      }
    }
    if (n > burn_in_generation) {
      abundance = species_abundance(comm)
      
      if (n %% interval_oct == 0) {
        octets[[n]] = octaves(abundance)
      }
    }
    n = n + 1
    
  }
  run_time = (proc.time()[3] - start)
  
  save(
    speciation_rate,
    size,
    interval_rich,
    interval_oct,
    burn_in_generation,
    run_time,
    rich,
    octets,
    file = output_file_name
  )
  
}

outfile = paste("pg5117_cluster_", iter, ".rda", sep = "")

set.seed(iter)

if (iter < 25) {
  cluster_run(
    speciation_rate = 0.002125,
    size = 500,
    wall_time = 41000,
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
    wall_time = 41000,
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
    wall_time = 41000,
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
    wall_time = 41000,
    interval_rich = 1,
    interval_oct = 500,
    burn_in_generation = 40000,
    output_file_name = outfile
  )
}
