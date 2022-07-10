library(pracma)
library(ggplot2)
library(dplyr)

get_fao = function(met1 = c(352.1, 298.7, 390.6),
                   met5 = c(171.6, 358.8, 400.6),
                   calcaneus = c(314.0, 707.2, 386.4),
                   talus = c(311.0, 616.3, 289.0),
                   parallel_to_foot = TRUE){
  get_mode = function(x){
    sum(x^2)^0.5
  }
  
  get_distance = function(x, y){
    distance = ((x[1]-y[1])^2+(x[2]-y[2])^2+(x[3]-y[3])^2)^0.5
    return(distance)
  }
  
  middle_point = (met1+met5)/2
  
  plane = cross((met5-met1),(calcaneus-met1))
  intersection = -plane[1]*calcaneus[1]-plane[2]*calcaneus[2]-plane[3]*calcaneus[3]
  
  if(parallel_to_foot){
    projection = solve(
      rbind(
        c(1,0,0,-plane[1]),
        c(0,1,0,-plane[2]),
        c(0,0,1,-plane[3]),
        c(plane[1],plane[2],plane[3],0)
      ),
      c(talus[1],talus[2],talus[2],-intersection)
    )[1:3]
  }else{
    z = (-intersection-plane[1]*talus[1]-plane[2]*talus[2])/plane[3]
    projection = c(talus[1], talus[2], z)
  }
  
  projection_midline_distance = get_mode(cross((calcaneus-projection), (middle_point - calcaneus)))/
    get_mode(middle_point - calcaneus)
  base = get_distance(middle_point, calcaneus)
  fao = projection_midline_distance/base*100
  
  return(fao)
  
}

### NOT RUN
get_fao(met1 = c(352.1, 298.7, 390.6),
        met5 = c(171.6, 358.8, 400.6),
        calcaneus = c(314.0, 707.2, 386.4),
        talus = c(311.0, 616.3, 289.0),
        parallel_to_foot = FALSE)







