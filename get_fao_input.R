get_fao_input = function(met1 = c(352.1, 298.7, 390.6),
                   met5 = c(171.6, 358.8, 400.6),
                   calcaneus = c(314.0, 707.2, 386.4),
                   talus = c(311.0, 616.3, 289.0),
                   parallel_to_foot = TRUE){
  
  return(
    
    tibble(Coord = c("X", "Y", "Z"),
           Met1 = met1,
           Met5 = met5,
           Calcaneus = calcaneus,
           Talus = talus
    )
    
  )
  
}