# A collection of functions for summarising the output of gpModel.R and arenaModel.R


# Create a data frame of service times based on GP model output
getServiceTime <- function(gpModelOutput){
  map_df(gpModelOutput, ~tibble(id = .$serviceTime$id,
                        station = .$serviceTime$station,
                        mins = .$serviceTime$mins), .id = "iter") 
}

# Create a data frame of queue times based on GP model output
getQueueTime <- function(gpModelOutput){
  map_df(gpModelOutput, ~tibble(id = .$qTime$id,
                        station = .$qTime$station,
                        mins = .$qTime$mins), .id = "iter") 
}


# Create a data frame of queue times based on Arena model output
getUtilTime <- function(arenaModelOutput){
  map_df(arenaModelOutput, ~tibble(station = .$qLengths$station,
                                   nServers = .$nServers$nServers,
                                   qLengths = .$qLengths$qLengths,
                                   util = .$util$util), .id = "iter") 
}



# Summarise GP model settings
plotGPSettings <- function(GPModelOutput){
  
  GPUtilTime <- getGPUtilTime(GPModelOutput)
  ServiceTime <- getServiceTime(GPModelOutput)
  
  fig1 <- GPUtilTime %>% 
    distinct(nServers) %>% 
    ggplot(aes(x = 'Vaccination', y = nServers)) + 
    geom_col() + 
    scale_y_continuous('Number of staff', limits = c(0, NA)) + 
    scale_x_discrete(NULL) 
  
  fig2 <- ServiceTime %>% 
    ggplot(aes(x = station, y = mins)) + 
    geom_boxplot() + 
    scale_y_continuous('Service time (minutes)', limits = c(0, NA)) + 
    scale_x_discrete(NULL) 
  
  fig3 <- GPUtilTime %>% 
    ggplot(aes(x = 'Vaccination', y = util)) + 
    annotate('rect', xmin=0, xmax = 2, ymin=0.70, ymax=0.80, fill = 'blue', alpha = 0.2) +
    geom_jitter(color = 'grey40', shape = 1) + 
    scale_y_continuous('Utilisation factor', limits = c(0,1)) + 
    scale_x_discrete(NULL)
  
  plot <- ggpubr::ggarrange(fig1, fig2, fig3, ncol = 3, labels = c('A', 'B', 'C'))
  
  return(plot)
  
}

# Summarise Arena model settings
plotArenaSettings <- function(arenaModelOutput){
  
  ArenaUtilTime <- getArenaUtilTime(arenaModelOutput)
  ServiceTime <- getServiceTime(arenaModelOutput)
  
  fig1 <- ArenaUtilTime %>% 
    distinct(station, nServers) %>% 
    ggplot(aes(x = station, y = nServers)) + 
    geom_col() + 
    scale_y_continuous('Number of staff', limits = c(0, NA)) + 
    scale_x_discrete(NULL) 
  
  fig2 <- ServiceTime %>% 
    filter(station != 'Observation') %>% 
    ggplot(aes(x = station, y = mins)) + 
    geom_boxplot() + 
    scale_y_continuous('Service time (minutes)', limits = c(0, NA)) + 
    scale_x_discrete(NULL) 
  
  fig3 <- ArenaUtilTime %>% 
    ggplot(aes(x = station, y = util)) + 
    annotate('rect', xmin=0, xmax = 5, ymin=0.70, ymax=0.80, fill = 'blue', alpha = 0.2) +
    geom_jitter(color = 'grey40', shape = 1) + 
    scale_y_continuous('Utilisation factor', limits = c(0,1)) + 
    scale_x_discrete(NULL)
  
  plot <- ggpubr::ggarrange(fig1, fig2, fig3, ncol = 1, labels = c('A', 'B', 'C'))
  
  return(plot)
  
}
