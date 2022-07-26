
#basketballShotData <- read.csv(file =  "C:\\Users\\sam.williams\\OneDrive - Scientific Games Corporation\\Documents\\R\\Basketball Shots\\BasketballxP\\all_shots1520.csv")
#basketballShotData$player.loc_x_adjusted <- basketballShotData$player.loc_x + 25

library(ggplot2)
library(nbastatR)        

#Setting the M*N grid that describes the football pitch
M <- 50 #width
N <- 94 #length

#Pitch length and width retrived from 
#https://github.com/ML-KULeuven/socceraction/blob/master/socceraction/spadl/config.py

get_cell_indexes <- function(x, y, l, w)
{
  
  xmin = 0
  ymin = 0
  
  xi = (x - xmin) / 50 * l
  yj = (y - ymin) / 94 * w
  xi <- ceiling(pmax(pmin(xi, l), 1))
  yj <- ceiling(pmax(pmin(yj, w), 1))
  
  return(list(xi, yj))
}

get_flat_indexes <- function(x,y,l,w)
{
  output <- c()
  indexes <- get_cell_indexes(x, y, l, w)
  
  for(i in 1:length(indexes[[1]]))
  {
    output <- append(output, w - indexes[[2]][i] + 1 + (12 * (indexes[[1]][i] - 1)))
  }
  
  return(output)
}


count <- function(x,y,l,w)
{
  x = x[is.na(x) == FALSE & is.na(y) == FALSE]
  y = y[is.na(x) == FALSE & is.na(y) == FALSE]
  
  flat_indexes = get_flat_indexes(x, y, l, w)
  vn <- table(flat_indexes)
  
  #Creating and filling  the vector of occurrences
  vector <- rep(0, l*w)
  
  for(i in 1:(l*w))
  {
    if(as.character(i) %in% names(vn))
    {
      vector[i] <- vn[[as.character(i)]]
    }
  }
  
  
  return(matrix(data = vector, nrow = w, ncol = l))
}

safe_divide <- function(a,b)
{
  output <- a/b
  output[which(is.nan(output))] <- 0
  return(output)
}

scoring_prob <- function(actions, l, w)
{
  shot_actions = actions
  shot_makes = shot_actions[which(shot_actions$player.shot_made_numeric == 1),]
  
  shotmatrix = count(shot_actions$player.loc_x_adjusted, shot_actions$player.loc_y, l, w)
  makesmatrix = count(shot_makes$player.loc_x_adjusted, shot_makes$player.loc_y, l, w)
  
  return(safe_divide(makesmatrix, shotmatrix))
}
test_matrix <- scoring_prob(basketballShotData, M, N)

test_matrix



