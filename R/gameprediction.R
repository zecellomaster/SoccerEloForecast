#Name: gameprediction.R
#Author: zecellomaster
#Date: 03/17/21 (v1.0)
#' Simulates match results or calculates an array of win%/loss%/draw%
#' of the higher Elo team (Team A) depending on what the user wants
#'
#' @import stats
#'
#' @param function_mode String or int; which mode should the function run: "chances"
#' or the number of matches to be simulated (Default = "chances": 15,000 simulations)
#'
#' @param elos List; current Elo ratings of Teams A and B respectively
#'
#' @param home List of booleans; Whether or not either team is at home
#'
#' @param prev_matches List of Dataframe; contains previous matches, Elo ratings,
#' and (if available) match weights of Teams A and B respectively.
#' Dataframe columns: team_elo, opp_elo, team_score, opp_score, team_home, team_away,
#' match_weight
#'
#' @return Win%/Draw%/Loss% of Team A or Simulated Match Results
#'
#' @export

gameprediction <- function(function_mode = "chances", elos, home, prev_matches){


  if (is.numeric(function_mode) == TRUE){
      num_sims <- function_mode
  }else if (function_mode == "chances"){
      num_sims <- 15000
      a_win <- 0
      b_win <- 0
  }else{
    stop("Error. Please enter the number of game simulations you would like to
         perform or \"chances\" to caluclate the odds of Team A's results")
  }

  reg_data <- poissonregression(elos,home,prev_matches)

  #Here, the function returns lambda_a and b_reg. lambda_b will depend on lambda_a via
  #b_reg
  if (reg_data[[1]]== 1){

    lambda_a <- reg_data[[2]]
    goals <- data.frame(a_goals = rpois(num_sims, lambda_a))
    b_reg <- reg_data[[3]]

    for (i in 1:num_sims){

      lambda_b <- exp(coef(b_reg)["(Intercept)"] + (coef(b_reg)["opp_elo"]*elos[2]) +
        coef(b_reg)["opp_score"]*goals[i,"a_goals"])
      goals[i,"b_goals"] <- rpois(1,lambda_b)

      if(function_mode == "chances"){
        if(goals[i,"a_goals"] > goals[i,"b_goals"]){
          a_win = a_win + 1
        }else if(goals[i,"a_goals"] < goals[i,"b_goals"]){
          b_win = b_win + 1
        }
      }
    }

  } else if (reg_data[[1]] == 2){

    lambda_a <- reg_data[[2]]
    lambda_b <- reg_data[[3]]
    goals <- data.frame(a_goals = rpois(num_sims,lambda_a),
                        b_goals = rpois(num_sims,lambda_b))

    for(i in 1:dim(goals)[1]){
      if(function_mode == "chances"){
        if (goals[i,"a_goals"] > goals[i,"b_goals"]){
          a_win <- a_win + 1
        }else if (goals[i,"a_goals"] < goals[i,"b_goals"]){
          b_win <- b_win + 1
        }
      }
    }

  } else if (reg_data[[1]] == 3){
    lambda_a <- reg_data[[2]]
    goals <- data.frame(a_goals = rpois(num_sims, lambda_a))
    lambda_b1 <- reg_data[[3]]
    b_reg <- reg_data[[4]]
    l_weight <- reg_data[[5]]

    for (i in 1:num_sims){

      lambda_b2 <- exp(coef(b_reg)["(Intercept)"] + (coef(b_reg)["opp_elo"]*elos[2]) +
        coef(b_reg)["opp_score"]*goals[i,"a_goals"])

      lambda_bavg <- ((lambda_b1*(1-l_weight)) + (lambda_b2 * l_weight))/2

      goals[i,"b_goals"] <- rpois(1,lambda_bavg)

      if(function_mode == "chances"){
        if(goals[i,"a_goals"] > goals[i,"b_goals"]){
          a_win = a_win + 1
        }else if(goals[i,"a_goals"] < goals[i,"b_goals"]){
          b_win = b_win + 1
        }
      }
    }
  }

  if(function_mode == "chances"){ #returns odds of win, loss, or draw
    return(list(a_win/num_sims, b_win/num_sims, (1 -(a_win/num_sims) - (b_win/num_sims))))
  }else{
    return(goals)
  }
}
