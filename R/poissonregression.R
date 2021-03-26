#File: poissonregression.R
#Author: zecellomaster
#Date: 03/17/21 (v1.0)
#' Calculates the Poisson regressions of two teams using prior match results and
#' Elo data.
#'
#' @import stats
#'
#' @param elos List; current Elo ratings of Teams A and B respectively. Team A
#' is in the first index and has the higher rating
#'
#' @param home 2 item List of booleans; Whether or not either team is at home
#'
#' @param prev_matches List of Dataframe; contains previous matches, Elo ratings,
#' and (if available) match weights of Teams A and B respectively.
#' Dataframe columns: team_elo, opp_elo, team_score, opp_score, team_home, team_away,
#' weight
#'
#'
#' @return regressions, state, adjust_weight (if Team B # of matches < 25)
#'
#' @export

poissonregression <- function(elos, home, prev_matches){
  #Checks if Team A has the higher Elo
  if (elos[1] < elos[2]){
    stop("WARNING: Team A's Elo rating must be *greater than or equal to* Team B's.
         Check the order and try again.")
  }


  #Checks if either team is at home
  if (home[1] == TRUE){
    elos[1] <- elos[1] + 100

  } else if (home[2] == TRUE){
    elos[2] <- elos[2] + 100

  }
  #Here, we adjust the Elo rating depending on who is home
  for (i in 1:length(prev_matches)){

    if(length(prev_matches[[i]]) == 0){
      next
    }

    for (j in 1:dim(prev_matches[[i]])[1]){

      if (prev_matches[[i]][j,"team_home"] == TRUE){
        prev_matches[[i]][j,"team_elo"] <- prev_matches[[i]][j,"team_elo"] + 100

      } else if (prev_matches[[i]][j,"opp_elo"] == TRUE){
        prev_matches[[i]][j,"opp_elo"] <- prev_matches[[i]][j,"opp_elo"] + 100

      }
    }
  }

  if (dim(prev_matches[[1]])[1] <= 14 && dim(prev_matches[[2]])[1] > 14){
    #If Team A has little to no match data, but Team B does, then the Poisson
    #regression only uses Team B data for both lambda_a and lambda_b (nested)

    nu_b <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[2]][,"weight"],data =  prev_matches[[2]])

    lambda_a <- exp(coef(nu_b)["(Intercept)"] + (coef(nu_b)["opp_elo"]*elos[1]))

    b_reg <- glm(formula = team_score ~ opp_elo + opp_score, family = poisson(link = "log"),
                 weights = prev_matches[[2]][,"weight"], data = prev_matches[[2]])
    state <- 1

    return(list(state, lambda_a, b_reg))

  }else if (dim(prev_matches[[2]])[1] <= 14) {
    #If Team B has little to no match data, regardless of whether or not Team A does
    #we switch to 2 independent regressions using Team A data

    mu_a <- glm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[1]][,"weight"], data =  prev_matches[[1]])

    nu_a <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[1]][,"weight"], data =  prev_matches[[1]])

    lambda_a <- exp(coef(mu_a)["(Intercept)"] + (coef(mu_a)["opp_elo"]*elos[2]))
    lambda_b <- exp(coef(nu_a)["(Intercept)"] + (coef(nu_a)["opp_elo"]*elos[1]))

    state <- 2

    return(list(state, lambda_a, lambda_b))

  }else if (dim(prev_matches[[1]])[1] < 25) {
    #If Team A has insufficient match data, then its contribution to its
    #Poisson regression is weighted by the number of matches it has completed. Follows
    #standard nested procedure
    adjust_weight = dim(prev_matches[[1]])[1]/50

    mu_a <- glm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
                 weights = prev_matches[[1]][,"weight"],data =  prev_matches[[1]])

    nu_b <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[2]][,"weight"],data =  prev_matches[[2]])

    lambda_a <- (exp((coef(mu_a)["(Intercept)"] + (coef(mu_a)["opp_elo"]*elos[2]))*(adjust_weight))+
                   (exp(coef(nu_b)["(Intercept)"] + (coef(nu_b)["opp_elo"]*elos[2]))*(1-adjust_weight)))/2

    b_reg <- glm(formula = team_score ~ opp_elo + opp_score, family = poisson(link = "log"),
                 weights = prev_matches[[2]][,"weight"], data = prev_matches[[2]])


    state <- 1

    return(list(state, lambda_a, b_reg))

  }else if (dim(prev_matches[[2]])[1] < 25) {
    #If Team B has no match data, we switch to 2 independent regressions
    #using Team A data, but also calculate the nested regression. We send the
    #weight as well
    adjust_weight = dim(prev_matches[[2]])[1]/50

    mu_a <- glm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[1]][,"weight"],data =  prev_matches[[1]])

    nu_b <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[2]][,"weight"],data =  prev_matches[[2]])

    mu_b <-glm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
               weights = prev_matches[[2]][,"weight"],data =  prev_matches[[2]])

    nu_a <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[1]][,"weight"],data =  prev_matches[[1]])

    lambda_a <- ((exp(coef(mu_a)["(Intercept)"] + (coef(mu_a)["opp_elo"]*elos[2]))*adjust_weight) +
                (exp(coef(nu_b)["(Intercept)"] + (coef(nu_b)["opp_elo"]*elos[1]))*(1-adjust_weight)))/2


    lambda_b <- ((exp(coef(mu_b)["(Intercept)"] + (coef(mu_b)["opp_elo"]*elos[1]))*(1-adjust_weight)) +
                   (exp(coef(nu_a)["(Intercept)"] + (coef(nu_a)["opp_elo"]*elos[2]))*adjust_weight))/2

    b_reg <- glm(formula = team_score ~ opp_elo + opp_score, family = poisson(link = "log"),
                 weights = prev_matches[[2]][,"weight"], data = prev_matches[[2]])

    state <- 3

    return(list(state, lambda_a, lambda_b, b_reg, adjust_weight*2))

  } else{
    #This portion calculates the necessary coefficients
    #Poisson Regression of goals scored *for* Team A as a function of the opponent's Elo
    mu_a <- glm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[1]][,"weight"],data =  prev_matches[[1]])

    #Poisson regression of goals scored *against* Team B as a function of Team B's Elo
    nu_b <- glm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                weights = prev_matches[[2]][,"weight"],data =  prev_matches[[2]])

    lambda_a <- (exp(coef(mu_a)["(Intercept)"] + (coef(mu_a)["opp_elo"]*elos[2])) +
                   exp(coef(nu_b)["(Intercept)"] + (coef(nu_b)["opp_elo"]*elos[1])))/2

    b_reg <- glm(formula = team_score ~ opp_elo + opp_score, family = poisson(link = "log"),
                 weights = prev_matches[[2]][,"weight"], data = prev_matches[[2]])
    state <- 1

    return(list(state,lambda_a, b_reg))
  }

}
