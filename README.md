# SoccerEloForecast

`SoccerEloForecast` is a package that allows a user to anaylze previous matches and Elo ratings to fit Possion distributions onto them. This regression can also be used to simulate games and calculate the probabilities of match results.

## Installation
You can install this function from Github using devtools:

``` r
install.packages("devtools")
install_github("zecellomaster/SoccerEloForecast")
```

Let me know if there are any issues.

## Functions

### Forecasting

`poissonregression(elos, home, prev_matches, type)`:
This function uses the current Elo ratings of 2 teams along with prior Elo/match result data to formulate a Poisson regression and returns one of three types of lists.

The first item will always be `state`, which signifies the rest of the list. Fore each state, the rest of the list is comprised of:

1) The random variable for Team A (`lambda_a`) and the raw Poisson generalized linear model (`b_reg`) so that random variable for Team B can be calculated 

2) The random variables for both Teams A (`lambda_a`) and B (`lambda_b`)

3) The random variables for both Teams A (lambda_a), B (lambda_b), the raw Poisson generalized linear model (`b_reg`) so that random variable for Team B can be calculated, and the `weight_factor`. This factor is based on how many previous matches were sent to the function. It becomes 1 after 25 matches as it is meant to be used to calculate weighted average of the nested random variable and independent one, where the first will go to zero as more games are sent while the latter becomes 1.

This function is based off a methodology for Elo-based independent and nested Poisson forecast models described by Lorenz A. Gilch and Sebastian Müller in the paper
*[On Elo Based Prediction Models for the FIFA World Cup 2018](https://arxiv.org/pdf/1806.01930.pdf)*.

`gameprediction(function_mode = "chances", elos, home, prev_matches)`:
This uses the output of 'poissonregression' to either simulate a number of matches(if `function_mode` is a positive integer) or calculate the % chances of a win/loss/draw for the higher Elo team (default).

Note that in both functions, the team with the higher elo rating (Team A) must have their data entered first. For more information, check out the function documentation.

### Coming soon: In-game win probability


### Gloassary
`elos`: a vector that contains the Elos of each team. Team A is the higher Elo team and is listed first.

`home`: 2 item vector of booleans that dictates whether or not either team is at home.

`prev_matches`: list of dataframes; contains previous matches, Elo ratings, and (if available) match weights of Teams A ([[1]]) and B ([[2]]) respectively.
Dataframe columns: team_elo, opp_elo, team_score, opp_score, team_home, team_away, weight.

`type`: The type of Poisson regression to be used; nested ("nested")  or independent ("indep"). If empty, the function will choose the best type depending on the circumstances.

`team_home`: boolean column in both of 'prev_matches' dataframe that dictates if the team was at home

`opp_home`: boolean column in both of 'prev_matches' dataframe that dictates if the opponent was at home

## See this in action

To better understand the purpose of this package check out these other repositories:

[MLSEloForecast](https://github.com/zecellomaster/SoccerEloForecast): A group of R scripts meant to forcast the MLS Season.


