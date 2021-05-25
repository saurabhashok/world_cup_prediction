# FIFA 2018 Predictions (Upgrad)

The objective here is to predict the winner, runner-up and the semi-finalists in the 2018 World Cup.

## Getting Started

You will need the latest version of R and R studio along with all the libraries mentioned in below to get started
### Prerequisites

* R
* R Studio

```
library("tidyverse")
library("viridis")
library("gridExtra")
library("modelr")
library("broom")
library("ggrepel")
```

### Data

The data used here is 
* All the International football matches
* Finals of each World Cup
* A dummy data with 2018 fixtures


## Cleaning the data set

The data sets are mostly clean and does not require any specific cleaning
Note - The data set is duplicated at one point to accomodate a separate row for each team in a game played

### Feature Extraction and Engineering

The features extracted here are-
* Home goals scored
* Away goals scored

The features engineered are-
* Importance of Tournament
* Strength of the Tournament(across defense, offence and overall)
* Time to next World Cup

### Modelling

An Ensemble model consisting of poisson and Logistic regression are used to predict the goals scored for each game

The probability generated is then used to predict who wins the game
Even the number of Goals scored is predicted but the accuracy for that will be low

## Assumptions

* The games are predicted regardless of the knowledge the group to which each team belongs to
* The variables used are strenghts and importance across different characteristcs(Defensive, Offensice & Overall)

## Built With

R

## Visualizations

Used the ggplot library to plot all the required visualiations

## Authors

* Aditya Thokala
* Saurabh A K

## Acknowledgments

* The Kaggle community
* Inspiration - Upgrad

# Final Results
The model predicts SPAIN to be the winner of this World Cup.
BELGIUM being the runner-up followed by CROATIA and ENGLAND.
Lets hope for the best teams to win!