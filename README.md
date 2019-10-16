# Predicting The Results of the English Premiere League.
This project was made for [STU Big Data](https://github.com/STUBigData).

### The Objective
To Increase the Accuracy of Predicting the Results of Soccer Matches of the EPL based on Team and Individual Player Data Using an Artificial Neural Network.

#### Background
The English Premeire Legue is the best league in the world right now. Has been named the best league in the world three years in a row. It consists of 20 teams. 
The prediction of the premiere league is an ongoing project by [STU Big Data](https://github.com/STUBigData).
Early versions of the predictor delt with team data as a whole. Our version of the predictor uses data from all the players.

## Requirements
The requirements to run this project are:
- R laguage
- Packages: `rvest`, `h2o`, `tydiverse`,`purrr`, and `magrittr`
- This scripts should be run on a high end computer.

### Usage
The steps followed to run the package are the following.
1. Create a file .txt with the matches urls from [FBref](https://fbref.com/en/comps/9/Premier-League-Stats)
2. Run the EPL_SCRAPER.R.
  This Script does the following:
  a). Scrapes match results using `rvest` from [FBref](https://fbref.com/en/comps/9/Premier-League-Stats)
  b). Scrape player data using `rvest` with the matches urls obtained from  [FBref](https://fbref.com/en/comps/9/Premier-League-Stats)
  c). Merge the two data sets by averaging the last 5 games played by each team and the last 5 apperances of each player.
3. Run the EPL_ANN.R file. Make sure all independant variables, number of Layers, Runs, and Nodes are adjusted to your specific enviroment. 
4. The EPL_ANN.R script will output two files, one for accuracy and one for predictions. 
