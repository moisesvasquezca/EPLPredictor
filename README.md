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
- Packages: `rvest`, `h2o`, and `tydiverse`
- Thanos

### Usage
The steps followed to run the package are the following.
1. Scrape match results using `rvest` from [FBref](https://fbref.com/en/comps/9/Premier-League-Stats)
2. Scrape player data using `rvest` from  [FBref](https://fbref.com/en/comps/9/Premier-League-Stats)
3. Merge the two data sets by averaging the last 5 games played by each team and the last 5 apperances of each player.
4. Run the Neural Network by adjusting the independant variables.
