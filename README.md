# Predictive Factors of Powerlifting Competition Performance

## Table of Contents

- [What is Powerlifting?](#what-is-powerlifting)
- [Overview](#overview)
- [Purpose of the Project](#purpose-of-the-project)
- [Features](#features)
- [Results](#results)
- [Dataset](#dataset)
- [Similar Projects Regarding Powerlifting](#similar-projects-regarding-powerlifting)

## What is Powerlifting?

Powerlifting is a competitive strength sport consisting of three movements: **Squat**, **Bench Press** and **Deadlift**. The goal is to <ins>lift the heaviest amount of weight possible in each movement to maximize the total</ins> (the sum of the best attempts of each movement). The athlete with the highest total wins the competition.

## Overview

There are two models in this project: a proportions model and a logistic regression model. The first is used to classify athletes by movement specialty. The second is used to predict the podiums and study the variables involved in the model to determine feature importance.

## Purpose of the Project

1. Proof data collection of variables help to understand and study sports.
2. Improve Powerlifting strategies based on high performance data.
3. Explore the sport from different perspectives in order to incentivize data collection.

## Features

- Classifies athletes by movement specialty based on their best attempts of each lift.
- Predicts athletes reaching the podium using some of their metrics.
- Implements threshold optimization for better results.
- Plots ROC/PR curves and Confusion Matrices to interpret the results.

## Results:

- The vast majority of athletes are **balanced** in terms of proportions between the three movements.
- Model accuracy hits 79% and AUC is 82-85%.
- **Balanced athletes**, **Squat** and **Bench** specialists are more likely to reach the podium. 
- The **Deadlift** is the most important lift of all three movements. **Squat** is also very important for the male athletes.

## Dataset

The data has been downloaded from www.openpowerlifting.org on January 4th, 2025.

## Similar Projects Regarding Powerlifting

While this project is a creation of my own and had not seek inspiration from others, I recently found a couple of articles that could also be helpful to those looking to study the sport from other perspectives that are similar to mine:

1. [What are the odds? Identifying factors related to competitive success in powerlifting](https://bmcsportsscimedrehabil.biomedcentral.com/articles/10.1186/s13102-022-00505-2#:~:text=The%20results%20suggest%20that%20competitors,compared%20to%20other%20PL%20athletes.)
2. [Multivariate Multiple Regression with Applications to Powerlifting Data](https://scse.d.umn.edu/sites/scse.d.umn.edu/files/cassiequickfinalpaper.pdf)
