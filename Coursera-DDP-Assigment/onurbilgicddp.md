Devoloping Data Products with Shinyapps
========================================================
author: Onur Bilgic   
date: 18th March 2017
transition: rotate
transition-speed : slow
autosize: true
font-family: 'Risque'
width: 1000
height: 800
General Information
========================================================

This application is made for Coursera Devoloping Data Products assignment. It is aim that using twenty years data to predict a match
results with decision tree and estimate goal numbers with lineer regression. Their accuracy are too low to trust. In future some changes will be able to be made for confident predictions. The database also is in the main panel. In addition a good part of this tool the user can select date,league and teams paramathers easily. It updates itself. By these choises, two graphics can be displayed on the screen. First one winning and draw barplot and second one is goals barplot. 


General View
========================================================

![alt text](img1.png)



Models and Features
========================================================
autosize: true
I used decision tree for predicting the match result. My outcome is match result which are home or away wins and draw. My incomes are date, mean odds for home away and draw and half time goals,means of league goals, means of home team and away teams goals and mean of previous mathces goals. 

To predict all goals for match , the lineer regression is used, however same features are used as decision tree.
***

The important features are below. 

- Date
- League
- Home Team 
- Away Team 
- Odds
- Half Time Results



 


Data Source
========================================================


The data source is Football-Data.co.uk. Files are organized as one season with one league generally. Therefore they are merged together and created a new database. In addition there are lot odds features in 
primitive data, after the mean function is used for reducing features numbers. 
http://www.football-data.co.uk/


Application and Code Links
========================================================

You can find my application and codes in below links. 

- https://onurbilgicddp.shinyapps.io/models/

- https://github.com/OnurBilgic/Coursera-DDP-Assignment


Thank you for reading my study 
