# How People Get to Work
## Dashboarding with shiny

This dashboard maps how commuters using different modes of transport to get to work are distributed around city centers. This was my final project for Data Visualization (STAT 302) at Northwestern in Winter 2022. It is hosted at https://shruti820.shinyapps.io/TransportApp/

I used the mapview package to create interactive maps of New York City, Chicago, and Los Angeles. Census tract level data on commutes is from the NHGIS database, but I got the median income estimates that I use for the popups internally from the tigris package in R. I used interactives (radio buttons and a drop down) so that users can select which city, the mode of transport, and the map base layer they prefer.

The project prompt was to create any set of visualizations in R. I chose to use shiny so I could make it interactive, and I chose the topic because I wanted to map Census data. Creating the popups that appear when you click took the longest to troubleshoot, and I finally did them with HTML. The popups still don't work on the web app for Chicago and L.A., I think due to data constraints on free users of shinyapps.
