
========================================================
---
title: "Music Top 10"
output: html_document
---
How to predict if a song will be hit?
##Data
There is some data,

```r
setwd("~/work/learn/analyticsEdge")
songs <- read.csv("data/songs.csv")
```


Here's a detailed description of the variables:

year = the year the song was released
songtitle = the title of the song
artistname = the name of the artist of the song
songID and artistID = identifying variables for the song and artist
timesignature and timesignature_confidence = a variable estimating the time signature of the song, and the confidence in the estimate
 loudness = a continuous variable indicating the average amplitude of the audio in decibels
 tempo and tempo_confidence = a variable indicating the estimated beats per minute of the song, and the confidence in the estimate
 key and key_confidence = a variable with twelve levels indicating the estimated key of the song (C, C#, . . ., B), and the confidence in the estimate
 energy = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudness
pitch = a continuous variable that indicates the pitch of the song
 timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, . . . , timbre_11_min, and timbre_11_max = variables that indicate the minimum/maximum values over all segments for each of the twelve values in the timbre vector (resulting in 24 continuous variables)
Top10 = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 if it was in the top 10, and 0 if it was not)

##What can we do with the data?
A logistic regression model to predict if a song will be a hit, not much better than the baseline of negative answer for
each song. However the value of the model is in what you want it do. Do you want a conservative answer, to not make bad investments in songs that will not make it to the top 10? Then the model is good. The FDR (false discovery rate) is very small. 

__In any case, there is a lot of data here, can we improve the model? Use any other model for classification?__
