## Overview

This post will show you how to build a simple recommendation system with GUI powered by Shiny, step by step.

![test](https://storage.googleapis.com/photo-for-moor/test.jpg)

## Tools

- [R](https://www.r-project.org/)
- [RStudio](https://www.rstudio.com/) and [Shiny](https://www.rstudio.com/products/shiny/)
- [Dataset from Moivelens](https://grouplens.org/datasets/movielens/)

## Preparation

### 1. Installation of software

- Install R, but I guess you all have it installed.

- Install RStudio. Who uses R without RStudio?

- Install Shiny.

  For most of us, RStudio will automatically give you a wizard to install Shiny, when you trying to create a Shiny app. So you don't need to worry about it now.

  Or Shiny can be installed as followed. Type this in console.

  ` install.packages("shiny")`

### 2. Data collection

Download the "[MovieLens 20M Dataset](http://files.grouplens.org/datasets/movielens/ml-20m.zip)", which contains 20 million records in the size of 190 MB. This zip file contains 6 datasets, which are `genome-scores`, `genome-tags`, `links`, `movies`, `ratings`, and `tags`.

We will use `genome-scores`, `genome-tags`, `links` and `movies`. I will fill you in with some descriptions.

* Basic properties

  An internal movie id is used as primary key to identify movies.

* `genome-scores`

  This dataset describes the relationship between movies and tags, how much a movies fits a tag, Like Titanic is achieving high scores in romance and tragedy, but low in comedy. For example, tag No. 175 is "California".

* `genome-tags`

  This dataset is aux dataset for describe every tag. 

* `links`

  Aux set provides external IMDB and TMDB id for each movie. For example, movie with internal id 1 is  with IMDB id 0114709 and TMDB id 862.

* `movies`

  A dataset tells you which movie is linked to which internal id. For example, id No. 1 is Toy Story.

![datasets](https://storage.googleapis.com/photo-for-moor/datasets.jpg)

## Model Building

### 1. Ideas

Here I employed a vey intuitive algorithm, because I want to spend time on building a useful and deployable application, rather than on building a comprehensive model.

Assume we have **one input movie**, who has **scores on all 1000+ tags**, and we want to find similar movies to recommend, as output. I select **10 tags that have biggest scores**, thus we can build a 10-dimension working space. Now I select some **movies that is closest this movie** in the working space. And we are done!

However, I select movies that is most far from origin point, thus we can find the best movies in this working space. Maybe a movie that is better than our input movie.

### 2. Steps

First of all, let's load all the dataset. I used `data.table` here to improve speed.

```R
## Using data.table
> system.time({
+   library(data.table)
+   fread('D:/Temp/ml-20m/genome-scores.csv', sep=',')
+ })
Read 11709768 rows and 3 (of 3) columns from 0.301 GB file in 00:00:12
   user  system elapsed 
  10.78    0.05   11.12
  
## Using native data.frame
> system.time({
+   read.csv('D:/Temp/ml-20m/genome-scores.csv', sep=',')
+ })
   user  system elapsed 
  29.10    0.55   29.93
```

The largest set here is `genome-scores`. As we can see from performance comparison, `data.table` is 3 times faster than `data.frame`.

Let's put data into memory.

```R
library(data.table)
score  <- fread('D:/Temp/ml-20m/genome-scores.csv', sep=',')
movies <- fread('D:/Temp/ml-20m/movies.csv', sep=',')
tag    <- fread('D:/Temp/ml-20m/genome-tags.csv', sep=',')
links  <- fread('D:/Temp/ml-20m/links.csv', sep=',')
```

Then we will write a module than can build working space.

```R
find_tags <- function(movie_number) {  # input movie is represented by internal movie id
    
  setkey(score, movieId)
  target <- score[movieId == movie_number]  # find scores of input movie
  top_tags <- target[order(relevance, decreasing = TRUE)[1:10], tagId]  # find 10 most related tags
  print(tag[tagId %in% top_tags])  # inspection the tags
  return(list(top_tags = top_tags, origin_number = movie_number))  # return movie id and tag id
    
  }
```

Let's check the result of movie No. 1, Toy Story.

```
> find_tags(1)
    tagId                tag
 1:    64          animation
 2:   186            cartoon
 3:   204           children
 4:   244 computer animation
 5:   536       imdb top 250
 6:   588               kids
 7:   589    kids and family
 8:   785              pixar
 9:   786    pixar animation
10:  1036               toys
$top_tags
 [1] 1036  244  786  589   64  588  785  204  186  536

$origin_number
[1] 1
```

Great, it works!

Then we will work on output.

```R
find_recommendations <- function(tags, n) {  # input is result from previous module and number of recommendations you want
    
  possi <- score[tagId %in% tags$top_tags]  # select working space
  possi_rele <- possi[, .(rele_mean = mean(relevance)), by = movieId]  # calculate mean score in the space
  possi_id <- possi_rele[order(rele_mean, decreasing = TRUE)[1:(n+1)], movieId]  # Find top n+1 movies. Usually input movie is top 1 but not necessarily.
  possi_id_minus_origin <- setdiff(possi_id, tags$origin_number)[1:n]  # remove input movie from output candidates
  return(movies[movieId %in% possi_id_minus_origin, ])  # refer the information of movies
    
  }
```

Let's check the result of movie No. 1, Toy Story.

```
> find_recommendations(find_tags(1), 10)
    tagId                tag
 1:    64          animation
 2:   186            cartoon
 3:   204           children
 4:   244 computer animation
 5:   536       imdb top 250
 6:   588               kids
 7:   589    kids and family
 8:   785              pixar
 9:   786    pixar animation
10:  1036               toys
    movieId                           title                                           genres
 1:    2355            Bug's Life, A (1998)              Adventure|Animation|Children|Comedy
 2:    3114              Toy Story 2 (1999)      Adventure|Animation|Children|Comedy|Fantasy
 3:    4886           Monsters, Inc. (2001)      Adventure|Animation|Children|Comedy|Fantasy
 4:    6377             Finding Nemo (2003)              Adventure|Animation|Children|Comedy
 5:    8961         Incredibles, The (2004)       Action|Adventure|Animation|Children|Comedy
 6:   45517                     Cars (2006)                        Animation|Children|Comedy
 7:   50872              Ratatouille (2007)                         Animation|Children|Drama
 8:   68954                       Up (2009)               Adventure|Animation|Children|Drama
 9:   76093 How to Train Your Dragon (2010)        Adventure|Animation|Children|Fantasy|IMAX
10:   78499              Toy Story 3 (2010) Adventure|Animation|Children|Comedy|Fantasy|IMAX
```

Check all the recommendations, it works fine.

## Shiny GUI

After building backend functions, we can work on GUI now.

![build shiny](https://storage.googleapis.com/photo-for-moor/build shiny.jpg)

Some clicks, some type in. Name your app, choose "multiple file" and select your directory.

![build shiny files](https://storage.googleapis.com/photo-for-moor/build shiny files.jpg)

Now we have two separate files, ui.R and server.R. Let's implement them.

ui.R

```R
library(shiny)

shinyUI(fluidPage(  # fluid page is a simple frame, learn more on shiny website
  
  theme = "shiny.css",  # I am using aux css, it should be stored in a folder called "www" in same directory
  
  titlePanel("Tag-based Recommandation System"),  # this is tile
  
  sidebarLayout(  # I chose to use a layput with sidebar, so I can configure model and put in some info
    ###################################################
    sidebarPanel(
      numericInput("id",  # here I can put in movie id
                   "movieId:",
                   min = 1,
                   max = 131262,
                   value = 1),
      numericInput("number",  # here I can put in the number of recommendations I want
                   "recommandation number:",
                   min = 1,
                   max = 50,
                   value = 8),
      textInput("search", "Search"),  # because Shiny can not interact with output, so I build a shabby search engine, you got to match characters to get result, then use movie id to perform recommendation
      tableOutput("candidates")
    ),
    ###################################################
    mainPanel(
      h3("Input Information"),
      fluidRow(
        htmlOutput("input")
      ),
      h3("Movies you may like"),
      fluidRow(
        htmlOutput("test")  # the layout of output is changing depends on number of recommendations
      )
    )
    ###################################################
  )
  
))
```

server.R

```R
library(shiny)

shinyServer(function(input, output) {
  
  # ... recommendation modules omitted
  
  get_tt <- function(movie_id) {
    return(links[movieId == movie_id, imdbId])
  }
    
  get_poster <- function(movie_id) {  # I bought an api key from OMDB, for 5 bucks a month, so that I can get posters
    tt <- get_tt(movie_id)
    if(nchar(tt) == 6) {
      tt <- paste0("0", tt)
    }
    src <- paste0("http://img.omdbapi.com/?i=tt", tt, "&h=600&apikey=YOUR API KEY HERE")
    return(src)
  }
    
  # ... output generation modules omitted
  
  })
```

## Final Product

![test](https://storage.googleapis.com/photo-for-moor/test.jpg)

Use the search box in the sidebar, let's say "spider-man", to get the corresponding ID. Then put the movie ID of spider-man in the "movieid" box. Here we are. Try it!

![spider2](https://storage.googleapis.com/photo-for-moor/spider2.jpg)

Please use [Google Chrome](https://www.google.com/chrome/) and allow 10 seconds to let it load dataset. https://moorchen.shinyapps.io/tag-based

The full code is on [GitHub](https://github.com/moorchen/tag-based-recommendation). I removed dataset because GitHub don't allow that big size. So if you want to run the code, please download the dataset and put them into the "www" folder. Also, you need to [buy an API from omdb](http://www.omdbapi.com/) to get all those posters.