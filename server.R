library(shiny)

shinyServer(function(input, output) {
  
  library(data.table)
  score  <- fread('www/ml-20m/genome-scores.csv', sep=',')
  movies <- fread('www/ml-20m/movies.csv', sep=',')
  tag    <- fread('www/ml-20m/genome-tags.csv', sep=',')
  links  <- fread('www/ml-20m/links.csv', sep=',')
  find_tags <- function(movie_number){
    setkey(score, movieId)
    target <- score[movieId == movie_number]
    top_tags <- target[order(relevance, decreasing = TRUE)[1:10], tagId]
    print(tag[tagId %in% top_tags])
    return(list(top_tags = top_tags, origin_number = movie_number))
  }
  find_recommendations <- function(tags, n){
    possi <- score[tagId %in% tags$top_tags]
    possi_rele <- possi[, .(rele_mean = mean(relevance)), by = movieId]
    possi_id <- possi_rele[order(rele_mean, decreasing = TRUE)[1:(n+1)], movieId]
    possi_id_minus_origin <- setdiff(possi_id, tags$origin_number)[1:n]
    return(movies[movieId %in% possi_id_minus_origin, ])
  }
  get_tt <- function(movie_id) {
    return(links[movieId == movie_id, imdbId])
  }
  get_poster <- function(movie_id) {
    tt <- get_tt(movie_id)
    if(nchar(tt) == 6) {
      tt <- paste0("0", tt)
    }
    src <- paste0("http://img.omdbapi.com/?i=tt", tt, "&h=600&apikey=YOUR API KEY HERE")
    return(src)
  }
  generate_item <- function(id = 1, rec.table){
    paste0(
      "
      <div class=\"grid-item\">
      <div class=\"grid-info\">
      <img src=\"",
      get_poster(rec.table$movieId[id]),
      "\">
      </div>
      <div class=\"grid-info\">
      <h3>",
      rec.table$title[id],
      "</h3>
      </div>
      </div>"
    )
  }
  #####################################################################################
  output$table <- renderTable({
    tags <- find_tags(input$id)
    find_recommendations(tags, input$number)
  })
  output$input <- renderText({
    return(   
      paste0(
        "
        <div class=\"info-container\">
        <div class=\"info-item\">
        <img src=\"",
        get_poster(input$id),
        "\">
        </div>
        <div class=\"info-item\">
        <h3>",
        movies[movieId == input$id, title],
        "</h3>
        <h3>Genres: ",
        movies[movieId == input$id, genres],
        "</h3>
        </div>
        </div>"
      )
    )
    
  })
  output$input_movie <- renderText({
    return(get_poster(input$id))
  })
  output$test <- renderText({
    tags <- find_tags(input$id)
    rec.table <- find_recommendations(tags, input$number)
    return(
      paste0(
        "<div class=\"grid-container\">",
        paste(sapply(1:input$number, function(x) generate_item(x, rec.table)), collapse = ""),
        "</div>"
      )
    )
  })
  output$candidates <- renderTable({
    if(input$search != ""){
      return(
        movies[grep(input$search, movies[, title], ignore.case = TRUE)]
      )
    } else {
      return()
    }
  })
  
  
  })