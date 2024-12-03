#myAPI.R 
library(GGally)

#An info endpoint. This endpoint shouldn’t have any inputs. The output should be a message with:
#– Your name
#– A URL for your rendered github pages site
#* @get /info
function(){
  "This is our basic API"
}

#http://localhost:PORT/readme


#* A pred endpoint. This endpoint should take in any predictors used in your ‘best’ model. You should
#have default values for each:
#the mean of that variable’s values (if numeric) or 
#the most prevalent class (if categorical). 
#Below this API put three example function calls to the API in
#comments so that I can easily copy and paste to check that it works!
#* @param pred1 1st predictor
#* @param pred2 2nd predictor
#* @param pred3 3rd predictor
#* @param pred4 4th predictor
#* @param pred5 5th predictor
#* @get /pred
function(pred1, pred2, pred3, pred4, pred5){
  log(as.numeric(num))
}

#query with http://localhost:PORT/pred?pred1=10&pred2=20&pred3=30&pred4=40&pred5=50

#* A confusion endpoint. This endpoint should produce a plot of the confusion matrix for your model
#fit. That is, comparing the predictions from the model to the actual values from the data set (again)                                                                                              you fit the model on the entire data set for this part).
#* @serializer png
#* @param type base or ggally
#* @param color TRUE or FALSE (only for ggally)
#* @get /confusion
function(type = "base", color = FALSE){
  if(tolower(type) == "ggally"){
    if(color){
      a <- GGally::ggpairs(iris, aes(color = Species))
      print(a)
    } else {
      a <- GGally::ggpairs(iris)
      print(a)
    }
  } else {
    pairs(iris)
  }
}
#http://localhost:PORT/plotiris?type=ggally


#* Plotting widget
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet::leaflet() |>
    addTiles() |>  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&lat=-36


# Choose a predictor
#* @param predictor
#* @get /pred
function(predictor) {
  data <- iris
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (predictor == "Species") {
    table <- table(data[[predictor]])
    return(paste0(names(table), ": ", table))
  } else {
    stop("Invalid predictor.")
  }
}