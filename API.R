#myAPI.R 
library(GGally)
library(baguette)
library(glmnet)
library(lattice)
library(lubridate)
library(plumber)
library(ranger)
library(tidymodels)
library(tidyverse)
library(vip)
library(vroom)

#read in data
#diabetes_binary_health_indicators_BRFSS2015.csv
#dbhi = diabetes binary health indicators
data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

dbhi_data <- data |>
  mutate(Diabetes_binary=
           factor(Diabetes_binary,
                  levels=c("0","1"),
                  labels=c("no diabetes","prediabetes or diabetes")),
         HighBP=
           factor(HighBP,
                  levels=c("0","1"),
                  labels=c("no high BP","high BP")),
         HighChol=
           factor(HighChol,
                  levels=c("0","1"),
                  labels=c("no high cholesterol","high cholesterol")),
         CholCheck=
           factor(CholCheck,
                  levels=c("0","1"),
                  labels=c("no cholesterol check in 5 years","yes cholesterol check in 5 years")),
         #BMI=factor(BMI),
         Smoker=
           factor(Smoker,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         Stroke=
           factor(Stroke,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         HeartDiseaseorAttack=
           factor(HeartDiseaseorAttack,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         PhysActivity=
           factor(PhysActivity,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         Fruits=
           factor(Fruits,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         Veggies=
           factor(Veggies,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         HvyAlcoholConsump=
           factor(HvyAlcoholConsump,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         AnyHealthcare=
           factor(AnyHealthcare,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         NoDocbcCost=
           factor(NoDocbcCost,
                  levels=c("0","1"),
                  labels=c("no","yes")),
         GenHlth=
           factor(GenHlth,
                  levels=c("1","2","3","4","5"),
                  labels=c("excellent","very good","good","fair","poor")),
         #MentHlth=
         #factor(MentHlth),
         #PhysHlth=
         #factor(PhysHlth),
         DiffWalk=
           factor(DiffWalk,
                  levels=c("0","1"),
                  labels=c("no difficulty walking","yes difficulty walking")),
         Sex=
           factor(Sex,
                  levels=c("0","1"),
                  labels=c("female","male")),
         Age=
           factor(Age,
                  levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13"),
                  labels=c("18 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 or older")),
         Education=
           factor(Education,
                  levels=c("1","2","3","4","5","6"),
                  labels=c("Never attended school or only kindergarten","Grades 1 through 8 (Elementary)","Grades 9 through 11 (Some high school)","Grade 12 or GED (High school graduate)","College 1 year to 3 years (Some college or technical school)","College 4 years or more (College graduate)")),
         Income=
           factor(Income,
                  levels=c("1","2","3","4","5","6","7","8"),
                  labels=c("Less than $10,000","$10,000 to less than $15,000","$15,000 to less than $20,000","$20,000 to less than $25,000","$25,000 to less than $35,000","$35,000 to less than $50,000","$50,000 to less than $75,000","$75,000 or more"))
  )

#Fit the ‘best’ model to the entire data set

#Split the data into testing and training
set.seed(11)
dbhi_split <- initial_split(dbhi_data, prop = 0.70, strata=Sex) #strata = argument goes in the parentheses, if needed
dbhi_train <- training(dbhi_split)
dbhi_test <- testing(dbhi_split)

#Recipe
#Diabetes_binary ~ Age + HeartAttackorDisease + Income + Sex + HvyAlcoholConsump
dbhi_recipe <- recipe(Diabetes_binary ~ ., data = dbhi_train) |>
  update_role(Age,Smoker,BMI,HighBP,HighChol,CholCheck,PhysActivity,
              Fruits,Veggies,AnyHealthcare,NoDocbcCost,GenHlth,MentHlth,
              PhysHlth,DiffWalk,Education,new_role = "bystander") |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric(), -all_outcomes())

#Random forest
rf_spec <- rand_forest(mtry = 9) |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

#Workflow
rf_wkf <- workflow() |>
  add_recipe(dbhi_recipe) |>
  add_model(rf_spec)

best_model <- rf_wkf |>
  finalize_workflow(rf_wkf) |>
  fit(dbhi_data)

#* info endpoint
#* @get /info
function(){
  "Name:  Matthew Corne; Github Pages URL:  TBD"
}

#http://localhost:9000/info
#http://127.0.0.1:9071/__docs__/info


#* pred endpoint 
#* @param Sex female or male
#* @param Income Less than $10,000, $10,000 to less than $15,000, $15,000 to less than $20,000, $20,000 to less than $25,000, $25,000 to less than $35,000, $35,000 to less than $50,000, $50,000 to less than $75,000, $75,000 or more
#* @param Stroke no or yes
#* @param HeartDiseaseorAttack no or yes
#* @param HvyAlcoholConsump no or yes
#* @get /pred
function(Sex = "female", 
         Income = "$35,000 to less than $50,000", 
         Stroke = "no", 
         HeartDiseaseorAttack = "no", 
         HvyAlcoholConsump = "no"){
  
}

#http://localhost:9000/pred?Sex=female&Income=$35,000%20to%20less%20than%20$50,000&Stroke=no&HeartDiseaseorAttack=no&HvyAlcoholConsump=no
#http://127.0.0.1:9071/__docs__/pred?Sex=female&Income=$35,000%20to%20less%20than%20$50,000&Stroke=no&HeartDiseaseorAttack=no&HvyAlcoholConsump=no

#* confusion endpoint
#* @get /confusion
#* @serializer png
#* @param type heatmap or mosaic
function(type = "heatmap"){
  cm <- dbhi_data |> 
    mutate(
      estimate = best_model |> 
        predict(dbhi_data) |> 
        pull()) |>
    conf_mat(Diabetes_binary,estimate)
  
  
  #cmplot <- autoplot(cm, type)
  cmplot <- levelplot(cm$table, cuts=1, col.regions=c("red", "blue"))
   
#  ffplot <- fourfoldplot(cm$table, color = c("#CC6666", "#99CC99"),
#  conf.level = 0, margin = 1, main = "Confusion Matrix")
  
  print(cmplot)
}

#http://localhost:9000/confusion?type=heatmap
#http://127.0.0.1:9071/__docs__/confusion?type=heatmap