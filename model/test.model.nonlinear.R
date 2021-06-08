library( sf )
library( rgdal )
library( raster )
library( readxl )
library( tidyverse )
library( mc2d )
library( here )

# The ML 'tidymodels' collection
library( tidymodels )

setwd( here() )

source( "./model/globals.R" )
source( "./model/plot.functions.R" )
source( "./model/spatial.functions.R" )

# Here we test the model by generating "fake" Salmonella status data, to check if the model works well
# I make some fake data with a fake relationship to some variables 

# Adminstrative boarders of Spain, used for plotting purposes
path_adm <-file.path(".", "data", "original","ESP_adm")
adm.sf <- read_sf( dsn=path_adm, layer="ESP_adm2") %>%  
  st_transform( crs.default ) %>%
  st_crop( bbox.default )

# Municipality map of Spain
muniSpain <- read_sf( dsn="./data/original/spain_municipality_shape/", layer="spldf_muni") %>% 
  st_transform( crs.default ) %>%
  st_crop( bbox.default ) 
plot(muniSpain)

# Extract the boundaries values of spain
bnd_muniSpain <- st_bbox(muniSpain)
min_xbox <-  bnd_muniSpain[1]
max_xbox <-  bnd_muniSpain[3]
min_ybox <-  bnd_muniSpain[2]
max_ybox <-  bnd_muniSpain[4]

# Randomly generate measurements points within the boundaries of Spain
# This is done in pieces: to cover almost all the surface of Spain and avoid that the points fall in the water
n <- 700
coord_x <- runif(n, min = min_xbox + 2.7, max = max_xbox - 5) # I add and subtract some values to avoid that the centroids fall in the water
coord_y <- runif(n, min = min_ybox + 2.5, max = max_ybox - 0.8)
n1 <- 100
coord_x <- c(coord_x,runif(n1, min = -8.5, max = -6)) # I add and subtract some values to avoid that the centroids fall in the water
coord_y <- c(coord_y,runif(n1, min = 42, max = 43.2))
meas_points <- cbind(coord_x,coord_y) %>% as.data.frame()
n2 <- 100
coord_x <- c(coord_x,runif(n2, min = -2.3, max = 2.5)) # I add and subtract some values to avoid that the centroids fall in the water
coord_y <- c(coord_y,runif(n2, min = 41.5, max = 42.5))
meas_points <- cbind(coord_x,coord_y) %>% as.data.frame()
n3 <- 100
coord_x <- c(coord_x,runif(n3, min = -6, max = -2)) # I add and subtract some values to avoid that the measurements fall in the water
coord_y <- c(coord_y,runif(n3, min = 37, max = 37.5))
meas_points<- cbind(coord_x,coord_y) %>% as.data.frame()

# Transform in an sf object
sf_meas_points <- st_as_sf( meas_points, coords = c("coord_x", "coord_y"), crs=crs.default$proj4string )

# Plot the fake measurements
plot.shape( sf_meas_points, adm.sf, NULL, NULL, filename=NA, "Fake measurements" )

# Calculate buffers around the measurements
sf_meas_buffers <- st_buffer( sf_meas_points, dist=0.1 )

# Plot the buffers 
plot.shape( sf_meas_buffers, adm.sf, NULL, NULL, filename=NA, "Buffers around the fake measurements" )

# Get all the covariates
all.maps <- addLayer( brick( "./data/processed/all.maps.grd" ),
                      brick( "./data/processed/corine/corine.grd" ),
                      brick( "./data/processed/climate/climate.grd" ))

# Extract the covariates, taking the mean in the buffer area
all.maps.extract.meas <- raster::extract(all.maps, sf_meas_buffers, fun=mean, na.rm=TRUE, df=TRUE )

save.image( "./data/generated/all.maps.extracted.meas.RData" )
load( "./data/generated/all.maps.extracted.meas.RData" )

# Make up a relationship
# Collect the variables you want to stop in the relationship
# Relationship: p(positive) = logit (beta0 + beta1*maxT^2 + beta2*sin(boars) + beta3*density_hunting + beta4*indust ^(1/2))
beta0 <- -3.5
beta1 <-  0.08
beta2 <-  1.2
beta3 <- -30
beta4 <- -4

inv_logit <- function(x) exp(x)/(1+exp(x))

all.maps.extract.meas <- all.maps.extract.meas %>% 
  mutate( p_pos = (beta0 +beta1*Mean_temp_2011_2016^2 + beta2*sin(boars) + beta3*Dens_hunt + 
    beta4*(Industrial.or.commercial.units)^(0.5) + rnorm(n())) %>% 
      inv_logit())

# Plot the probability p_pos
with( all.maps.extract.meas, plot(p_pos[order(p_pos)]) )

# Add the status (0 = negative, 1 = positive) to the dataframe of the measurements
sf_meas_points <-  sf_meas_points %>% 
  cbind( all.maps.extract.meas$p_pos ) %>% 
  mutate( positiveN = rbern(n(), p_pos) )%>% 
  mutate( sampleN = 1)%>%
  filter(!is.na(positiveN))
            
# Spatially join the measurements points and the municipality
sf_meas_db <- st_join(muniSpain,sf_meas_points,left =FALSE) %>%  # inner join
  rename(muniID = ID_4) %>%
  group_by( muniID ) %>%
  summarize( status = ifelse( sum(positiveN>0), "Pos", "Neg" ),
  frac_pos = sum(positiveN)/sum(sampleN)) %>%
  dplyr::select( code=muniID, status, frac_pos ) %>%
  mutate( status = as.factor( status ) )

save(sf_meas_db, file="./data/generated/measurements_db.RData" )

plot.shape( sf_meas_db, adm.sf, my_fill=status, NULL, filename="fake_status", "Status of fake measurements" )
plot.shape( sf_meas_db, adm.sf, my_fill=frac_pos, NULL, filename="fake_frac_pos", "Fraction positives of fake measurements" )

# Calculate the centroids and create buffers around the centroids 
fake_status_centroids.sf <- st_centroid( sf_meas_db ) %>% dplyr::select( status, frac_pos, geometry )
fake_status_buffers.sf <- st_buffer( fake_status_centroids.sf, dist=0.1 )

plot.shape(fake_status_buffers.sf, adm.sf, my_fill = status, my_color = status, filename="fake_status_buffers", "Status of fake measurements in buffers" )
plot.shape(fake_status_buffers.sf, adm.sf, my_fill = frac_pos, my_color = frac_pos, filename="fake_frac_pos_buffers", "Fraction of positives in buffers" )

# Test the model
# Extract again the covariates now to match the size of the dataframe of the buffers

all.maps.extract.mod <- raster::extract(all.maps, fake_status_buffers.sf, fun=mean, na.rm=TRUE, df=TRUE )

save.image( "./data/generated/all.maps.extracted.mod.RData" )
load( "./data/generated/all.maps.extracted.mod.RData" )

data.total <- cbind( as.data.frame(fake_status_centroids.sf) %>% dplyr::select( status, frac_pos ),
                     all.maps.extract.mod %>% dplyr::select( -ID )) %>%
  na.omit()

data.total  %>%
  pivot_longer(-status, "riskfactor" ) %>% 
  ggplot( )+
    geom_histogram( aes( value, fill=status), position = "dodge" )+
    facet_wrap( ~riskfactor, scales="free" )

# Splitting
set.seed(31415)
split      <- initial_split(data.total %>% 
                              dplyr::select( -frac_pos ), prop = 3/4, strata = status )
train_data <- training(split)
test_data  <- testing(split)

preprocess <- recipe(status ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -status ) %>%
  step_nzv( all_predictors() ) %>%
  step_corr(all_numeric())

rf_model <- 
  rand_forest(trees = tune(), mtry = tune(), mode = "classification") %>%
  set_engine("randomForest", importance = T )

rf_param <- rf_model %>%
  parameters() %>%
  update(mtry = mtry(c(1L, 10L))) %>%
  finalize( select( data, -status, -frac_pos ) )

rf_grid <- rf_param %>%
  grid_max_entropy(size = 10)

rf_wflow <- workflow() %>%
  add_recipe(preprocess) %>%
  add_model(rf_model)

cv_splits  <- vfold_cv(train_data, v = 10, strata = status )

rf_tuned <- tune_grid(rf_wflow, cv_splits, param_info=rf_param, grid=rf_grid, 
                      metrics = metric_set( roc_auc, accuracy ) ) 
rf_tuned

# have a look at the tuning
rf_tuned %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# choose the best model
best_auc <- select_best(rf_tuned, "roc_auc")

# use the finalize_model function to select the last model
best_rf_model <- finalize_model(
  rf_model,
  best_auc
)

rf_wflow_best <- workflow() %>%
  add_recipe(preprocess) %>%
  add_model(best_rf_model)

rf_best_model_fitted <- fit(rf_wflow_best, data = train_data )

vip( pull_workflow_fit(rf_best_model_fitted), geom="point" )

# I use the function last fit.This function fits a final model on the entire training set and evaluates on the testing set. 
# We just need to give this function our original train/test split.

final_res <- rf_wflow_best %>%
  last_fit(split)

final_res %>%
  collect_metrics()

# Prediction on test data. 
# I just want to check if I get different outputs then with last fit
custom_predict <- function(object, newdata) {predict(object, newdata)$.pred_class}

pred_rf <- tibble( truth=test_data$status, 
                   prediction=custom_predict(rf_best_model_fitted, test_data ) )

## Assessment -- test error
metrics(pred_rf, truth = truth, estimate = prediction)

conf_mat(pred_rf, truth = truth, estimate = prediction) %>% 
         autoplot( "heatmap")

pred <- rf_best_model_fitted %>% 
  predict( test_data, type = "prob") %>% 
  bind_cols(test_data %>% dplyr::select(status) )

pred %>% 
  roc_curve(truth = status, .pred_Neg ) %>% 
  autoplot()

#
# Regression
#

# Splitting
split      <- initial_split(data.total %>% dplyr::select( -status ), prop = 3/4 )
train_data <- training(split)
test_data  <- testing(split)

# I do the same tuning procedure for this model
preprocess_frac <- recipe(frac_pos ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -frac_pos ) %>%
  step_nzv( all_predictors() )
  step_corr(all_numeric())
  
rf_model_frac <- 
  rand_forest(trees = tune(), mtry = tune(), mode = "regression") %>%
  set_engine("randomForest", importance = T )


rf_param_frac <- rf_model_frac %>%
  parameters() %>%
  update(mtry = mtry(c(1L, 10L))) %>%
  finalize( select( data, -status, -frac_pos ) )

rf_grid_frac <- rf_param_frac %>%
  grid_max_entropy(size = 10)

rf_wflow_frac <- workflow() %>%
  add_recipe(preprocess_frac) %>%
  add_model(rf_model_frac)

cv_splits_frac  <- vfold_cv(train_data, v = 10, strata = frac_pos )

rf_tuned_frac <- tune_grid(rf_wflow_frac, cv_splits_frac, param_info=rf_param_frac, grid=rf_grid_frac) 
rf_tuned_frac

rf_tuned_frac %>%
  collect_metrics() %>%filter(.metric == "rmse") %>%
  select(mean, trees, mtry) %>%
  pivot_longer(trees:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

# choose the best model
best_auc <- select_best(rf_tuned_frac, "rmse")

# use the finalize_model function to select the last model
best_rf_model_frac <- finalize_model(
  rf_model_frac,
  best_auc
)

# update the workflow with the best model

rf_wflow_frac_best <- workflow() %>%
  add_recipe(preprocess_frac) %>%
  add_model(best_rf_model_frac)

rf_best_model_frac_fitted <- fit(rf_wflow_frac_best, data = train_data )

vip( pull_workflow_fit(rf_best_model_frac_fitted), geom="point" )

# I use the function last fit.This function fits a final model on the entire training set and evaluates on the testing set. 
# We just need to give this function our original train/test split.

final_res_frac <- rf_wflow_frac_best %>%
  last_fit(split)

final_res_frac %>%
  collect_metrics()


pred_rf_frac <- tibble( truth=test_data$frac_pos, 
                   prediction=predict(rf_best_model_frac_fitted, test_data )$.pred )

p <- ggplot( pred_rf_frac )
p <- p + geom_point( aes( x=truth, y=prediction) )
p <- p + geom_smooth( aes( x=truth, y=prediction), method="lm" )
p

ggsave( filename=str_c("./figures/randomforest/", "prediction_rf.png", sep=""), plot=p )

#rf_fit <- 
#  rand_forest(trees = 5000, mtry = 10, mode = "regression") %>%
  #set_engine("ranger", seed = 63233) %>%
#  set_engine("rand_forest", seed = 63233) %>%
#  fit(frac_pos ~ ., data = train_data )

#rf_fit

#pred.df <- tibble( truth=test_data$frac_pos, 
#                   prediction=predict(rf_fit, test_data)$.pred )

#p <- ggplot( pred.df )
#p <- p + geom_point( aes( x=truth, y=prediction) )
#p <- p + geom_smooth( aes( x=truth, y=prediction), method="lm" )
#p

#ggsave( filename=str_c("./figures/randomforest/", "prediction.png", sep=""), plot=p )
