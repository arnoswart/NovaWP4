library( sf )
library( rgdal )
library( raster )
library( readxl )
library( tidyverse )
library( mc2d )
library( here )

# The ML 'tidymodels' collection
library( rsample )
library( parsnip )
library( ranger )
library( yardstick )
library( vip )

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

save.image( "all.maps.extracted.meas.RData" )
load( "all.maps.extracted.meas.RData" )

# Make up a relationship
# Collect the variables you want to stop in the relationship
covariates <- cbind(meanT = all.maps.extract.meas$Mean_temp_2011_2016.1, boars = all.maps.extract.meas$boars,denshunt = all.maps.extract.meas$Dens_hunt) %>%
              as.data.frame() %>%
              filter(!is.na(meanT)) %>%
              filter(!is.na(boars)) %>% # remove lines with na in the boars
              filter(!is.na(denshunt))

# Relationship: p(positive) = logit (beta0 + beta1*maxT + beta2*boars +beta3*density_hunting)
beta0 <- -3.5
beta1 <- 0.8
beta2 <- 2.5
beta3 <- -30

y_pos <- beta0 +beta1*covariates$meanT + 
  beta2*covariates$boars + beta3*covariates$denshunt + rnorm(nrow(covariates)) 

# Logistic function
p_pos <- exp(y_pos) / (1 + exp(y_pos))

# Plot the probability p_pos
plot(p_pos[order(p_pos)])

# Extract the positive from a Bernoulli distribution
POS   <- rbern(nrow(meas_points), p_pos)

# Add the status (0 = negative, 1 = positive) to the dataframe of the measurements
sf_meas_points <-  sf_meas_points %>% 
                   mutate( positiveN = POS )%>% 
                   mutate( sampleN = rep(1, nrow(sf_meas_points)))%>%
                   filter(!is.na(positiveN))
            
# Spatially join the measurements points and the municipality
sf_meas_db <- st_join(muniSpain,sf_meas_points,left =FALSE) %>%  # inner join
              rename(muniID = ID_4) %>%
              group_by( muniID ) %>%
              summarize( status = ifelse( sum(positiveN>0), "Pos", "Neg" ),
              frac_pos = sum(positiveN)/sum(sampleN)) %>%
              dplyr::select( code=muniID, status, frac_pos ) %>%
              mutate( status = as.factor( status ) )

plot.shape( sf_meas_db, adm.sf, my_fill=status, NULL, filename=NA, "Status of fake measurements" )
plot.shape( sf_meas_db, adm.sf, my_fill=frac_pos, NULL, filename=NA, "Fraction positives of fake measurements" )

# Calculate the centroids and create buffers around the centroids 
fake_status_centroids.sf <- st_centroid( sf_meas_db ) %>% dplyr::select( status, frac_pos, geometry )
fake_status_buffers.sf <- st_buffer( fake_status_centroids.sf, dist=0.1 )

plot.shape(fake_status_buffers.sf, adm.sf, my_fill = status, my_color = status, filename=NA, "Status of fake measurements in buffers" )
plot.shape(fake_status_buffers.sf, adm.sf, my_fill = frac_pos, my_color = frac_pos, filename=NA, "Fraction of positives in buffers" )

# Test the model
# Extract again the covariates now to match the size of the dataframe of the buffers

all.maps.extract.mod <- raster::extract(all.maps, fake_status_buffers.sf, fun=mean, na.rm=TRUE, df=TRUE )

save.image( "all.maps.extracted.mod.RData" )
load( "all.maps.extracted.mod.RData" )

data.total <- cbind( as.data.frame(fake_status_centroids.sf) %>% dplyr::select( status, frac_pos ),
                     all.maps.extract.mod %>% dplyr::select( -ID )) %>%
  na.omit()

p <- ggplot( data.total  %>%
               pivot_longer(-status, "riskfactor" ) )
p <- p + geom_histogram( aes( value, fill=status), position = "dodge" )
p <- p + facet_wrap( ~riskfactor, scales="free" )
p

# Splitting
set.seed(31415)
split      <- initial_split(data.total %>% 
                              dplyr::select( -frac_pos ), prop = 3/4, strata = status )
train_data <- training(split)
test_data  <- testing(split)

# TODO: 'dials' for parameter optimization (https://www.hvitfeldt.me/blog/authorship-classification-with-tidymodels-and-textrecipes)
# TODO: this site explains recipes: https://www.r-bloggers.com/tidytuesday-hotel-bookings-and-recipes/

preprocess <- recipe(status ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -status ) %>%
  step_nzv( all_predictors() )

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

cv_splits  <- vfold_cv(train_data, v = 3, strata = status )

rf_tuned <- tune_grid(rf_wflow, cv_splits, param_info=rf_param, grid=rf_grid, 
                      metrics = metric_set( roc_auc, accuracy ) ) %>%
  show_best( metric = "roc_auc" )

best_rf_model <- rand_forest(mode = "classification", mtry = rf_tuned$mtry[1],
                             trees = rf_tuned$trees[1]) %>%
  set_engine("randomForest")

rf_wflow_best <- workflow() %>%
  add_recipe(preprocess) %>%
  add_model(best_rf_model)

rf_best_model_fitted <- fit(rf_wflow_best, data = train_data )

vip( pull_workflow_fit(rf_best_model_fitted), geom="point" )

# Prediction on test data
custom_predict <- function(object, newdata) {predict(object, newdata)$.pred_class}

pred_rf <- tibble( truth=test_data$status, 
                   prediction=custom_predict(rf_best_model_fitted, test_data ) )

## Assessment -- test error
metrics(pred_rf, truth = truth, estimate = prediction)

conf_mat(pred_rf, truth = truth, estimate = prediction) %>% 
  autoplot( "heatmap")

#
# Regression
#

# Splitting
split      <- initial_split(data.total %>% dplyr::select( -status ), prop = 3/4 )
train_data <- training(split)
test_data  <- testing(split)

rf_fit <- 
  rand_forest(trees = 5000, mtry = 10, mode = "regression") %>%
  set_engine("ranger", seed = 63233) %>%
  fit(frac_pos ~ ., data = train_data )

rf_fit

pred.df <- tibble( truth=test_data$frac_pos, 
                   prediction=predict(rf_fit, test_data)$.pred )

p <- ggplot( pred.df )
p <- p + geom_point( aes( x=truth, y=prediction) )
p <- p + geom_smooth( aes( x=truth, y=prediction), method="lm" )
p
