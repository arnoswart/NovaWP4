library( sf )
library( rgdal )
library( raster )
library( readxl )
library( tidyverse )
library( here )

# The ML 'tidymodels' collection
library( tidymodels )
library( vip )

setwd( here() )

source( "./model/globals.R" )
source( "./model/plot.functions.R" )
source( "./model/spatial.functions.R" )

# Salmonella status as a data frame
salm_status.df <- read_xlsx( "./data/original/farmSalmonellaStatus/Arno_SalSpainMunicipalLevel_1farm1sample_20200128.xlsx") %>%
  filter( year >= 2007 ) %>%
  group_by( muniID ) %>%
  summarize( status = ifelse( sum(positiveN>0), "Pos", "Neg" ),
             frac_pos = sum(positiveN)/sum(sampleN)) %>%
  dplyr::select( code=muniID, status, frac_pos ) %>%
  mutate( status = as.factor( status ) )

# Salmonella status as a map
salm_status.sf <- read_sf( dsn="./data/original/spain_municipality_shape/", layer="spldf_muni") %>% 
  st_transform( crs.default ) %>%
  st_crop( bbox.default ) %>% 
  left_join( salm_status.df )%>% 
  filter( !is.na(status) )

plot.shape( salm_status.sf, NULL, my_fill=status, my_color=NULL, filename="salm_status", "Municipalities")
plot.shape( salm_status.sf, NULL, my_fill=frac_pos, my_color=NULL, filename="salm_status", "Municipalities")

# Obtain the centroids and buffers
salm_status_centroids.sf <- st_centroid(salm_status.sf ) %>% dplyr::select( status, frac_pos, geometry )
salm_status_buffers.sf <- st_buffer(salm_status_centroids.sf, dist=0.1 )
plot( salm_status_buffers.sf )

# Get all covariates
all.maps <- addLayer( brick( "./data/processed/all.maps.grd" ),
                      brick( "./data/processed/corine/corine.grd" ),
                      brick( "./data/processed/climate/climate.grd" ))

all.maps.extract <- raster::extract(all.maps, salm_status_buffers.sf, fun=mean, na.rm=TRUE, df=TRUE )

save.image( "./data/generated/all.maps.extracted.RData" )
load( "./data/generated/all.maps.extracted.RData" )

data.total <- cbind( as.data.frame(salm_status_centroids.sf) %>% dplyr::select( status, frac_pos ),
                     all.maps.extract %>% dplyr::select( -ID )) %>%
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
