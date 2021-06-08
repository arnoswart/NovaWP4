library( sf )
library( rgdal )
library( raster )
library( readxl )
library( tidyverse )
library( here )

# The ML 'tidymodels' collection
library( rsample )
library( parsnip )
library( ranger )
library( yardstick )
library( vip )
library( recipes )
library( tune )
library( workflows )
library( dials )

setwd( here() )

source( "./model/globals.R" )
source( "./model/plot.functions.R" )
source( "./model/spatial.functions.R" )

# Salmonella status as a data frame
salm_status_ext.df <- read_xlsx( "./data/original/pig_production_extensive/efsa_breeding_pig_muni_arno_20200519.xlsx") %>%
  mutate(sampleN= rep(1,length(sal_positive)) )  %>% # each row is a sample. Therefore I make a column with ones
  group_by(municipal_code) %>%
   summarize(status = ifelse( sum(sal_positive>0), "Pos", "Neg" ),
             frac_pos = sum(sal_positive)/sum(sampleN)) %>%
  dplyr::select( code=municipal_code, status, frac_pos ) %>%
  mutate( status = as.factor( status ) )

# Salmonella status as a map
salm_status_ext.sf <- read_sf( dsn="./data/original/spain_municipality_shape/", layer="spldf_muni") %>% 
  st_transform( crs.default ) %>%
  st_crop( bbox.default ) %>% 
  left_join( salm_status_ext.df )%>% 
  filter( !is.na(status) )

# load the shape of Spain for plotting purposes only
shapeSpain <- read_sf( dsn="./data/original/ESP_adm/", layer="ESP_adm2") %>%
  st_transform( crs.default) %>%
  st_crop( bbox.default )


plot.shape( salm_status_ext.sf, shapeSpain, my_fill=status, my_color=NULL, filename="salm_status_ext", "Salmonella status in extensive farms grouped by municipalities")
plot.shape( salm_status_ext.sf, shapeSpain, my_fill=frac_pos, my_color=NULL, filename="salm_fracpos_ext", "Fraction of positive salmonella in extensive farms grouped by municipalities")

# Obtain the centroids and buffers
salm_status_centroids_ext.sf <- st_centroid(salm_status_ext.sf ) %>% dplyr::select( status, frac_pos, geometry )
salm_status_buffers_ext.sf <- st_buffer(salm_status_centroids_ext.sf, dist=0.1 )

plot.shape(salm_status_centroids_ext.sf, shapeSpain, my_fill = status, my_color = status, filename="salm_status_buffers_ext", "Salmonella status in extensive farms in buffers" )
plot.shape(salm_status_centroids_ext.sf, shapeSpain, my_fill = frac_pos, my_color = frac_pos, filename="salm_frac_pos_buffers_ext", "Fraction of positives salmonella in extensive farms in buffers" )

# Get all covariates
all.maps <- addLayer( brick( "./data/processed/all.maps.grd" ),
                      brick( "./data/processed/corine/corine.grd" ),
                      brick( "./data/processed/climate/climate.grd" ))

all.maps.extract_ext <- raster::extract(all.maps, salm_status_buffers_ext.sf, fun=mean, na.rm=TRUE, df=TRUE )

save.image( "all.maps.extracted_ext.RData" )
load( "all.maps.extracted_ext.RData" )

data.total_ext <- cbind( as.data.frame(salm_status_centroids_ext.sf) %>% dplyr::select( status, frac_pos ),
                     all.maps.extract_ext %>% dplyr::select( -ID )) %>%
  na.omit()

p <- ggplot( data.total_ext  %>%
               pivot_longer(-status, "riskfactor" ) )
p <- p + geom_histogram( aes( value, fill=status), position = "dodge" )
p <- p + facet_wrap( ~riskfactor, scales="free" )
p

# Splitting
set.seed(31415)
split      <- initial_split(data.total_ext %>% 
                              dplyr::select( -frac_pos ), prop = 3/4, strata = status )
train_data <- training(split)
test_data  <- testing(split)

# TODO: 'dials' for parameter optimization (https://www.hvitfeldt.me/blog/authorship-classification-with-tidymodels-and-textrecipes)
# TODO: this site explains recipes: https://www.r-bloggers.com/tidytuesday-hotel-bookings-and-recipes/

preprocess <- recipe(status ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -status ) %>%
  step_nzv( all_predictors() ) %>%
  step_corr(all_numeric())

rf_model <- 
  rand_forest(trees = tune(), mtry = tune(), mode = "classification") %>%
  set_engine("randomForest", importance = T )

rf_param <- rf_model %>%
  parameters() %>%
  update(mtry = mtry(c(1L, 10L)),
         trees=trees(c(100,5000))) %>%
  finalize( select( data, -status, -frac_pos ) )

rf_grid <- rf_param %>%
  grid_max_entropy(size = 10)

rf_wflow <- workflow() %>%
  add_recipe(preprocess) %>%
  add_model(rf_model)

cv_splits  <- vfold_cv(train_data, v = 10, strata = status )

rf_tuned <- tune_grid(rf_wflow, cv_splits, param_info=rf_param, grid=rf_grid, 
                      metrics = metric_set( roc_auc, accuracy ) ) #%>%
 # show_best( metric = "roc_auc" )

rf_tuned

# have a look at the tuning
rf_tuned %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, trees, mtry) %>%
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

#best_rf_model <- rand_forest(mode = "classification", mtry = rf_tuned$mtry[1],
#                             trees = rf_tuned$trees[1]) %>%
#  set_engine("randomForest")

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
# I just want to check if I get different outputs than with last fit
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
split      <- initial_split(data.total_ext %>% dplyr::select( -status ), prop = 3/4 )
train_data <- training(split)
test_data  <- testing(split)

# I do the same tuning procedure for this model
preprocess_frac <- recipe(frac_pos ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -frac_pos ) %>%
  step_nzv( all_predictors() ) %>%
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
