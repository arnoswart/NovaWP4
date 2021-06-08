library( sf )
library( rgdal )
library( raster )
library( readxl )
library( tidyverse )
library( workflowsets )
library( here )
library( tidymodels )

setwd( here() )

source( "./model/globals.R" )
source( "./model/plot.functions.R" )
source( "./model/spatial.functions.R" )

path_wildb_sal <- file.path(".", "data", "original","wild_boar_sal_result")

wildboar.sal.sf <- read_sf( dsn=path_wildb_sal, layer="POSALL_DATA_reproject") %>% 
  st_transform( crs.default ) %>% 
  st_crop( bbox.default ) %>% 
  dplyr::filter( Salmonella %in% c("Negativo", "Positivo")) %>% 
  dplyr::group_by( MATRICULA ) %>% 
  dplyr::summarize( sal_pos = ifelse( any( Salmonella=="Positivo"), "pos", "neg" ) %>% 
                      as.factor(),
                    frac_pos = mean( Salmonella=="Positivo")) %>% 
  dplyr::select( sal_pos, frac_pos, geometry )

ggplot(wildboar.sal.sf ) +
          geom_sf( aes( color= frac_pos)) +
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.title=element_text(size=6),
         legend.text=element_text(size=6),
         legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.3,"cm"))

# load the shape of Spain for plotting purposes only
shapeSpain <- read_sf( dsn="./data/original/ESP_adm/", layer="ESP_adm2") %>%
  st_transform( crs.default) %>%
  st_crop( bbox.default )

plot.shape( wildboar.sal.sf, shapeSpain, my_fill=NULL, my_color=sal_pos,title="Salmonella status in wild boar")

# Obtain the centroids and buffers
salm_status_centroids.sf <- st_centroid(wildboar.sal.sf ) %>% 
  dplyr::select( sal_pos, frac_pos, geometry )

salm_status_buffers.sf <- st_buffer(salm_status_centroids.sf, dist=0.1 )

plot.shape(salm_status_buffers.sf, shapeSpain, my_color = frac_pos, 
           title="Salmonella status in wild boar in buffers" )

# Get all covariates
all.maps <- addLayer( brick( "./data/processed/all.maps.grd" ),
                      brick( "./data/processed/corine/corine.grd" ),
                      brick( "./data/processed/climate/climate.grd" ))

all.maps.extract <- raster::extract(all.maps, salm_status_buffers.sf, 
                                          fun=mean, na.rm=TRUE, df=TRUE )
save.image( "./data/generated/all.maps.extracted.wildboar.RData" )

data.total <- cbind( as.data.frame(salm_status_centroids.sf) %>% 
                             dplyr::select( sal_pos, frac_pos ),
                           all.maps.extract %>% dplyr::select( -ID )) %>%
  na.omit()

# Status
data.total  %>%
  pivot_longer(-c(sal_pos,frac_pos), "riskfactor" ) %>%
  ggplot(  ) +
    geom_histogram( aes( value, fill=sal_pos), position = "dodge" ) +
    facet_wrap( ~riskfactor, scales="free" )

# Fractions
data.total %>%
  pivot_longer(-c(sal_pos,frac_pos), "riskfactor" ) %>%
  ggplot(  ) +
  geom_point( aes( value, frac_pos) ) +
  facet_wrap( ~riskfactor, scales="free" )


####
# ML
####

# Splitting
set.seed(314)
split      <- initial_split(data.total %>% select(-frac_pos), prop = 3/4, strata = sal_pos )
train_data <- training(split)
test_data  <- testing(split)

preprocess <- recipe(sal_pos ~ ., data = train_data ) %>%
  step_dummy(all_nominal(), -sal_pos ) %>%
  step_nzv( all_predictors() ) %>%
  step_corr(all_numeric()) %>% 
  step_normalize(all_numeric() )

rf_model <- 
  rand_forest(trees = 3000, mtry = 5, mode = "classification") %>%
  set_engine("randomForest", importance = T )

  
glm_model <- logistic_reg() %>% 
  set_engine("glm")

boost_model <- boost_tree(
  mode = "classification", mtry = 5, trees = 300 ) %>% 
  set_engine( "xgboost")

wfs <- workflow_set(
  preproc = list(classification_recipe = preprocess),
  models = list(rf_model, boost_model, glm_model ),
  cross = TRUE )

folds <- vfold_cv(train_data, v=4 )
wfs_cv <- 
  wfs %>% 
  workflow_map( "fit_resamples", resamples=folds )

wfs_cv %>% 
  rank_results(rank_metric = "roc_auc")

autoplot(wfs_cv, metric="roc_auc", event_level="second" )

best_id <- wfs_cv %>% 
  rank_results( rank_metric = "roc_auc", select_best = TRUE ) %>%
  filter( .metric=="roc_auc" )
best_id <- best_id[[1,1]]

best_fit <- wfs %>%
  pull_workflow( best_id ) %>% 
  fit(data = train_data)
best_fit

best_fit %>% 
  pull_workflow_fit() %>% 
  vip( geom="point" )

# Prediction on test data. 
pred <- best_fit %>% 
  predict( test_data, type = "prob") %>% 
  bind_cols(test_data %>% dplyr::select(sal_pos) ) 

pred %>% 
  roc_curve(truth = sal_pos, .pred_pos, event_level="second"  ) %>% 
  autoplot()

pred %>% 
  roc_auc(truth = sal_pos, .pred_pos, event_level="second")

pred <- 
  predict(best_fit, test_data, type = "class") %>% 
  bind_cols(test_data %>% dplyr::select(sal_pos)) 

pred %>% 
  metrics( truth = sal_pos, estimate = .pred_class )

pred %>% 
  conf_mat(truth = sal_pos, estimate = .pred_class ) %>% 
  autoplot( "heatmap")

####
# ML
####

# Splitting
set.seed(3154)
split      <- initial_split(data.total %>% dplyr::select(-sal_pos), prop = 3/4 )
train_data <- training(split)
test_data  <- testing(split)

preprocess <- recipe(frac_pos ~ ., data = train_data ) %>%
  step_dummy(all_nominal() ) %>%
  step_nzv( all_predictors() ) %>%
  step_corr(all_predictors()) %>% 
  step_normalize(all_predictors() )

rf_model <- 
  rand_forest(trees = 3000, mtry = 6, mode = "regression") %>%
  set_engine("randomForest", importance = T )

lm_model <- linear_reg() %>% 
  set_engine("lm")

boost_model <- boost_tree(
  mode = "regression", mtry = 6, trees = 3000 ) %>% 
  set_engine( "xgboost")

wfs <- workflow_set(
  preproc = list(classification_recipe = preprocess),
  models = list(rf_model, boost_model, lm_model ),
  cross = TRUE )

folds <- vfold_cv(train_data, v=4 )
wfs_cv <- 
  wfs %>% 
  workflow_map( "fit_resamples", resamples=folds )

wfs_cv %>% 
  rank_results( )

autoplot(wfs_cv, metric="rmse" )

best_id <- wfs_cv %>% 
  rank_results( rank_metric = "rmse", select_best = TRUE ) %>%
  filter( .metric=="rmse" )
best_id <- best_id[[1,1]]

best_fit <- wfs %>%
  pull_workflow( best_id ) %>% 
  fit(data = train_data)
best_fit

best_fit %>% pull_workflow_fit() %>% vip( geom="point" )

# Prediction on test data. 
pred <- best_fit %>% 
  predict( test_data ) %>% 
  bind_cols(test_data %>% dplyr::select(frac_pos) ) 

ggplot(pred, aes(x = frac_pos, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted", x = "Observed") +
  coord_obs_pred()
