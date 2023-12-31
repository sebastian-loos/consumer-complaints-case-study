# Create a synthetic dataset
set.seed(123)
products <- c("mortgage", "student loan", "car loan", "credit card")
ids <- 1:20
complaints <- c(
  "The mortgage terms 0.00 are not what I expected. Very disappointed.",
  "I have a __ student loan 94.05, and the interest rates are too high.",
  "My car loan 1900.00 application was denied without proper explanation.",
  "I was charged incorrectly on my credit card statement.",
  "Mortgage payments keep increasing unexpectedly.",
  "Student loan servicer is unresponsive to my requests.",
  "Car loan process was slow and frustrating.",
  "Unauthorized transactions on my credit card account.",
  "Mortgage company failed to provide accurate documentation.",
  "Student loan payments are causing financial strain.",
  "Car loan interest rates are too high compared to others.",
  "Credit card billing error not resolved despite multiple calls.",
  "Mortgage application process was smooth and efficient.",
  "Received a notice of default on my student loan.",
  "Car loan customer service was rude and unhelpful.",
  "Credit card rewards program is confusing to understand.",
  "Mortgage statement does not reflect my payments correctly.",
  "Student loan grace period was not properly communicated.",
  "Car loan terms were changed without prior notice.",
  "Credit card annual fee is too expensive for the benefits."
)

complaint_df <- tibble(
  complaint_id = as.factor(ids),
  product = sample(products, 20, replace = TRUE),
  complaint = complaints) |> 
  mutate(product = as_factor(product))

# Load the data into a corpus
one_word <- complaint_df |> 
  unnest_tokens(word, complaint) |> 
  filter(!word %in% stop_words$word)

dtm <- one_word |> 
  count(complaint_id, word) |>
  cast_dtm(document = complaint_id, term = word, value = n)
  
inspect(dtm)

# Assuming you have a DTM named 'dtm' and 'complaint_df' dataset
data_matrix <- as.matrix(dtm)
complaints_modeling <- cbind(complaint_df |> select(-complaint), data_matrix) |> 
  as_tibble()

complaints_modeling

# complaints_counts <- complaints_df |> 
#   count(word, .by = product, sort = TRUE) |>
#   rename(product = .by) |>
#   cast_dtm(document = product, term = word, value = n) |> 
#   tidy() |> 
#   rename(word = term, product = document)
# 
# count_join <- complaints_counts |>
#   left_join(complaints_train_otpdpr, by = join_by(product, word)) |> 
#   unique()
# 
# complaints_simlpe <- complaints_train |>
#   select(!consumer_complaint_narrative) |> 
#   mutate("id_complaint" = row_number(), .after = product)
# 
# complaints_joined <- count_join |>
#   left_join(complaints_simlpe, by = join_by(id_complaint, product))
#   
#   # Create a Document-Term Matrix (DTM)
#   cast_dtm(product, word, id)
# 
# tidy_dtm <- dtm |> tidy()
# 
# wider_complaints <- tidy_dtm |> pivot_wider(names_from = term, values_from = count)


# Step 1: Split into training and testing sets

# ensure the split is done the same if code is rerun
set.seed(1234)
# split data set into test and training
split_complaints <- initial_split(complaints_modeling, prop = 2/3) 
split_complaints

train_complaints <- training(split_complaints)
head(train_complaints)
count(train_complaints, product)

test_complaints <-testing(split_complaints)
head(test_complaints)
count(test_complaints, product)

# Step 1.2 Split training set into cross validation sets

set.seed(1234)
vfold_complaints <- rsample::vfold_cv(data = train_complaints, v = 4)
vfold_complaints

pull(vfold_complaints, splits)

# Explore one of the folds
first_fold <-vfold_complaints$splits[[1]]
head(as.data.frame(first_fold, data = "analysis")) # training set of this fold

head(as.data.frame(first_fold, data = "assessment")) # test set of this fold


### Step 2: Create recipe with recipe()

complaints_recipe <- train_complaints %>%
  recipes::recipe(product ~ .) 
  # recipes::recipe(document ~ count + company + state) 

complaints_recipe

summary(complaints_recipe)


### Step 3: Specify model, engine, and mode (parsnip)


complaints_model <- 
  # define Model (Classification And Regression Tree (CART))
  parsnip::decision_tree() %>%
  # set mode (classification)
  parsnip::set_mode("classification") %>%
  #set engine (rpart)
  parsnip::set_engine("rpart")

complaints_model



#### Specify hyperparameters to tune (tune())



### Step 4: Create workflow, add recipe, add model


prod_comp_wflow <-workflows::workflow() %>%
  workflows::add_recipe(complaints_recipe) %>%
  workflows::add_model(complaints_model)

prod_comp_wflow


### Step 5.1 Fit workflow with cross validation


prod_comp_wflow_fit <- parsnip::fit(prod_comp_wflow, data = train_complaints)

prod_comp_wflow_fit

# store fit
wf_fit_comp <- prod_comp_wflow_fit %>% 
  extract_fit_parsnip()

wf_fit_comp

# wf_fit_comp$fit$variable.importance

#### Explore Variable importance using vip()

prod_comp_wflow_fit %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 10)

### Step 5.2: Fit workflow with cross validation


set.seed(122)
resample_fit <- tune::fit_resamples(prod_comp_wflow, vfold_complaints)


### Step 5.3: For workflow with tuning


reasmple_fit <-tune::tune_grid(prod_comp_wflow_tune, resamples = vfold_complaints, grid = 4)

tune::collect_metrics(resample_fit)

tune::show_best(resample_fit, metric = "accuracy")


### Step 6: Get predictions


pred_documents <- predict(prof_comp_wflow_fit, new_data = train_complaints)


yardstick::accuracy(train_complaints, 
                    truth = documents, estimate = pred_documents$.pred_class)

count(train_complaints, Species)
count(pred_documents, .pred_class)

predicted_and_truth <- bind_cols(train_complaints, 
                                 predicted_documents = pull(pred_documents, .pred_class))

head(predicted_and_truth)

filter(predicted_and_truth, documents != predicted_documents)



set.seed(122)
resample_fit <- tune::fit_resamples(prod_comp_wflow, vfold_complaints)

resample_fit

collect_metrics(resample_fit)





final_workflow <- finalize_workflow(my_wf, my_best_model)
final_model <- fit(final_workflow, my_training_data)
my_results <- predict(final_model, my_testing_data )

my_results %>% knitr::kable( caption = "My Predictions")
