CVgeneric = function(classifier="all_0", features=features_list, labels=labels_list, K=5, loss=accuracy){
  scores = c()
  fold = 1
  
  for (x in 1:K) {
    cv_train_features = do.call("rbind", features_list[-seq(9*fold - 8, 9*fold)])
    cv_test_features = do.call("rbind", features_list[seq(9*fold - 8, 9*fold)])
    
    cv_train_labels = unlist(labels_list[-seq(9*fold - 8, 9*fold)])
    cv_test_labels = unlist(labels_list[seq(9*fold - 8, 9*fold)])
    
    
    cv_train_features$label = cv_train_labels
    cv_test_features$label = cv_test_labels
    
    if (classifier == "all_0") {
      pred = all_0(cv_test_features)
    }
    
    if (classifier == "logistic") {
      model = glm(data=cv_train_features, formula = label ~ NDAI + log_SD + CORR, family = binomial(link="logit"))
      pred = round(predict(model, newdata=cv_test_features, type="response"))
    }
    
    if (classifier == "lda") {
      lda = lda(formula = label ~ NDAI + log_SD + CORR, data = cv_train_features)
      pred_probs = predict(lda, newdata = cv_test_features, type="response")
      pred = as.integer(pred_probs$class) - 1
    }
    
    if (classifier == "qda") {
      qda = qda(formula = label ~ NDAI + log_SD + CORR, data = cv_train_features)
      pred_probs = predict(qda, newdata = cv_test_features, type="response")
      pred = as.integer(pred_probs$class) - 1
    }
    
    
    if (classifier == "xgboost") {
      xgboost = xgboost(data = as.matrix(cv_train_features[,c("NDAI", "log_SD", "CORR")]), label = as.matrix(cv_train_features$label), max.depth = 5, eta = 1,
                        nrounds = 2, objective = "binary:logistic", verbose = 0)
      pred_probs = predict(xgboost, newdata = as.matrix(cv_test_features[,c("NDAI", "log_SD", "CORR")]), type="response")
      pred = round(pred_probs)
    }
    
    actual = cv_test_labels
    scores = c(scores, accuracy(pred, actual))
    fold = fold + 1
  }
  print("Accuracy for each of K Folds:")
  return(scores)
}