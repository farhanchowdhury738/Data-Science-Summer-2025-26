suppressWarnings(suppressMessages({
  required_packages <- c("quanteda", "quanteda.textmodels", "e1071", "glmnet", "stopwords")
  packages_to_install <- required_packages[!required_packages %in% rownames(installed.packages())]
  if (length(packages_to_install) > 0) {
    install.packages(packages_to_install, repos = "https://cloud.r-project.org")
  }
  library(quanteda)
  library(quanteda.textmodels)
  library(e1071)
  library(glmnet)
}))

set.seed(42)

positive_class <- "Hallucinated"
class_levels <- c("Faithful", "Hallucinated")
test_set_ratio <- 0.20
minimum_term_frequency <- 2
minimum_document_frequency <- 2




dataset_file <- "C:/Users/Farhan/Downloads/archive/Hallucination_data.csv"

raw_dataset <- read.csv(
  dataset_file,
  header = TRUE,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

names(raw_dataset) <- trimws(names(raw_dataset))
names(raw_dataset)[match(c("Reference Answer", "LLM Answer", "Label"), names(raw_dataset))] <-
  c("reference_answer", "llm_answer", "label")

qa_data <- raw_dataset[, c("ID", "Question", "Passage", "reference_answer", "llm_answer", "label")]
qa_data$label <- factor(trimws(qa_data$label), levels = class_levels)
qa_data <- qa_data[!is.na(qa_data$label), ]




dataset_row_and_column_count <- dim(qa_data)
dataset_row_and_column_count

missing_or_empty_values_per_column <- colSums(is.na(qa_data) | qa_data == "")
missing_or_empty_values_per_column

label_distribution_counts <- table(qa_data$label)
label_distribution_counts

label_distribution_proportions <- round(prop.table(label_distribution_counts), 4)
label_distribution_proportions

duplicate_qa_row_count <- sum(duplicated(qa_data[, c("Question", "reference_answer", "llm_answer", "label")]))
duplicate_qa_row_count





qa_data$reference_equals_llm_answer <- as.integer(trimws(qa_data$reference_answer) == trimws(qa_data$llm_answer))
reference_answer_match_versus_label <- table(reference_equals_llm_answer = qa_data$reference_equals_llm_answer,
                                             label = qa_data$label)
reference_answer_match_versus_label

qa_data$answer_pair_text <- paste(qa_data$reference_answer, qa_data$llm_answer)
qa_data$answer_pair_character_length <- nchar(qa_data$answer_pair_text)
qa_data$answer_pair_word_count <- lengths(strsplit(qa_data$answer_pair_text, "\\s+"))

answer_pair_character_length_summary <- summary(qa_data$answer_pair_character_length)
answer_pair_character_length_summary

answer_pair_word_count_summary <- summary(qa_data$answer_pair_word_count)
answer_pair_word_count_summary

label_distribution_bar_positions <- barplot(label_distribution_counts,
                                            col = c("#4C9F70", "#D1495B"),
                                            ylim = c(0, max(label_distribution_counts) * 1.15),
                                            main = "Label Distribution", ylab = "Number of QA pairs")
text(label_distribution_bar_positions, label_distribution_counts,
     labels = label_distribution_counts, pos = 3, font = 2)

boxplot(answer_pair_word_count ~ label, data = qa_data, col = c("#4C9F70", "#D1495B"),
        main = "Answer-pair Word Count by Label", xlab = "Label", ylab = "Words")




normalize_bangla_text <- function(text_vector) {
  text_vector <- as.character(text_vector)
  text_vector <- gsub("[[:cntrl:]]", " ", text_vector, perl = TRUE)
  text_vector <- gsub("[[:punct:]]", " ", text_vector, perl = TRUE)
  text_vector <- gsub("[।॥‘’“”–—]", " ", text_vector)
  text_vector <- gsub("\\s+", " ", text_vector, perl = TRUE)
  trimws(text_vector)
}

qa_data$answer_pair_text_clean <- normalize_bangla_text(qa_data$answer_pair_text)

bangla_stopwords <- tryCatch(stopwords::stopwords("bn", source = "stopwords-iso"),
                             error = function(e) character(0))

answer_pair_corpus <- corpus(qa_data$answer_pair_text_clean)
answer_pair_tokens <- tokens(answer_pair_corpus, remove_punct = TRUE, remove_symbols = TRUE)
answer_pair_tokens <- tokens_remove(answer_pair_tokens, bangla_stopwords)
answer_pair_tokens_unigram_bigram <- tokens_ngrams(answer_pair_tokens, n = 1:2)

sample_tokenized_documents <- sapply(1:5, function(row_index) {
  paste0("[", as.character(qa_data$label[row_index]), "] ",
         paste(as.character(answer_pair_tokens_unigram_bigram[[row_index]]), collapse = " "))
})
sample_tokenized_documents





question_id <- as.integer(factor(qa_data$Question))
unique_question_ids <- sort(unique(question_id))
test_question_ids <- sample(unique_question_ids, floor(test_set_ratio * length(unique_question_ids)))
is_training_row <- !(question_id %in% test_question_ids)

training_tokens <- answer_pair_tokens_unigram_bigram[is_training_row]
test_tokens <- answer_pair_tokens_unigram_bigram[!is_training_row]
training_labels <- qa_data$label[is_training_row]
test_labels <- qa_data$label[!is_training_row]

training_and_test_row_counts <- c(training_rows = sum(is_training_row), test_rows = sum(!is_training_row))
training_and_test_row_counts

training_label_counts <- table(training_labels)
training_label_counts

test_label_counts <- table(test_labels)
test_label_counts




untrimmed_training_bow_features <- dfm(training_tokens)
training_bow_features <- dfm_trim(untrimmed_training_bow_features,
                                  min_termfreq = minimum_term_frequency,
                                  min_docfreq = minimum_document_frequency)
if (nfeat(training_bow_features) < 20) {
  training_bow_features <- dfm_trim(untrimmed_training_bow_features, min_termfreq = 1, min_docfreq = 1)
}
test_bow_features <- dfm_match(dfm(test_tokens), featnames(training_bow_features))

inverse_document_frequency_weights <- docfreq(training_bow_features, scheme = "inverse", base = 10)
training_tfidf_features <- dfm_weight(training_bow_features, weights = inverse_document_frequency_weights)
test_tfidf_features <- dfm_weight(test_bow_features, weights = inverse_document_frequency_weights)

vocabulary_size <- nfeat(training_bow_features)
vocabulary_size

fifteen_most_frequent_terms <- topfeatures(training_bow_features, 15)
fifteen_most_frequent_terms

feature_representations <- list(
  BoW   = list(training = training_bow_features,   test = test_bow_features),
  TFIDF = list(training = training_tfidf_features, test = test_tfidf_features)
)





compute_classification_metrics <- function(actual_labels, predicted_labels) {
  actual_labels <- factor(actual_labels, levels = class_levels)
  predicted_labels <- factor(predicted_labels, levels = class_levels)
  confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
  true_positive  <- confusion_matrix[positive_class, positive_class]
  false_positive <- sum(confusion_matrix[positive_class, ]) - true_positive
  false_negative <- sum(confusion_matrix[, positive_class]) - true_positive
  true_negative  <- sum(confusion_matrix) - true_positive - false_positive - false_negative
  accuracy  <- (true_positive + true_negative) / sum(confusion_matrix)
  precision <- if ((true_positive + false_positive) == 0) 0 else true_positive / (true_positive + false_positive)
  recall    <- if ((true_positive + false_negative) == 0) 0 else true_positive / (true_positive + false_negative)
  f1_score  <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
  list(confusion_matrix = confusion_matrix, accuracy = accuracy,
       precision = precision, recall = recall, f1_score = f1_score)
}





predict_with_naive_bayes <- function(training_features, test_features, training_labels) {
  naive_bayes_model <- textmodel_nb(training_features, training_labels, distribution = "multinomial")
  as.character(predict(naive_bayes_model, newdata = test_features, type = "class"))
}

predict_with_logistic_regression <- function(training_features, test_features, training_labels) {
  training_feature_matrix <- as(training_features, "dgCMatrix")
  test_feature_matrix <- as(test_features, "dgCMatrix")
  set.seed(42)
  logistic_regression_model <- cv.glmnet(training_feature_matrix, training_labels,
                                         family = "binomial", alpha = 1,
                                         nfolds = 10, type.measure = "class")
  as.character(predict(logistic_regression_model, newx = test_feature_matrix,
                       s = "lambda.min", type = "class"))
}

predict_with_svm <- function(training_features, test_features, training_labels) {
  svm_model <- svm(x = as.matrix(training_features), y = training_labels,
                   kernel = "linear", scale = FALSE)
  as.character(predict(svm_model, as.matrix(test_features)))
}

classifier_functions <- list(
  "Naive Bayes"         = predict_with_naive_bayes,
  "Logistic Regression" = predict_with_logistic_regression,
  "SVM"                 = predict_with_svm
)



model_comparison_results <- data.frame()
all_confusion_matrices <- list()

for (representation_name in names(feature_representations)) {
  training_features <- feature_representations[[representation_name]]$training
  test_features <- feature_representations[[representation_name]]$test
  for (classifier_name in names(classifier_functions)) {
    predicted_test_labels <- classifier_functions[[classifier_name]](training_features, test_features, training_labels)
    classification_metrics <- compute_classification_metrics(test_labels, predicted_test_labels)
    model_comparison_results <- rbind(model_comparison_results, data.frame(
      Representation = representation_name,
      Model = classifier_name,
      Accuracy = round(classification_metrics$accuracy, 4),
      Precision = round(classification_metrics$precision, 4),
      Recall = round(classification_metrics$recall, 4),
      F1 = round(classification_metrics$f1_score, 4)
    ))
    all_confusion_matrices[[paste(representation_name, classifier_name, sep = " + ")]] <-
      classification_metrics$confusion_matrix
  }
}

model_comparison_results <- model_comparison_results[order(-model_comparison_results$F1), ]
row.names(model_comparison_results) <- NULL
model_comparison_results

all_confusion_matrices




f1_score_comparison <- reshape(model_comparison_results[, c("Representation", "Model", "F1")],
                               idvar = "Model", timevar = "Representation", direction = "wide")
names(f1_score_comparison) <- gsub("^F1\\.", "", names(f1_score_comparison))
row.names(f1_score_comparison) <- NULL
f1_score_comparison

best_model <- model_comparison_results[which.max(model_comparison_results$F1), ]
best_model





f1_comparison_matrix <- t(as.matrix(f1_score_comparison[, c("BoW", "TFIDF")]))
colnames(f1_comparison_matrix) <- f1_score_comparison$Model

previous_par <- par(mar = c(5, 4, 4, 8), xpd = TRUE)
f1_bar_positions <- barplot(f1_comparison_matrix, beside = TRUE,
                            col = c("#3E7CB1", "#F0A202"), ylim = c(0, 1), cex.names = 0.85,
                            main = "F1-score: BoW vs TF-IDF by Model",
                            xlab = "Classifier", ylab = "F1-score (Hallucinated class)")
text(f1_bar_positions, f1_comparison_matrix,
     labels = sprintf("%.2f", f1_comparison_matrix), pos = 3, cex = 0.8)
legend(x = max(f1_bar_positions) + 0.8, y = 1,
       legend = c("BoW", "TF-IDF"), fill = c("#3E7CB1", "#F0A202"),
       bty = "n", title = "Representation")
par(previous_par)




accuracy_comparison <- reshape(model_comparison_results[, c("Representation", "Model", "Accuracy")],
                               idvar = "Model", timevar = "Representation", direction = "wide")
names(accuracy_comparison) <- gsub("^Accuracy\\.", "", names(accuracy_comparison))
row.names(accuracy_comparison) <- NULL
accuracy_comparison

accuracy_comparison_matrix <- t(as.matrix(accuracy_comparison[, c("BoW", "TFIDF")]))
colnames(accuracy_comparison_matrix) <- accuracy_comparison$Model

previous_par <- par(mar = c(5, 4, 4, 8), xpd = TRUE)
accuracy_bar_positions <- barplot(accuracy_comparison_matrix, beside = TRUE,
                                  col = c("#3E7CB1", "#F0A202"), ylim = c(0, 1), cex.names = 0.85,
                                  main = "Accuracy: BoW vs TF-IDF by Model",
                                  xlab = "Classifier", ylab = "Accuracy")
text(accuracy_bar_positions, accuracy_comparison_matrix,
     labels = sprintf("%.2f", accuracy_comparison_matrix), pos = 3, cex = 0.8)
legend(x = max(accuracy_bar_positions) + 0.8, y = 1,
       legend = c("BoW", "TF-IDF"), fill = c("#3E7CB1", "#F0A202"),
       bty = "n", title = "Representation")
par(previous_par)





model_comparison_results$Config <- paste(model_comparison_results$Representation,
                                         model_comparison_results$Model, sep = " + ")

representation_colors <- c(BoW = "#3E7CB1", TFIDF = "#F0A202")
model_point_shapes <- c("Naive Bayes" = 16, "Logistic Regression" = 17, "SVM" = 15)

point_colors <- representation_colors[model_comparison_results$Representation]
point_shapes <- model_point_shapes[model_comparison_results$Model]

previous_par <- par(mar = c(5, 4, 4, 11), xpd = TRUE)
plot(model_comparison_results$Precision, model_comparison_results$Recall,
     xlim = c(0, 1), ylim = c(0, 1), pch = point_shapes, col = point_colors, cex = 2,
     main = "Precision vs Recall (Hallucinated class)",
     xlab = "Precision", ylab = "Recall")
abline(a = 0, b = 1, lty = 2, col = "grey60")
text(model_comparison_results$Precision, model_comparison_results$Recall,
     labels = model_comparison_results$Config, pos = 3, cex = 0.65, xpd = TRUE)
legend(x = 1.05, y = 1,
       legend = model_comparison_results$Config,
       col = point_colors, pch = point_shapes,
       bty = "n", cex = 0.75, title = "Configuration")
par(previous_par)





best_config_key <- paste(best_model$Representation, best_model$Model, sep = " + ")
best_confusion_matrix <- all_confusion_matrices[[best_config_key]][class_levels, class_levels]


confusion_matrix_colors <- colorRampPalette(c("#FFFFFF", "#D1495B"))(100)

previous_par <- par(mar = c(5, 6, 4, 2))
image(x = 1:length(class_levels), y = 1:length(class_levels),
      z = t(best_confusion_matrix),
      col = confusion_matrix_colors, axes = FALSE,
      xlab = "Actual", ylab = "Predicted",
      main = paste("Confusion Matrix Heatmap -", best_config_key))
axis(1, at = 1:length(class_levels), labels = class_levels)
axis(2, at = 1:length(class_levels), labels = class_levels, las = 1)
for (predicted_index in 1:length(class_levels)) {
  for (actual_index in 1:length(class_levels)) {
    cell_count <- best_confusion_matrix[predicted_index, actual_index]
    text(actual_index, predicted_index, labels = cell_count, cex = 1.6, font = 2,
         col = ifelse(cell_count > max(best_confusion_matrix) / 2, "white", "black"))
  }
}
box()
par(previous_par)



