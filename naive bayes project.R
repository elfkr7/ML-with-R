library(data.table) # to handle the data in a more convenient manner
library(tidyverse) # for a better work flow and more tools to wrangle and visualize the data
library(tm) # for text mining
library(SnowballC) # for word stemming
library(gridExtra) # for multiple plots
library(wordcloud) # visualize text data
library(RColorBrewer) # for beautifying visualizations with custom colors
library(e1071) # for naive bayes
library(gmodels) # model evaluation
library(knitr) # for better table printing
library(kableExtra) # for better table printing
library(scales) # for formatting numbers
library(magrittr) # tools for better handling data structures
library(purrr) # tools for better handling data structures
library(IRdisplay) # printing html tables from kable
options(warn = -1) # for suppressing messages
########
########
loan.dt <- fread("../data/csv/loan_final313.csv", stringsAsFactors=TRUE)
str(loan.dt)loan <- copy(loan.dt)
sapply(loan, function(x) length(unique(x)))
a <- as.data.table(sapply(loan, function(x) length(unique(x))))
b <- cbind(names(loan),a) 
colnames(b)<- c("name","count")
loan[,b[count==1,name]:=NULL]
loan[,id:=NULL]
loan[,c("home_ownership_cat","income_cat","term_cat","purpose_cat","interest_payment_cat",
        "loan_condition_cat","grade_cat"):=NULL]
loan[,c("issue_d","final_d"):=NULL]
loan_factors <- loan %>% purrr::keep(is.factor) %>% # select factor columns
  tidyr::gather() %>% # convert into long format for faceting
  ggplot(aes(x = value)) + # plot value
  facet_wrap(~ key, scales = "free") + # divide into separate plots by key
  geom_bar()

plotly::ggplotly(loan_factors)
loan %>% purrr::keep(is.numeric) %>% # select columns
  tidyr::gather() %>% # reshape into long format in columns "key" and "value"
  ggplot(aes(value)) + # plot value
  facet_wrap(~ key, scale = "free" ) + # divide into separate plots by key
  geom_density(fill = "green")  # get density plots
set.seed(2018)
loan1 <- loan[,-c("loan_condition")]
train <- loan1[,sample(.I, .N*3/4)]
labels <- factor(loan$loan_condition, labels = c("bad", "good"))
fit1 <- e1071::naiveBayes(formula = labels[train] ~ ., data = loan1[train])
fit1
pred_probs <- predict(fit1, loan1[-train], type = "raw")
pred_percent <- pred_probs %>% apply(2, scales::percent, accuracy = 0.01)
pred_percent
labs <- colnames(pred_percent)[max.col(pred_probs)]

pred_percent %>%
  magrittr::set_rownames(labs)
pred <- predict(fit1, loan1[-train], type = "class")
pred
identical(labs, as.character(pred))
result <- caret::confusionMatrix(pred, labels[-train])
result
str(result)
result$overall[1]
loan1 %>% purrr::keep(is.numeric) %>% 
  cor() %>%
  corrplot::corrplot.mixed(upper = "ellipse",
                           lower = "number",
                           tl.pos = "lt",
                           number.cex = .5,
                           lower.col = "black",
                           tl.cex = 0.7)
set.seed(2018)
loan2 <- loan[,-c("loan_condition","loan_amount")]
train2 <- loan2[,sample(.I, .N*3/4)]
labels <- factor(loan$loan_condition, labels = c("bad", "good"))
fit2 <- e1071::naiveBayes(formula = labels[train2] ~ ., data = loan2[train2])
pred_probs2 <- predict(fit2, loan2[-train2], type = "raw")
pred_percent2 <- pred_probs2 %>% apply(2, scales::percent, accuracy = 0.01)
labs2 <- colnames(pred_percent2)[max.col(pred_probs2)]
pred_percent2 %>%
  magrittr::set_rownames(labs2)
pred2 <- predict(fit2, loan2[-train2], type = "class")
identical(labs2, as.character(pred2))
result2 <- caret::confusionMatrix(pred2, labels[-train2])
result2
