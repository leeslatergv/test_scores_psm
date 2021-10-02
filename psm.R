# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
 pacman::p_load(tidyverse, ggplot2, MatchIt)

data <- read.csv("test_scores.csv")
head(data)

data %>%
    group_by(teaching_method) %>%
    summarise(n_students = n(),
            mean_posttest = mean(posttest),
            std_error = sd(posttest) / sqrt(n_students))

data %>%
    mutate(test = (posttest - mean(posttest)) / sd(posttest)) %>%
    group_by(teaching_method) %>%
    summarise(mean_test = mean(test))

data <- data %>%
    mutate(posttest_standard = (posttest - mean(posttest)) / sd(posttest))

with(data, t.test(posttest ~ teaching_method))
# Experimental teaching group got higher average posttest score

colnames(data)

data_cov <- c("school", "school_seting", "school_type",
            "classroom", "n_student", "gender",
            "lunch", "pretest")
data %>%
    group_by(teaching_method) %>%
    select(one_of(data_cov)) %>%
    
