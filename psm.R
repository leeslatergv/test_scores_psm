# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
 pacman::p_load(tidyverse, ggplot2, MatchIt, gridExtra)

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

# Need to make dummy variables for categorical variables
table(data$lunch)
data <- data %>%
    mutate(rural = ifelse(school_setting == "Rural", 1, 0),
            suburban = ifelse(school_setting == "Suburban", 1, 0),
            urban = ifelse(school_setting == "Urban", 1, 0),
            public_school = ifelse(school_type == "Public", 1, 0),
            female = ifelse(gender == "Female", 1, 0),
            fsm = ifelse(lunch == "Qualifies for reduced/free lunch", 1, 0),
            experimental_teaching_method = 
                    ifelse(teaching_method == "Experimental", 1, 0))


data_cov <- c("rural", "suburban", "urban",
            "n_student", "public_school", "female",
            "fsm", "pretest")
data %>%
    group_by(teaching_method) %>%
    select(one_of(data_cov)) %>%
    summarise_all(funs(mean(., na.rm = T)))

lapply(data_cov, function(v){
    t.test(data[, v] ~ data[, "teaching_method"])
})
# No sig diff with gender and FSM % between students who were
# taught with Experimental vs Standard teaching methods

m_ps <- glm(experimental_teaching_method ~
            suburban + urban + n_student + public_school + pretest +
            female + fsm,
                family = binomial(),
                data = data)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                    experimental_teaching_method = m_ps$model$experimental_teaching_method)

head(prs_df)

labs <- paste("Teaching method used:", c("Experimental", "Standard"))
prs_df %>%
    mutate(experimental = ifelse(experimental_teaching_method == 1, labs[1], labs[2])) %>%
    ggplot(aes(x = pr_score)) +
    geom_histogram(color = "white") +
    facet_wrap(~experimental) +
    xlab("Probability of being a student under experimental teaching methods") +
    theme_bw()

data_nomiss <- data %>%
    select(posttest, experimental_teaching_method, one_of(data_cov)) %>%
    na.omit()

mod_match <- matchit(experimental_teaching_method ~ 
            suburban + urban + n_student + public_school + pretest +
            female + fsm,
            method = "nearest", data = data_nomiss)

summary(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

fn_bal <- function(dta, variable) {
    dta$variable <- dta[, variable]
    dta$experimental_teaching_method <- as.factor(dta$experimental_teaching_method)
    support <- c(min(dta$variable), max(dta$variable))
    ggplot(dta, aes(x = distance, y = variable, color = experimental_teaching_method)) +
        geom_point(alpha = 0.2, size = 1.3) +
        geom_smooth(method = "loess", se = F) +
        xlab("Propensity score") +
        ylab(variable) +
        theme_bw() +
        ylim(support)
}

grid.arrange(
    fn_bal(dta_m, "suburban"),
    fn_bal(dta_m, "urban") + theme(legend.position = "none"),
    fn_bal(dta_m, "public_school"),
    fn_bal(dta_m, "pretest") + theme(legend.position = "none"),
    fn_bal(dta_m, "female") + theme(legend.position = "none"),
    fn_bal(dta_m, "fsm") + theme(legend.position = "none"),
    nrow = 3, widths = c(1, 0.8)
)

dta_m %>%
  group_by(experimental_teaching_method) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean))

lapply(data_cov, function(v){
    t.test(dta_m[, v] ~ dta_m$experimental_teaching_method)
})
# We have not been able to match successfully
# on rural (not that we tried), and urban.
# Others are reasonably well matched

with(dta_m, t.test(posttest ~ experimental_teaching_method))

lm_treat1 <- lm(posttest ~ experimental_teaching_method, data = dta_m)
summary(lm_treat1)
lm_treat2 <- lm(posttest ~ experimental_teaching_method + 
            suburban + urban + n_student + public_school + pretest +
            female + fsm, data = dta_m)
summary(lm_treat2)
