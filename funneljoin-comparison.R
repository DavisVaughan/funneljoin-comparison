library(tidyverse)
library(funneljoin)

# # Used to setup the rds files from the original CSVs
#
# answers <- read_csv("Answers.csv")
# questions <- read_csv("Questions.csv")
#
# questions <- questions %>%
#   janitor::clean_names() %>%
#   filter(!is.na(owner_user_id))
#
# answers <- answers %>%
#   janitor::clean_names() %>%
#   filter(!is.na(owner_user_id))
#
# questions <- questions %>%
#   select(owner_user_id, creation_date)
#
# answers <- answers %>%
#   select(owner_user_id, creation_date)
#
# saveRDS(answers, here::here("answers.rds"))
# saveRDS(questions, here::here("questions.rds"))

answers <- readRDS(here::here("answers.rds"))
questions <- readRDS(here::here("questions.rds"))


check_equal <- function(x, y) {
  # Take off attribute for equality check
  attr(x, "after_join") <- NULL

  # I think `after_join()` might adjust the sort ordering somehow, not sure.
  # Regardless, same rows!
  x <- arrange(x, pick(everything()))
  y <- arrange(y, pick(everything()))

  identical(x, y)
}

# ---

first_answer_after_first_question <- questions %>%
  after_left_join(answers,
                  by_time = "creation_date",
                  by_user = "owner_user_id",
                  type = "first-firstafter",
                  suffix = c("_question", "_answer"))

first_answer_after_first_question2 <- questions %>%
  arrange(creation_date) %>%
  distinct(owner_user_id, .keep_all = TRUE) %>%
  left_join(
    answers,
    join_by(owner_user_id, closest(creation_date <= creation_date)),
    suffix = c("_question", "_answer")
  )

check_equal(
  first_answer_after_first_question,
  first_answer_after_first_question2
)

# ---

first_answer_after_first_question_with_limit <- questions %>%
  after_join(answers,
             by_time = "creation_date",
             by_user = "owner_user_id",
             type = "first-firstafter",
             mode = "left",
             suffix = c("_question", "_answer"),
             max_gap = as.difftime(1, units = "weeks"))

first_answer_after_first_question_with_limit2 <- questions %>%
  arrange(creation_date) %>%
  distinct(owner_user_id, .keep_all = TRUE) %>%
  mutate(creation_date_upper = creation_date + as.difftime(1, units = "weeks")) %>%
  left_join(
    answers,
    join_by(
      owner_user_id,
      closest(creation_date <= creation_date),
      creation_date_upper >= creation_date
    ),
    suffix = c("_question", "_answer")
  ) %>%
  select(!creation_date_upper)

check_equal(
  first_answer_after_first_question_with_limit,
  first_answer_after_first_question_with_limit2
)

# ---

answered_before_asking <- answers %>%
  after_right_join(questions,
                   by_time = "creation_date",
                   by_user = "owner_user_id",
                   type = "first-first",
                   suffix = c("_answer", "_question"))


first_answers <- answers %>%
  arrange(creation_date) %>%
  distinct(owner_user_id, .keep_all = TRUE)

first_questions <- questions %>%
  arrange(creation_date) %>%
  distinct(owner_user_id, .keep_all = TRUE)

answered_before_asking2 <- first_answers %>%
  right_join(
    first_questions,
    join_by(
      owner_user_id,
      creation_date <= creation_date
    ),
    suffix = c("_answer", "_question")
  )

check_equal(
  answered_before_asking,
  answered_before_asking2
)

# ---

all_answers_after_first_question <- questions %>%
  after_inner_join(answers,
                   by_time = "creation_date",
                   by_user = "owner_user_id",
                   type = "first-any")

all_answers_after_first_question2 <- questions %>%
  arrange(creation_date) %>%
  distinct(owner_user_id, .keep_all = TRUE) %>%
  inner_join(
    answers,
    join_by(
      owner_user_id,
      creation_date <= creation_date
    )
  )

check_equal(
  all_answers_after_first_question,
  all_answers_after_first_question2
)

