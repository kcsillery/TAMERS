## #######################################################################

## TAMERS survey data analysis - Katalin Csillery 8 Sept 2026

## 3. create different datasets for different analysis

## ########################################################################

## extract question codes from the suvery file
## ##############################################
library(XML)
doc <- xmlParse("data/limesurvey_survey_116296_question_code.lss")

## answer codes: aid, qid, code, scale_id
## scale_id is kept because a handful of questions (e.g. G02Q03a) define
## two parallel answer scales under the same qid (scale_id 0 and 1), which
## correspond to the "_0"/"_1" suffix LimeSurvey appends to that question's
## data columns. Without scale_id, codes2 ends up with duplicate, indistin-
## guishable (title, code) rows for those questions.
ans <- getNodeSet(doc, "//answers/rows/row")

answers <- data.frame(
  aid      = trimws(sapply(ans, function(x) xmlValue(x[["aid"]]))),
  qid      = trimws(sapply(ans, function(x) xmlValue(x[["qid"]]))),
  code     = trimws(sapply(ans, function(x) xmlValue(x[["code"]]))),
  scale_id = trimws(sapply(ans, function(x) xmlValue(x[["scale_id"]]))),
  stringsAsFactors = FALSE
)

## answer labels: aid, answer, language
lab <- getNodeSet(doc, "//answer_l10ns/rows/row")

answer.labels <- data.frame(
  aid      = trimws(sapply(lab, function(x) xmlValue(x[["aid"]]))),
  answer   = trimws(sapply(lab, function(x) xmlValue(x[["answer"]]))),
  language = trimws(sapply(lab, function(x) xmlValue(x[["language"]]))),
  stringsAsFactors = FALSE
)

## questions: qid, title
qs <- getNodeSet(doc, "//questions/rows/row")

questions <- data.frame(
  qid   = trimws(sapply(qs, function(x) xmlValue(x[["qid"]]))),
  title = trimws(sapply(qs, function(x) xmlValue(x[["title"]]))),
  stringsAsFactors = FALSE
)

## merge codes + English labels + question titles
answer.labels.en <- answer.labels[answer.labels$language == "en", ]

codes2 <- merge(answers, answer.labels.en, by = "aid", all.x = TRUE)
codes2 <- merge(codes2, questions, by = "qid", all = TRUE)

codes2 <- codes2[, c("title", "scale_id", "code", "answer", "qid", "aid")]

## Not every question ends up with codes here -- LimeSurvey only stores
## custom answer options (e.g. country, consent, degree) in the answers
## table. Built-in scale types (5-point/10-point arrays, Yes/No/Uncertain
## arrays, checkboxes, free-text arrays) use a fixed, hardcoded code set
## that is never written to the .lss file, so those questions necessarily
## come through with code/answer = NA. This is expected, not a bug -- the
## scale meaning for those questions has to come from the column header
## text in the raw survey export instead. Printed here so it's obvious
## which questions these are, rather than discovering stray NAs later.
no.codes <- unique(codes2$title[is.na(codes2$code)])
cat(length(no.codes), "of", length(unique(codes2$title)),
    "questions have no answer codes in the .lss (built-in scale type):\n")
print(no.codes)

## => 18 questions with no answer codes

save(codes2, file="data/codes2.RData")



