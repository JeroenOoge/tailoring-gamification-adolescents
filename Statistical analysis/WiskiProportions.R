library(dplyr)
library(reshape2)
library(ggpubr)
library(HH)

subs.valid.bfi = read.csv("Datasets/submissionsBFI.csv")
subs.valid.hex = read.csv("Datasets/submissionsHexad.csv")

# Proportion of a specific value in a column
props.compute = function(d, int) {
  return(d %>%
           apply(
             MARGIN = 2,
             FUN = function(col) {
               return(length(col[col == int]) / length(col) * 100)
             }
           ))
}

# Proportions of all values between start and end in a column
props.compute.all = function(d, start, end) {
  props = data.frame(props.compute(d, start))
  for (i in (start + 1):end) {
    vector = props.compute(d, i)
    props = cbind(props, vector)
  }
  names(props) = paste0(rep("value", length(start:end)), start:end)
  return(props)
}

# Melt proportions
props.melt = function(props) {
  props %>%
    cbind(question = rownames(props)) %>%
    melt(variable.name = "answer") %>%
    rename(proportion = value) %>%
    mutate(type = substr(question, 1, nchar(as.character(question)) - 1))
}

# Proportions of BFI
props.bfi = props.compute.all(subs.valid.bfi, 0, 4) %>%
  rename(
    "Completely disagree" = value0,
    "Disagree" = value1,
    "Agree nor disagree" = value2,
    "Agree" = value3,
    "Completely agree" = value4
  )
props.bfi.melted = props.melt(props.bfi)
props.bfi.melted$type[props.bfi.melted$type == "O1"] = "O"

# Proportions of Hexad
props.hexad = props.compute.all(subs.valid.hex, 1, 7) %>%
  rename(
    "Completely disagree" = value1,
    "Disagree" = value2,
    "Rather disagree" = value3,
    "Neutral" = value4,
    "Rather agree" = value5,
    "Agree" = value6,
    "Completely agree" = value7
  )
props.hexad.melted = props.melt(props.hexad)

# Likert stacked bar charts for BFI and Hexad
make.stackedbar = function(d, m) {
  likert(d,
         main = m,
         xlim = c(-100:100),
         xlab = "Percent",
         grouping = d$type)
}
make.stackedbar(props.bfi, "Distribution of BFI item answers")
make.stackedbar(props.hexad, "Distribution of Hexad item answers")