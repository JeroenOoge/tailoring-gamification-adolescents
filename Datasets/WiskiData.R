library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

# import datasets
data.raw = read.csv("Datasets/userDataRaw.csv")
webform.sub = read.csv("Datasets/webformSubmissionsRaw.csv")
webform.data = read.csv("Datasets/webformDataRaw.csv")

# filter data based on year and age
data = data.raw %>%
  filter(Year == "3de middelbaar" | Year == "4de middelbaar") %>%
  filter(Age > 12)

# return submitted values for the webform with the given id
subs = function(id) {
  webform.sub %>%
    filter(nid == id) %>%
    merge(webform.data, by = "sid") %>%
    select(c("sid", "uid", "no", "data")) %>%
    group_by(sid) %>%
    mutate(rn = paste0("item", no)) %>%
    select(-no) %>%
    spread(rn, data) %>%
    ungroup() %>%
    select(-sid) %>%
    mutate_all(function(x)
      as.numeric(as.character(x)))
}

# reverse data value
subs.reverse = function(data, cols, max) {
  data[cols] = max - data[cols]
  return(data)
}

# all BFI submissions, renamed and reverse-scored
subs.bfi = subs(15) %>%
  rename(
    E1 = item1,
    A1 = item2,
    C1 = item3,
    N1 = item4,
    O1 = item5,
    E2 = item6,
    A2 = item7,
    C2 = item8,
    N2 = item9,
    O2 = item10,
    E3 = item11,
    A3 = item12,
    C3 = item13,
    N3 = item14,
    O3 = item15,
    E4 = item16,
    A4 = item17,
    C4 = item18,
    N4 = item19,
    O4 = item20,
    E5 = item21,
    A5 = item22,
    ctrl = item23,
    C5 = item24,
    N5 = item25,
    O5 = item26,
    E6 = item27,
    A6 = item28,
    C6 = item29,
    N6 = item30,
    O6 = item31,
    E7 = item32,
    A7 = item33,
    C7 = item34,
    N7 = item35,
    O7 = item36,
    E8 = item37,
    A8 = item38,
    C8  = item39,
    N8 = item40,
    O8  = item41,
    O9 = item42,
    A9 = item43,
    C9 = item44,
    O10 = item45
  ) %>%
  subs.reverse(
    c(
      "A1",
      "E2",
      "C2",
      "N2",
      "A3",
      "C4",
      "E5",
      "C5",
      "N5",
      "A6",
      "E7",
      "N7",
      "O7",
      "A8",
      "O9",
      "C9"
    ),
    4
  )

# all Hexad submissions, renamed and reverse-scored
subs.hex = subs(16) %>%
  rename(
    D2 = item1,
    P2 = item2,
    Ac1 = item3,
    Ph1 = item4,
    F3 = item5,
    Ac3 = item6,
    D1 = item7,
    S4 = item8,
    D3 = item9,
    Ph2 = item10,
    P1 = item11,
    Ac2 = item12,
    ctrl = item13,
    F4 = item14,
    S2 = item15,
    F1 = item16,
    S3 = item17,
    F2 = item18,
    P3 = item19,
    Ac4 = item20,
    P4 = item21,
    Ph3 = item22,
    D4 = item23,
    S1 = item24,
    Ph4 = item25
  )

# sort columns by name
sortcolumns = function(data) {
  return(data %>%
           select(sort(names(.))))
}

# valid submissions
valid = function(x, value) {
  # correct answer for control question
  ctrlcorrect = x %>%
    filter(uid %in% data$Uid) %>%
    filter(ctrl == value) %>%
    select(-ctrl)
  # not all same values
  return(ctrlcorrect[apply(ctrlcorrect %>% select(-uid), 1, function(x)
    length(unique(x)) > 1), ])
}

# final data
subs.valid.bfi = subs.bfi %>%
  valid(1) %>%
  sortcolumns()

subs.valid.hex = subs.hex %>%
  valid(2) %>%
  sortcolumns()

data.ffm = data %>%
  filter(Uid %in% subs.valid.bfi$uid) %>%
  select(c(7:11))

data.hex = data %>%
  filter(Uid %in% subs.valid.hex$uid) %>%
  select(c(12:17))

# export data to csv
data.csv.export = function(data, filename) {
  write.csv(
    x = data,
    file = filename,
    row.names = FALSE,
    quote = FALSE,
    sep = ",",
    col.names = TRUE
  )
}

data.csv.export(subs.valid.bfi %>% select(-uid),
                "Datasets/submissionsBFI.csv")
data.csv.export(subs.valid.hex %>% select(-uid),
                "Datasets/submissionsHexad.csv")
data.csv.export(data.ffm, 'Datasets/scoresFFM.csv')
data.csv.export(data.hex, 'Datasets/scoresHexad.csv')
