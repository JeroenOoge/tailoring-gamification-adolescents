library(xtable)
library(reshape2)
library(MVN)
library(ggplot2)
library(dplyr)
library(psych)

data.ffm = read.csv("Datasets/scoresFFM.csv")
data.hex = read.csv("Datasets/scoresHexad.csv")
subs.valid.bfi = read.csv("Datasets/submissionsBFI.csv")
subs.valid.hex = read.csv("Datasets/submissionsHexad.csv")

# boxplots for BFI and Hexad
make.boxplot = function(data, l, b) {
  data.melted = melt(data)
  plot = ggplot(data.melted, aes(x = variable, y = value)) +
    geom_boxplot() +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL,
                       limits = l,
                       breaks = b) +
    theme_minimal() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_blank()
    )
  return(plot)
}
make.boxplot(data.ffm, c(0, 5), c(0:5))
make.boxplot(data.hex, c(4, 28), seq(4, 28, 2))

# violin plots for BFI and Hexad
make.violinplot = function(data, l, b) {
  data.melted = melt(data)
  plot = ggplot(data.melted, aes(x = variable, y = value)) +
    geom_violin(aes(fill = variable), color = "#cccccc") +
    scale_fill_manual(values = rep("#cccccc", length(unique(data.melted$variable)))) +
    geom_boxplot(width = 0.35) +
    coord_flip() +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(name = NULL,
                       limits = l,
                       breaks = b) +
    theme_minimal() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_blank(),
      legend.position = "none"
    )
  return(plot)
}
make.violinplot(data.ffm, c(0, 5), c(0:5))
make.violinplot(data.hex, c(4, 28), seq(4, 28, 2))

# bar plots for BFI and Hexad
make.barplot = function(data, n, b) {
  ggplot(melt(data), aes(value)) +
    geom_bar() +
    facet_wrap(~variable, ncol = n) +
    scale_x_continuous(breaks = b) +
    theme_minimal() +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
make.barplot(subs.valid.bfi, 4, c(0:4))
make.barplot(subs.valid.hex, 4, c(1:7))

# QQ plots for BFI and Hexad
make.qqplot = function(d, n) {
  ggplot(melt(d), aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~variable, ncol = n) +
    theme_minimal() +
    scale_x_continuous(name="Theoretical normal quantiles") +
    scale_y_continuous(name="Observed quantiles") +
    theme(
      axis.title.y = element_text(margin = margin(r=20)),
      axis.title.x = element_text(margin = margin(t=10))
    )
}
make.qqplot(subs.valid.bfi, 4)
make.qqplot(subs.valid.hex, 4)

# Multinormality test
mvn.des = function(d) {
  mvn = mvn(data=d,mvnTest="mardia")
  des = mvn$Descriptives[c("Mean","Skew", "Kurtosis")]
  des$Variable = rownames(des)
  return(des)
}
mvn.des(subs.valid.bfi)
mvn.des(subs.valid.hex)

# Absolute skewness and kurtosis of BFI and Hexad
mvn.des(subs.valid.bfi) %>%
  filter(abs(Skew) > 1)
mvn.des(subs.valid.hex) %>%
  filter(abs(Skew) > 1)
mvn.des(subs.valid.bfi) %>%
  filter(abs(Kurtosis) > 1)
mvn.des(subs.valid.hex) %>%
  filter(abs(Kurtosis) > 1)
mean(abs(mvn.des(subs.valid.bfi)$Skew))
mean(abs(mvn.des(subs.valid.hex)$Skew))

# Cronbach's alpha
cronbach.execute = function(data, type, nbs) {
  value = alpha(data %>% select(paste(type, nbs, sep = "")))$total$raw_alpha
  rate = if (value > 0.7)
    "passed"
  else
    "failed"
  print(rate)
  return(value)
}
cronbach.bfihex = c(
  cronbach.execute(subs.valid.bfi, "O", (1:10)),
  cronbach.execute(subs.valid.bfi, "C", (1:9)),
  cronbach.execute(subs.valid.bfi, "E", (1:8)),
  cronbach.execute(subs.valid.bfi, "A", (1:9)),
  cronbach.execute(subs.valid.bfi, "N", (1:8)),
  cronbach.execute(subs.valid.hex, "Ac", (1:4)),
  cronbach.execute(subs.valid.hex, "D", (1:4)),
  cronbach.execute(subs.valid.hex, "F", (1:4)),
  cronbach.execute(subs.valid.hex, "Ph", (1:4)),
  cronbach.execute(subs.valid.hex, "P", (1:4)),
  cronbach.execute(subs.valid.hex, "S", (1:4))
)

# Summary of all basic statistics
summary = function(x) {
  mean = sapply(x, mean)
  variance = sapply(x, var)
  skewness = mvn.des(x)$Skew
  kurtosis = mvn.des(x)$Kurtosis
  rbind(mean, variance, skewness, kurtosis)
}

basicstats = data.frame(cbind(summary(data.ffm), summary(data.hex))) %>%
  rename(
    O = Openness,
    C = Conscientiousness,
    E = Extraversion,
    A = Agreeableness,
    N = Neuroticism,
    Ph = Philanthropist,
    D = Disruptor,
    S = Socialiser,
    P = Player,
    F = Free.Spirit,
    Ac = Achiever
  )
basicstats = rbind(basicstats, cronbach = cronbach.bfihex)

# Save the basic statistics in a tex file
bold = function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(
  xtable(basicstats,
         label = "tab:basicstats",
         caption = "Basic statistics for the BFI-44 and Hexad types."),
  file = "Tables/basicStatsBFIHexad.tex",
  sanitize.colnames.function=bold,
  booktabs = TRUE
)
