library(lsr)
library(coin)
library(ggpubr)
library(data.table)
library(multcomp)
library(multcompView)
library(xtable)
library(userfriendlyscience)
library(mvnormtest)
library(car)
library(FSA)
library(rstatix)

## normal one-sample test
set.seed(1234)
x <- rnorm(10, 10, 1) %>% round(1)
paste0(x, collapse = ", ")
mean(x)
sd(x)
shapiro.test(x)
t.test(x)
t.test(x, mu = 9)
cohensD(x, mu = 9)


## non-normal one-sample test
set.seed(1234)
x <- rexp(10, 1) %>% round(2)
paste0(x, collapse = ", ")
mean(x)
sd(x)
shapiro.test(x)
wilcox.test(x, mu = 2)


## normal paired test
set.seed(1234)
x1 <- rnorm(8, 10, 1) %>% round(1)
x2 <- round(x1 + rnorm(8, 1), 1)
shapiro.test(x1 - x2)
t.test(x1, x2, paired = T, mu = 0.1)
paste0(x1, collapse = " & ")
paste0(x2, collapse = " & ")
mean(x1)
sd(x1)
mean(x2)
sd(x2)
mean(x1 - x2)
sd(x1 - x2)
cohensD(x1 - x2, mu = 0.5)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = gl(2, 8, labels = c("x1", "x2")),
    block = gl(8, 1, 16)
  )
f1 <-
  ggplot(d.plot, aes(group, observation)) +
  geom_boxplot(width = 0.2,
               position = position_nudge(x = c(-0.2, 0.2)),
               outlier.shape = NA) +
  geom_jitter(width = 0) +
  geom_segment(
    aes(
      x = 1,
      xend = 2,
      y = `x1`,
      yend = `x2`
    ),
    dcast(d.plot, block ~ group, value.var = "observation")
  ) +
  theme_pubr(10, border = T)
f2 <-
  dcast(d.plot, block ~ group, value.var = "observation") %>%
  .[, .(difference = x1 - x2), by = block] %>%
  ggplot(aes(x = 0, y = difference)) +
  geom_boxplot(width = 0.1) +
  geom_jitter(width = 0.05) +
  theme_pubr(10, border = T) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  ylab("difference\n(x1 - x2)") +
  xlim(c(-0.15, 0.15))
windows(4, 2.5)
ggarrange(
  f1,
  f2,
  nrow = 1,
  labels = "auto",
  align = "h",
  widths = c(2, 1.2)
)
ggsave("normal_paired_test.pdf")


## non-normal paired test
set.seed(1212314)
x1 <- runif(8, 5, 8) %>% round(1)
x2 <- round(x1 + runif(8,-1, 1), 1)
shapiro.test(x1 - x2)
wilcox.test(x1,
            x2,
            paired = T,
            mu = 1,
            exact = F)
t.test(x1, x2, paired = T, mu = 0.1)
paste0(x1, collapse = " & ")
paste0(x2, collapse = " & ")
mean(x1)
sd(x1)
mean(x2)
sd(x2)
mean(x1 - x2)
sd(x1 - x2)
cohensD(x1 - x2, mu = 0.5)
table(x1 - x2 < 1)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = gl(2, 8, labels = c("x1", "x2")),
    block = gl(8, 1, 16)
  )
f1 <-
  ggplot(d.plot, aes(group, observation)) +
  geom_boxplot(width = 0.2,
               position = position_nudge(x = c(-0.2, 0.2)),
               outlier.shape = NA) +
  geom_jitter(width = 0) +
  geom_segment(
    aes(
      x = 1,
      xend = 2,
      y = `x1`,
      yend = `x2`
    ),
    dcast(d.plot, block ~ group, value.var = "observation")
  ) +
  theme_pubr(10, border = T)
f2 <-
  dcast(d.plot, block ~ group, value.var = "observation") %>%
  .[, .(difference = x1 - x2), by = block] %>%
  ggplot(aes(x = 0, y = difference)) +
  geom_boxplot(width = 0.1) +
  geom_jitter(width = 0.05) +
  theme_pubr(10, border = T) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  ylab("difference\n(x1 - x2)") +
  xlim(c(-0.15, 0.15))
windows(4, 2.5)
ggarrange(
  f1,
  f2,
  nrow = 1,
  labels = "auto",
  align = "h",
  widths = c(2, 1.2)
)
ggsave("non-normal_paired_test.pdf")


## normal independent two-sample test
set.seed(124)
x1 <- rnorm(6, 10, 1) %>% round(1)
x2 <- rnorm(7, 9, 1) %>% round(1)
shapiro.test(x1)
shapiro.test(x2)
t.test(x1, x2)
paste0(x1, collapse = " & ")
paste0(x2, collapse = " & ")
mean(x1)
sd(x1)
mean(x2)
sd(x2)
cohensD(x1, x2)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = c(rep("x1", 6), rep("x2", 7))
  )
windows(4, 2.5)
ggplot(d.plot, aes(group, observation)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  theme_pubr(10, border = T)
ggsave("normal_independent_test.pdf")


## non-normal independent two-sample test
set.seed(6324)
x1 <- runif(8, 0, 1) %>% round(1)
x2 <- runif(7, 0, 2) %>% round(1)
shapiro.test(x1)
shapiro.test(x2)
paste0(x1, collapse = " & ")
paste0(x2, collapse = " & ")
mean(x1)
sd(x1)
mean(x2)
sd(x2)
windows(4, 2.5)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = c(rep("x1", 8), rep("x2", 7))
  )
windows(4, 2.5)
ggplot(d.plot, aes(group, observation)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  theme_pubr(10, border = T)
ggsave("non-normal_independent_test.pdf")


## oneway ANOVA
set.seed(364)
d <-
  data.table(y = round(c(rep(5, 6), rep(6, 5), rep(7, 7)) + rnorm(18), 2),
             group = factor(c(rep("x1", 6), rep("x2", 5), rep("x3", 7))))
tapply(d$y, d$group, shapiro.test)
bartlett.test(y ~ group, d)
d[, paste0(y, collapse = " & "), by = group]
d[, .(Mean = mean(y),
      SD = sd(y),
      n = length(y)), by = group] %>%
  as.data.frame %>%
  xtable(
    .,
    digits = 3,
    auto = T,
    label = "table:oneway_ANOVA",
    caption = "獨立三樣本的描述性統計。"
  )
fit <- aov(y ~ group, d)
summary(fit)
TukeyHSD(fit, "group")$group
TukeyHSD(fit, "group")$group %>%
  xtable(
    digits = 3,
    auto = T,
    label = "table:oneway_ANOVA_post",
    caption = "獨立三樣本的事後多重比較。"
  )
fit.mult <-
  TukeyHSD(fit, "group")$group[, "p adj"] %>%
  multcompLetters %>%
  .$Letters %>%
  data.table(group = names(.), rank = .) %>%
  merge(., d[, .(max.val = max(y)), by = group], by = "group")
windows(4, 2.5)
ggplot(d, aes(group, y)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  geom_text(aes(group, max.val + 0.5, label = rank),
            fit.mult,
            size = 10 * 0.352777778) +
  theme_pubr(10, border = T)
ggsave("oneway_ANOVA.pdf")


## Welch’s ANOVA
set.seed(12234)
d <-
  data.table(y = round(rnorm(
    18,
    mean = c(rep(4, 6), rep(6, 5), rep(7, 7)),
    sd = c(rep(1, 6), rep(2, 5), rep(3, 7))
  ), 2),
  group = factor(c(rep("x1", 6), rep("x2", 5), rep("x3", 7))))
tapply(d$y, d$group, shapiro.test)
bartlett.test(y ~ group, d)
d[, paste0(y, collapse = " & "), by = group]
d[, .(Mean = mean(y),
      SD = sd(y),
      n = length(y)), by = group] %>%
  as.data.frame %>%
  xtable(
    .,
    digits = 3,
    auto = T,
    label = "table:Welch_ANOVA",
    caption = "獨立三樣本的描述性統計。"
  )
aov(y ~ group, data = d) %>% summary
oneway.test(y ~ group, data = d)
mc <- posthocTGH(d$y, d$group, digits = 3)$output$games.howell
mc %>%
  xtable(
    digits = 3,
    auto = T,
    label = "table:Welch_ANOVA_post",
    caption = "獨立三樣本的事後多重比較。"
  )
fit.mult <-
  mc$p %>%
  set_names(rownames(mc)) %>%
  multcompLetters %>%
  .$Letters %>%
  data.table(group = names(.), rank = .) %>%
  merge(., d[, .(max.val = max(y)), by = group], by = "group")
windows(4, 2.5)
ggplot(d, aes(group, y)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  geom_text(aes(group, max.val + 1, label = rank),
            fit.mult,
            size = 10 * 0.352777778) +
  theme_pubr(10, border = T)
ggsave("Welch_ANOVA.pdf")


## Kruskal-Wallis Rank Sum Test
set.seed(1132234)
d <-
  data.table(y = c(rexp(8, 0.2), rexp(7, 0.5), rexp(6, 1)) %>% round(1),
             group = c(rep("x1", 8), rep("x2", 7), rep("x3", 6)))
d[, shapiro.test(y), by = group]
d[, paste0(y, collapse = " & "), by = group]
d[, .(Mean = mean(y),
      SD = sd(y),
      n = length(y)), by = group] %>%
  as.data.frame %>%
  xtable(
    .,
    digits = 3,
    auto = T,
    label = "table:rank_oneway",
    caption = "獨立三樣本的描述性統計。"
  )
kruskal.test(y ~ group, d)
kruskal_effsize(d, y ~ group)
dunnTest(y ~ group, d)
dunnTest(y ~ group, d)$res %>%
  xtable(caption = "Dunn's Kruskal-Wallis多重比較之結果。",
         label = "table:rank_oneway_post",
         digits = 3)
fit.mult <-
  dunnTest(y ~ group, d)$res[, "P.adj"] %>%
  set_names(dunnTest(y ~ group, d)$res[, "Comparison"] %>% gsub(" ", "", .)) %>%
  multcompLetters %>%
  .$Letters %>%
  data.table(group = names(.), rank = .) %>%
  merge(., d[, .(max.val = max(y)), by = group], by = "group")
windows(4, 2.5)
ggplot(d, aes(group, y)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  geom_text(aes(group, max.val + 1, label = rank),
            fit.mult,
            size = 10 * 0.352777778) +
  theme_pubr(10, border = T)
ggsave("rank_oneway.pdf")


## simple linear regression
set.seed(1234)
x <- rnorm(8, 10) %>% round(1)
y <- (x * 2 + rnorm(8)) %>% round(1)
d <- data.table(x, y)
fit <- lm(y ~ x, d)
summary(fit)
confint(fit)
shapiro.test(fit$residuals)
d[, paste0(x, collapse = " & ")]
d[, paste0(y, collapse = " & ")]
fit %>% {
  cbind(
    (.) %>% summary %>% .$coefficients ,
    confint(.)
  )
} %>%
  xtable(caption = "簡單線性迴歸之結果。",
         label = "table:simple_regression",
         digits = 3)
f1 <-
  ggplot(d, aes(x, y)) +
  geom_smooth(method = "lm", color = 1, fill = "#aaaaaa") +
  geom_point() +
  annotate(
    "text",
    label = "y = -1.016 + 2.069x,\nR^2 = 0.921",
    x = 9,
    y = 22,
    size = 10 * 0.352777778
  ) +
  theme_pubr(10, border = T)
f2 <-
  ggplot(d, aes(sample = fit$residuals)) +
  stat_qq() + 
  stat_qq_line() +
  theme_pubr(10, border = T)
windows(5, 2.5)
ggarrange(
  f1,
  f2,
  nrow = 1,
  labels = "auto",
  align = "hv",
  widths = c(1, 0.8)
)
ggsave("simple_regression.pdf")


## simple linear cor
set.seed(1234)
x1 <- rnorm(8, 10) %>% round(1)
x2 <- (x * 2 + rnorm(8)) %>% round(1)
d <- data.table(x1, x2)
d[, paste0(x1, collapse = " & ")]
d[, paste0(x2, collapse = " & ")]
mshapiro.test(d %>% as.matrix %>% t)
cor.test(d$x1, d$x2)
fit <- lm(x2 ~ x1, d)
windows(4, 2.5)
ggplot(d, aes(x1, x2)) +
  geom_path(data =
              dataEllipse(
                x1,
                x2,
                draw = F,
                levels = 0.95,
                segments = 500
              ) %>%
              as.data.table,
            aes(x, y)) +
  geom_point() +
  annotate(
    "text",
    label = "r = 960, p < 0.001",
    x = 8,
    y = 25,
    size = 10 * 0.352777778
  ) +
  theme_pubr(10, border = T)
ggsave("simple_cor.pdf")


## spearman correlation
set.seed(125)
x1 <- runif(8, 5, 8) %>% round(1)
x2 <- rexp(8) %>% round(1)
d <- data.table(x1, x2)
mshapiro.test(d %>% as.matrix %>% t)
d[, paste0(x1, collapse = " & ")]
d[, paste0(x2, collapse = " & ")]
cor.test(d$x1, d$x2, method = "spearman")
windows(4, 2.5)
ggplot(d, aes(x1, x2)) +
  geom_point() +
  annotate(
    "text",
    label = "r = -0.241, p = 0.565",
    x = 7,
    y = 4,
    size = 10 * 0.352777778
  ) +
  theme_pubr(10, border = T)
ggsave("spearman_cor.pdf")
