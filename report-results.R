library(lsr)
library(coin)
library(ggpubr)
library(data.table)
library(multcomp)
library(multcompView)
library(xtable)


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
windows(4, 2)
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
windows(4, 2)
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
windows(3, 2)
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
windows(3, 2)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = c(rep("x1", 8), rep("x2", 7))
  )
windows(3, 2)
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
windows(3, 2)
ggplot(d, aes(group, y)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  geom_text(aes(group, max.val + 0.5, label = rank),
            fit.mult,
            size = 10 * 0.352777778) +
  theme_pubr(10, border = T)
ggsave("oneway_ANOVA.pdf")
