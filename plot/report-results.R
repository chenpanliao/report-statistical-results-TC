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
library(rcompanion)
library(effsize)
sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] effsize_0.7.6             rcompanion_2.3.7          rstatix_0.3.0            
#  [4] FSA_0.8.26                car_3.0-5                 carData_3.0-3            
#  [7] mvnormtest_0.1-9          userfriendlyscience_0.7.2 xtable_1.8-4             
# [10] multcompView_0.1-7        multcomp_1.4-10           TH.data_1.0-10           
# [13] MASS_7.3-51.4             mvtnorm_1.0-11            data.table_1.12.6        
# [16] ggpubr_0.2.4              magrittr_1.5              ggplot2_3.2.1            
# [19] coin_1.3-1                survival_3.1-7            lsr_0.5                  


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
windows(4, 2.0)
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
windows(4, 2.0)
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
windows(2, 2.0)
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
cliff.delta(x1, x2)
d.plot <-
  data.table(
    observation = c(x1, x2),
    group = c(rep("x1", 8), rep("x2", 7))
  )
windows(2, 2.0)
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
cohens_d(d, y ~ group, var.equal = T) %>%
  as.data.table %>%
  .[, comparison := paste0(group2, "-", group1)] %>%
  .[, .(comparison, effsize)] %>%
  merge(.,
        TukeyHSD(fit, "group")$group %>% 
          as.data.table(keep.rownames = "comparison"),
        by = "comparison") %T>%
  print %>%
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
windows(2.5, 2.0)
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
  as.data.table %>%
  xtable(
    .,
    digits = 3,
    auto = T,
    label = "table:Welch_ANOVA",
    caption = "獨立三樣本的描述性統計。"
  )
aov(y ~ group, data = d) %>% summary
oneway.test(y ~ group, data = d)
cohens_d(d, y ~ group, var.equal = F) %>%
  as.data.table %>%
  .[, comparison := paste0(group2, "-", group1)] %>%
  .[, .(comparison, effsize)] %>%
  merge(
    .,
    posthocTGH(d$y, d$group, digits = 3)$output$games.howell %>% 
      as.data.table(keep.rownames = "comparison")
  ) %T>%
  print %>%
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
windows(2.5, 2.0)
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
combn(d$group %>% unique, 2) %>%
  apply(., 2, function(x){
    cliff.delta(y ~ group, d[group == x[1] | group == x[2]])$estimate
  }) %>%
  data.table(`Cliff's d` = .) %>%
  .[, Comparison := combn(d$group %>% unique, 2, paste0, collapse = " - ")] %>% 
  merge(., dunnTest(y ~ group, d)$res %>% as.data.table, by = "Comparison") %T>% 
  print %>%
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
windows(2.5, 2.0)
ggplot(d, aes(group, y)) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA) +
  geom_jitter(width = 0.3) +
  geom_text(aes(group, max.val + 1, label = rank),
            fit.mult,
            size = 10 * 0.352777778) +
  theme_pubr(10, border = T)
ggsave("rank_oneway.pdf")


## twoway ANOVA factorial design
set.seed(1224)
d <- data.table(
  A = c(rep("A1", 13), rep("A2", 13)),
  B = c(rep("B1", 4), rep("B2", 4), rep("B3", 5)) %>% rep(2),
  Y = rnorm(26, mean = c(
    rep(5, 4), rep(5, 4), rep(6, 5), rep(6, 4), rep(6, 4), rep(10, 5)
  )) %>% round(1)
)
d[, shapiro.test(Y), by = .(A, B)]
bartlett.test(Y ~ interaction(A, B), data = d)
fit.full <-
  lm(Y ~ A * B,
     data = d,
     contrasts = list(A = contr.sum, B = contr.sum))
summary(fit.full)
Anova(fit.full, type = 3) %>% 
  xtable(caption = "Twoway ANOVA之變方分析表。", label = "table:twowayANOVA")
drop1(fit.full, test = "F")
d[, group := paste0(A, B)]
fit <- aov(Y ~ group, data = d)
summary(fit)
TukeyHSD(fit, "group")$group
TukeyHSD(fit, "group")$group %>%
  xtable(
    digits = 3,
    auto = T,
    label = "table:twoway_ANOVA_post",
    caption = "$3\tiems2$因子實驗之簡單主效應事後多重比較。"
  )
fit.mult <-
  TukeyHSD(fit, "group")$group[, "p adj"] %>%
  multcompLetters %>%
  .$Letters %>%
  data.table(group = names(.), rank = .) %>%
  merge(., d[, .(max.val = max(Y)), by = group], by = "group")
d.summary <-
  d[, .(
    average = mean(Y),
    SD = sd(Y),
    CI.lower = t.test(Y)$conf.int[1],
    CI.upper = t.test(Y)$conf.int[2],
    n = .N
  ), by = .(A, B, group)] %>%
  merge(., fit.mult, by = "group") %T>%
  print
ggplot(fortify(fit.full), aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_pubr(10, border = T)

pd <- position_dodge(0.7)
windows(4, 3)
ggplot(d.summary, aes(A, average)) +
  geom_errorbar(
    aes(ymin = CI.lower, ymax = CI.upper, color = B),
    width = 0.3,
    size = 1,
    position = pd
  ) +
  geom_point(aes(shape = B, color = B), position = pd, size = 3) +
  geom_text(
    aes(
      y = CI.upper + 0.5,
      label = rank,
      group = B
    ),
    position = pd,
    size = 10 * 0.352777778
  ) +
  geom_text(aes(y = 2.5, label = n, group = B),
            position = pd,
            size = 10 * 0.352777778) +
  geom_jitter(
    aes(A, Y, shape = B),
    data = d,
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
    size = 1.2,
    color = 8
  ) +
  theme_pubr(10, border = T, legend = "top") +
  ylab("observation, average and 95% CI") +
  theme(legend.text = element_text(size = 10))
ggsave("twoway_ANOVA.pdf")


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
  geom_smooth(method = "lm") +
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
  fortify(fit) %>%
  .[order(.$.fitted)] %>%
  ggplot(., aes(.fitted, scale(.resid))) +
  geom_point() +
  geom_smooth(se = F) +
  xlab("fitted") +
  ylab("standardized\nresidual") +
  theme_pubr(10, border = T)
f3 <-
  ggplot(d, aes(sample = fit$residuals)) +
  stat_qq() + 
  stat_qq_line() +
  theme_pubr(10, border = T)
windows(3.5, 5.5)
ggarrange(
  f1,
  f2,
  f3,
  nrow = 3,
  labels = "auto",
  align = "hv",
  widths = c(1, 1)
)
ggsave("simple_regression.pdf")


## simple linear cor
set.seed(1234)
x1 <- rnorm(8, 10) %>% round(1)
x2 <- (x1 * 2 + rnorm(8)) %>% round(1)
d <- data.table(x1, x2)
d[, paste0(x1, collapse = " & ")]
d[, paste0(x2, collapse = " & ")]
mshapiro.test(d %>% as.matrix %>% t)
cor.test(d$x1, d$x2)
fit <- lm(x2 ~ x1, d)
windows(3, 2.0)
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
windows(3, 2.0)
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


## chi-squared goodness of fit
obs.val <- c(20, 15, 3, 2)
exp.p <- c(4, 3, 2, 1) %>% divide_by(sum(.))
d <-
  data.table(observation = obs.val,
             expectation = exp.p * sum(obs.val),
             blood = c("O", "A", "B", "AB")) %>%
  melt(measure.vars = c("observation", "expectation"),
       value.name = "frequency") %>%
  .[, proportion := frequency / sum(frequency), by = variable] %T>% 
  print
glm(
  frequency ~ 
    -1 + blood + 
    offset(sum(d[variable == "observation"]$frequency) %>% log %>% rep(4)),
  family = poisson,
  data = d[variable == "observation"]
) %>% 
  confint %>% 
  exp %>%
  as.data.table(keep.rownames = "blood") %>%
  .[, blood := gsub("blood", "", blood)] %>% 
  merge(d[variable == "observation"], .) %>%
  .[, variable := NULL] %>%
  xtable(caption = "血型頻率與比例估計。",
         label = "table:chisq_goodness",
         digits = 3)
chisq.test(
  obs.val,
  p = exp.p,
  rescale.p = T,
  simulate.p.value = T,
  B = 4999
)


## chi-squared independent test
d <-
  matrix(c(2, 10, 68, 30, 10, 50, 60, 19, 101), 3, byrow = T) %>%
  set_rownames(c("A", "B", "C")) %>%
  set_colnames(c("low", "median", "high"))
chisq.test(d)
chisq.test(d, simulate.p.value = T, B = 4999)
# ufs::cramersV(d)
# ufs::confIntV(d)
# rcompanion::cramerV(d, ci = T, bias.correct = T)
chisq.val <- chisq.test(d)$statistic
C <- sqrt(chisq.val/(chisq.val + sum(d)))
W <- sqrt(C^2 / (1-C^2))
W # Cohan's W
mc <-
  combn(1:nrow(d), 2, function(x) {
    chisq.test(d[, c(x[1], x[2])], simulate.p.value = T, B = 4999)
  }) %>%
  .[c(1, 3), ] %>%
  as.matrix %>%
  set_rownames(c("chisq", "p")) %>%
  set_colnames(combn(rownames(d), 2, function(x) {
    paste0(x[1], "-", x[2])
  })) %T>%
  print %>%
  .["p",] %>%
  unlist %>%
  p.adjust %>%
  multcompLetters %>%
  .$Letters %T>%
  print
d %>% as.table %>% prop.table
d %>% as.table %>% plot
d.long <-
  merge(
    d %>%
      reshape2::melt(
        varnames = c("location", "income"),
        value.name = "frequency"
      ) %>%
      as.data.table,
    d %>% as.table %>% prop.table(margin = 1) %>%
      reshape2::melt(
        varnames = c("location", "income"),
        value.name = "ratio by location"
      ) %>%
      as.data.table,
    by = c("location", "income")
  ) %>%
  merge(
        d %>% as.table %>% prop.table(margin = 2) %>%
      reshape2::melt(
        varnames = c("location", "income"),
        value.name = "ratio by income"
      ) %>%
      as.data.table
  ) %T>%
  print
f1 <-
  ggplot(d.long, aes(location, `ratio by location`)) +
  geom_col(aes(fill = income)) +
  theme_pubr(10, border = T) +
  annotate(
    "text",
    label = rowSums(d),
    x = 1:3,
    y = 1.1,
    size = 10 * 0.352777778
  ) +
  annotate(
    "text",
    label = mc,
    x = 1:3,
    y = 1.2,
    size = 10 * 0.352777778
  ) +
  scale_y_continuous("ratio", breaks  = seq(0, 1, 0.2), limits = c(0, 1.2))
f2 <-
  ggplot(d.long, aes(income, `ratio by income`)) +
  geom_col(aes(fill = location)) +
  theme_pubr(10, border = T) +
  annotate(
    "text",
    label = colSums(d),
    x = 1:3,
    y = 1.1,
    size = 10 * 0.352777778
  ) +
  scale_y_continuous("ratio", breaks  = seq(0, 1, 0.2), limits = c(0, 1.2))
windows(6, 2.5)
ggarrange(f1, f2, nrow = 1, align = "hv", labels = "auto", legend = "right")
ggsave("chisq_independent.pdf")
