# rm(list = ls())
# library(foreign)
# setwd('F:/extra/Sylvia MSc')
# data = read.spss('spss 1.sav')

data("BtheB", package = "HSAUR2")
BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject",
                        varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"),
                        direction = "long")
BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4))

library("lme4")
BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                       length + (1 | subject), data = BtheB_long,
                      REML = FALSE, na.action = na.omit)
BtheB_lmer2 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                        length + (time | subject), data = BtheB_long,
                      REML = FALSE, na.action = na.omit)
tidy(anova(BtheB_lmer1, BtheB_lmer2))
 
summary(BtheB_lmer1)
cftest(BtheB_lmer1)
 
library("gee")
osub <- order(as.integer(BtheB_long$subject))
BtheB_long <- BtheB_long[osub,]
btb_gee <- gee(bdi ~ bdi.pre + treatment + length + drug,
                data = BtheB_long, id = subject, family = gaussian,
                  corstr = "independence")

btb_gee1 <- gee(bdi ~ bdi.pre + treatment + length + drug,
                   data = BtheB_long, id = subject, family = gaussian,
                   corstr = "exchangeable")

data("respiratory", package = "HSAUR2")
resp <- subset(respiratory, month > "0")
resp$baseline <- rep(subset(respiratory, month == "0")$status,
                         rep(4, 111))
resp$nstat <- as.numeric(resp$status == "good")
resp$month <- resp$month[, drop = TRUE]

resp_glm <- glm(status ~ centre + treatment + gender + baseline
                + age, data = resp, family = "binomial")
resp_gee1 <- gee(nstat ~ centre + treatment + gender + baseline
                 + age, data = resp, family = "binomial", id = subject,
                 corstr = "independence", scale.fix = TRUE,
                 scale.value = 1)
resp_gee2 <- gee(nstat ~ centre + treatment + gender + baseline
                    + age, data = resp, family = "binomial", id = subject,
                    corstr = "exchangeable", scale.fix = TRUE,
                    scale.value = 1)

summary(resp_glm)

se <- summary(resp_gee2)$coefficients["treatmenttreatment",
                                       "Robust S.E."]
coef(resp_gee2)["treatmenttreatment"] +
  c(-1, 1) * se * qnorm(0.975)

exp(coef(resp_gee2)["treatmenttreatment"] +
         c(-1, 1) * se * qnorm(0.975))

# p <- 520/1000
# p = sum(data$psq_binary == 1)/nrow(data)
# sum(data$psq_binary == 1)/nrow(data) + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1)/nrow(data)*(1-sum(data$psq_binary == 1)/nrow(data)))
# 
# sum(data$psq_binary == 1)/nrow(data)
# sum(data$psq_binary == 1)/nrow(data) + c(-qnorm(0.975))*sqrt((1/nrow(data))*sum(data$psq_binary == 1)/nrow(data)*(1-sum(data$psq_binary == 1)/nrow(data)))

# 
# after = unique(mmm_e[c("Variable", "Value")])  %>%
#   group_by(Variable) %>%
#   filter(n() > 1)
# 
# before = unique(mmm_b[c("Variable", "Value")]) %>%
#   filter(Variable == after$Variable & Value == after$Value)

case545 <- data.frame(Case="545", 
                      Source=c("XX","X1"), PJ=c(68,21),SB=c(17,13),BW=c(7,15), SW=c(450,600))
case546 <- data.frame(Case="546",   
                      Source=c("XX","X1"), PJ=c(100,300),SB=c(12,34),BW=c(400,30), SW=c(300,500))
case547 <- data.frame(Case="547",   
                      Source=c("XX","X1"), PJ=c(55,200),SB=c(16,27),BW=c(100,23), SW=c(200,300))

library(dplyr)
DF <- ls(pattern = "case")

Output <- bind_rows(lapply(DF, function(DF){
  TMP <- get(DF)
  TMP <- TMP %>% 
    dplyr::select(grep(pattern = F,colSums( TMP != 0) == 0))
  TMP <- chisq.test(rbind(TMP[1,-c(1:2)],TMP[2,-c(1:2)]))
  TMP <- data.frame(X2=round(TMP$statistic,4),p=round(TMP$p.value, 4),case=DF)
  return(TMP)
}))

names(mmm_b) = c("Variable", "Value", "Count_b", "Percent_b", "ci_b")
names(mmm_e) = c("Variable", "Value", "Count_e", "Percent_e", "ci_e")
try = left_join(mmm_e, mmm_b)
try = try %>%
  group_by(Variable) %>%
  filter(., n() > 1) %>%
  ungroup()
