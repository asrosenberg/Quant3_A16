library(causalA16)
library(data.table)
library(ggplot2)
library(texreg)

dat <- load_dataset("fearon_laitin_03")
setDT(dat)
setkey(dat, onset)

ggplot(dat, aes(onset)) + geom_histogram() + theme_bw()
range(dat$onset)
table(dat$onset)
dat <- dat[onset < 4]

mod1 <- glm(onset ~ warl + gdpenl + lpopl1 + lmtnest + ncontig + Oil + nwstate +
  instab + polity2l + ethfrac + relfrac, family = binomial(link = "logit"),
  data = dat)
screenreg(mod1)
texreg(mod1)
dim(model.matrix(mod1))
