library(ggplot2)
library(reshape2)

setwd("~/regularity/analysis")
sizes <- data.frame(read.csv("sizes.csv"))
names(sizes) <- c("Algorithm", "Alphabet", "Initial size", "Derivative", "Size")

exp.plain.model <- lm(Size ~ exp(Derivative), subset(sizes, Algorithm %in% "plain"))
exp.smart.model <- lm(Size ~ exp(Derivative), subset(sizes, Algorithm %in% "smart"))

label_for <- function(name, model) {
  s <- summary(model)
  return(sprintf("%s\ny = %f + %fe^x\nR^2=%f", name, s$coefficients[1], s$coefficients[2], s$r.squared))
}

plainLabel <- label_for("plain", exp.plain.model)
smartLabel <- label_for("smart", exp.smart.model)

p <- ggplot(sizes, aes(group=Algorithm, x=Derivative,y=Size)) + scale_x_discrete(limits=seq(1,10))

expPlot <- p + geom_point(aes(color=Algorithm)) + 
    geom_text(data=subset(sizes, Size > 10000 & Algorithm == "plain"), nudge_x=-0.12*subset(sizes, Size > 10000 & Algorithm == "plain")$Derivative, nudge_y=20000, aes(label=Size)) +
    geom_text(data=subset(sizes, Size > 10000 & Algorithm == "smart"), nudge_x=-0.08*subset(sizes, Size > 10000 & Algorithm == "smart")$Derivative, nudge_y=20000, aes(label=Size)) +
    geom_smooth(data=subset(sizes, Algorithm %in% "smart"), method="lm", aes(color=Algorithm), formula=(y ~ exp(x)), se=FALSE, linetype = 1) + 
    geom_smooth(data=subset(sizes, Algorithm %in% "plain"), method="lm", aes(color=Algorithm), formula=(y ~ exp(x)), se=FALSE, linetype = 1) +
    scale_color_discrete(labels=c(plainLabel, smartLabel))

logPlot <- p + geom_point(aes(color=Algorithm)) + scale_y_log10() +
    geom_text(data=subset(sizes, Size > 10000), nudge_x=-0.7, nudge_y=0.25, aes(label=Size)) +
    geom_smooth(data=subset(sizes, Algorithm %in% "smart"), method="lm", aes(color=Algorithm), formula=(y ~ x), se=FALSE, linetype = 1) + 
    geom_smooth(data=subset(sizes, Algorithm %in% "plain"), method="lm", aes(color=Algorithm), formula=(y ~ x), se=FALSE, linetype = 1)

ggsave("exp.svg", plot=expPlot)
ggsave("log.svg", plot=logPlot)

expPlot
