datadir <- "~/Downloads/data/train/"
source("prep-data.R")

## fit a logistic regression model
fm.bid.up <- glm(delta.bid > 0 ~
                     net.order.flow +
                     I(lag(delta.buy.dist)/lag(ts.diff)),
                 data=Obook, family=binomial(link="logit"))
summary(fm.bid.up)

# diagnostics for the regression model
layout(matrix(1:4, nrow=2))
plot(fm.bid.up)





