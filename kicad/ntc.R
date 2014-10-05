library(ggplot2)
library(plyr)

ntc <- function(t, B=4000, T0=25)
    exp(B * (1 / (273.15 + t) - 1 / (273.15 + T0)))

custom_divider <- function(r1val, r2val, B) {
    function(t) {
        r1 = r1val
        r2 = r2val * ntc(t, B, 25)
        return (r2/(r1+r2))
    }
}

quantize <- function(x) {
    vq <- round(1023 * x)
    vq[vq>1023] <- 1023
    vq[vq<0] <- 0
    return (vq)
}

calc_div_value <- function(r.choice, t) {
    within(data.frame(t=t), {
        aref = 3.3 / 3
        # XXX B is approximate
        v <- 3.3 * custom_divider(r.choice$r1, r.choice$r2, B=4000)(t)
        q <- quantize(v / aref)
        q2 <- quantize(v * 2/3 / aref)
        q3 <- quantize(v * 1/3 / aref)
    })
}

r1.choice <- c(10e3,22e3,47e3,68e3,100e3)
r2.choice <- c(10e3,47e3,100e3)
r.choice <- expand.grid(r1 = r1.choice, r2 = r2.choice)
t <- seq(0, 100, 0.1)

div.vals <- adply(r.choice, 1, calc_div_value, t=t)
div.plot <- ggplot(div.vals, aes(x = t)) + geom_step(aes(y = q3)) + facet_grid(r2 ~ r1, labeller=label_both) + labs(x="Temperature/°C", y="10-bit value")
div.plot
