n <- 1000
x <- sort(rnorm(n, sd=1))
link_fun <- function(x){
  ifelse(abs(x)<1, 2+5*x -3*x^2 -x^3 + 10*sqrt(abs(x)), x)
}
y <- link_fun(x) + rnorm(n, sd=0.8)
plot(x,y)
nodes_internal <- seq(-1,1, by=0.5)
nodes_external <- c(-2,2)



# Fit ----
y_lm <- lm(y~x)
bs_df <- cbind.data.frame(1, splines::bs(x, knots=nodes_internal, Boundary.knots = nodes_external))
colnames(bs_df) <- paste0("V", 1:ncol(bs_df))
y_bs <- lm(as.formula(paste0("y~0+", paste(colnames(bs_df), collapse="+"))), data=bs_df)

ns_df <- cbind.data.frame(1, splines::ns(x, knots=nodes_internal, Boundary.knots = nodes_external))
colnames(ns_df) <- paste0("V", 1:ncol(ns_df))
y_ns <- lm(as.formula(paste0("y~0+", paste(colnames(ns_df), collapse="+"))), data=ns_df)

par(mfrow=c(1,2))
plot(x,y, main = "Fit", xlim=c(-5,5))
lines(x, y_lm$fitted.values, col="green", lwd=2)
lines(x, y_bs$fitted.values, col="red", lty=2, lwd=2)
lines(x, y_ns$fitted.values, col="cyan", lty=3, lwd=2)
legend("topleft", legend=c("Obs", "lm", "bs", "ns"),
       col=c("black", "green", "red", "cyan"), lty = c(NA,1,2,3), pch=c(1,NA, NA, NA), lwd=2)


# Pred ----
x2 <- sort(rnorm(n, sd=3))
y2 <- link_fun(x2) + rnorm(n, sd=0.8)

y_lm_pred <- predict.lm(object = y_lm, 
                    newdata = cbind.data.frame("x" = x2)
)
ns_df2 <- cbind.data.frame(1, splines::ns(x2, knots=nodes_internal, Boundary.knots = nodes_external))
colnames(ns_df2) <- paste0("V", 1:ncol(ns_df2))
y_ns_pred <- predict.lm(object = y_ns, 
                    newdata = ns_df2)
bs_df2 <- cbind.data.frame(1, splines::bs(x2, knots=nodes_internal, Boundary.knots = nodes_external))
colnames(bs_df2) <- paste0("V", 1:ncol(bs_df2))
y_bs_pred <- predict.lm(object = y_bs, 
                    newdata = bs_df2)

plot(x2,y2, main = "Predictions", xlim=c(-5,5))
lines(x2, y_lm_pred, col="green", lwd=2)
lines(x2, y_bs_pred, col="red", lty=2, lwd=2)
lines(x2, y_ns_pred, col="cyan", lty=3, lwd=2)
legend("topleft", legend=c("Obs", "lm", "bs", "ns"),
       col=c("black", "green", "red", "cyan"), lty = c(NA,1,2,3), pch=c(1,NA, NA, NA), lwd=2)