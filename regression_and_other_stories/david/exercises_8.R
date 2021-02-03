# library("rstanarm")
# source("./exercises_6.R")

rss <- function(x, y, a, b){
    resid <- y - (a + b*x)
    return(sum(resid^2))
}

datapath <- c("../../../ROS-Examples/ElectionsEconomy/data/hibbs.dat")



hibbs <- read.table(datapath, header=TRUE)
head(hibbs)

# ressum <- rss(hibbs$vote, hibbs$growth, 1, 1)


a_hat <- 46.3
b_hat <- 3

b_vals <- seq(-10, 10, by=0.1)
a_vals <- seq(20, 60, by=0.1)
# for (b_vals in seq(-10, 10) {
b_rss <- function(b){
    return(rss(hibbs$growth, hibbs$vote, a_hat, b))
}
a_rss <- function(a){
    return(rss(hibbs$growth, hibbs$vote, a, b_hat))
}

bs_ressum <- sapply(b_vals, b_rss)
as_ressum <- sapply(a_vals, a_rss)

pdf('./plots/a_values.pdf')
plot(a_vals, as_ressum, pch="*")
# legend("topright", legend=c("Intercept", "x_data", "sigma"), col=c("blue", "red", "green"), pch=c("*", "*", "*"), inset=c(0.02, 0.02))
dev.off()

pdf('./plots/b_values.pdf')
plot(b_vals, bs_ressum, pch="*")
# legend("topright", legend=c("Intercept", "x_data", "sigma"), col=c("blue", "red", "green"), pch=c("*", "*", "*"), inset=c(0.02, 0.02))
dev.off()



# }