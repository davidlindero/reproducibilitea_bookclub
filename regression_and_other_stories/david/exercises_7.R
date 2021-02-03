library("rstanarm")
source("./exercises_6.R")

# print(Sys.time())

## 7.7 
## a

fake <- simulate_n_datapoints(2, 3, 100, 5, output_scores=TRUE, verbose=TRUE, xmax=20)

lm_xy <- stan_glm(y_data ~ x_data, data=fake,
                  family=gaussian(link="identity"),
                  seed=12345, refresh=0)

pdfname <- c('./plots/data_and_regressionline_7.7_', Sys.time(), '.pdf')
pdf(implode(pdfname))
plot(fake$x_data, fake$y_data, main = "Regression and data",
 xlab = "X data", ylab = "Y scores",
 pch = 19, frame = FALSE)
abline(lm_xy, col = "pink")
dev.off()  


## b

