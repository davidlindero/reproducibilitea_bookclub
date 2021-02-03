library("rstanarm")
library("tractor.base")

is_polr <- function(object) {
    is(object, "polr")
}

is.binomial <- function(x) x == "binomial"
is.gaussian <- function(x) x == "gaussian"
is.gamma <- function(x) x == "Gamma"
is.ig <- function(x) x == "inverse.gaussian"
is.nb <- function(x) x == "neg_binomial_2"
is.poisson <- function(x) x == "poisson"
is.beta <- function(x) x == "beta" || x == "Beta regression"

.rename_aux <- function(family) {
  fam <- family$family
  if (is.gaussian(fam)) "sigma" else
    if (is.gamma(fam)) "shape" else
      if (is.ig(fam)) "lambda" else 
        if (is.nb(fam)) "reciprocal_dispersion" else NA
}

.aux_name <- function(object) {
  aux <- character()
  if (!is_polr(object)) {
    aux <- .rename_aux(family(object))
    if (is.na(aux)) {
      aux <- character()
    }
  }
  return(aux)
}

.median_and_madsd <- function(x) {
  cbind(Median = apply(x, 2, median), MAD_SD = apply(x, 2, mad))
}

simulate_n_datapoints <- function(a, b, n, th, output_scores=TRUE, verbose=FALSE, xmax=100) {
    x_data <- runif(n, min=0, max=xmax)
    errors <- rnorm(n, mean=0, sd=th)
    y_data <- a + b*x_data + errors

    all_data <- data.frame(x_data, y_data)

    lm_xy <- stan_glm(y_data ~ x_data, data=all_data,
                  family=gaussian(link="identity"),
                  seed=12345, refresh=0)

    # from stan_glm.print
    aux_nms <- .aux_name(lm_xy)
    mat <- as.matrix(lm_xy$stanfit)
    nms <- setdiff(rownames(lm_xy$stan_summary), c("log-posterior", aux_nms))
    ppd_nms <- grep("^mean_PPD", nms, value = TRUE)
    nms <- setdiff(nms, ppd_nms)
    coef_mat <- mat[, nms, drop = FALSE]
    estimates <- .median_and_madsd(coef_mat)
    more_aux <- .median_and_madsd(mat[, aux_nms, drop=FALSE])

    # concatenate to one line
    all_meds_and_mads <- data.frame("nums"=n, "intercept_med"=estimates["(Intercept)","Median"], "intercept_madsd"=estimates["(Intercept)","MAD_SD"], "x_data_med"=estimates["x_data","Median"], "x_data_madsd"=estimates["x_data","MAD_SD"], "sigma_med"=more_aux["sigma","Median"], "sigma_madsd"=more_aux["sigma","MAD_SD"])
    
    # if you want to plot the datapoints and report the fit
    if (verbose == TRUE) {
        print(lm_xy)

        print(sd(lm_xy$residuals))

        pdfname <- c('./plots/regressionplot_from_n_datapoints', Sys.time(), '.pdf')
        pdf(implode(pdfname))
        plot(x_data, y_data, main = "Super regression",
         xlab = "X data", ylab = "Y scores",
         pch = 19, frame = FALSE)
        abline(lm_xy, col = "pink")
        dev.off()        
    }

    # choose if you want to return data points or precision statistics
    if (output_scores == TRUE) {
        return(all_data)
    }
    if (output_scores == FALSE) {
        return(all_meds_and_mads)
    }
}

# str(summary(blabla))

# run things

ns_vec <- c()
for (i in seq(1, 300, by=10)) {
    ns_vec[i] <- as.integer(10^(i/100))
}
ns_values <- unique(ns_vec)
ns_values <- ns_values[ns_values > 5]

ns_values <- ns_values[!is.na(ns_values)]

all_simuls = data.frame()
for (ii in ns_values) {
    curr_simul <- simulate_n_datapoints(4, 2, ii, 9, output_scores=FALSE)
    all_simuls <- rbind(all_simuls, curr_simul)
}

pdf('./plots/development_dev_number_of_samples.pdf')
plot(all_simuls$nums, all_simuls$intercept_madsd, col="blue", xlab="Number of samples", ylim=c(0,5), pch="*")
points(all_simuls$nums, all_simuls$x_data_madsd, col="red", xlab="Number of samples", pch="*")
points(all_simuls$nums, all_simuls$sigma_madsd, col="green", xlab="Number of samples", pch="*")
legend("topright", legend=c("Intercept", "x_data", "sigma"), col=c("blue", "red", "green"), pch=c("*", "*", "*"), inset=c(0.02, 0.02))
dev.off()

pdf('./plots/actual_regression_values_nr_samples.pdf')
plot(all_simuls$nums, all_simuls$intercept_med, col="blue", xlab="Number of samples", ylim=c(0,10), pch="*")
points(all_simuls$nums, all_simuls$x_data_med, col="red", xlab="Number of samples", pch="*")
points(all_simuls$nums, all_simuls$sigma_med, col="green", xlab="Number of samples", pch="*")
legend("topright", legend=c("Intercept", "x_data", "sigma"), col=c("blue", "red", "green"), pch=c("*", "*", "*"), inset=c(0.02, 0.02))
dev.off()


# simulated_data <- simulate_n_datapoints(4, 2, 100, 15, verbose=TRUE)





#png('./rnormhist.png')
#hist(x)
#dev.off()

