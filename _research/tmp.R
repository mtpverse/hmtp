# library(hmtp)
library(mlr3extralearners)
library(mlr3pipelines)

gendata <- function(n, betap) {
	z1 <- rnorm(n)
	z2 <- rnorm(n)
	z3 <- rnorm(n)
	z4 <- rnorm(n)

	pi <- plogis(betap - z1 + 0.5*z2 - 0.25*z3 - 0.1*z4)
	A <- rbinom(n, 1, pi)

	delta <- rbinom(n, 1, plogis(-0.4*z1^2 + 0.1*z2 + 0.8*z3 - 0.3*z4 + 2*A))
	pid_1 <- plogis(-0.4*z1^2 + 0.1*z2 + 0.8*z3 - 0.3*z4 + 2)
	delta_1 <- rbinom(n, 1, plogis(-0.4*z1^2 + 0.1*z2 + 0.8*z3 - 0.3*z4 + 2))
	S <- exp(0.1 + 0.2*z1 + 0.4*z2 + 0.8*z3 + 0.3*z4 + 2*A)
	S_1 <- exp(0.1 + 0.2*z1 + 0.4*z2 + 0.8*z3 + 0.3*z4 + 2)

	data.frame(z1 = z1,
						 z2 = z2,
						 z3 = z3,
						 z4 = z4,
						 pi = pi,
						 A = A,
						 delta = delta,
						 S = S,
						 Y = delta*S,
						 pid_1 = pid_1,
						 S_1 = S_1,
						 Y_1 = delta_1*S_1)
}

# truth <- mean(gendata(1e7, 0)$Y_1)
# summary(gendata(1e7, 0)$pi)
# summary(gendata(1e7, -3)$pi)
truth <- 11.2

dat <- gendata(1e4, 2)

hmtp_tmle(dat, "A", "Y", paste0("z", 1:4),
					shift = static_binary_on, folds = 1,
					learners_trt = c("glm", "earth", "ranger", "lightgbm"),
					learners_zero = list(list("glm", filter = po("mutate", mutation = list(z1 = ~ z1^2)))),
					learners_positive = "glm")

hmtp_tmle(dat, "A", "Y", paste0("z", 1:4),
					shift = static_binary_on, folds = 1,
					learners_trt = c("glm", "earth", "ranger", "lightgbm"),
					learners_zero = list(list("glm", filter = po("mutate", mutation = list(z1 = ~ z1^2)))),
					learners_positive = "glm",
					boot = FALSE)

hmtp_tmle(dat, "A", "Y", paste0("z", 1:4),
					shift = static_binary_on, folds = 1,
					learners_trt = c("glm", "earth", "ranger", "lightgbm"),
					learners_zero = list(list("glm", filter = po("mutate", mutation = list(z1 = ~ z1^2)))),
					learners_positive = "glm",
					control = hmtp_control(.twostep = F))

