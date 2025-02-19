library(hmtp)
library(mlr3extralearners)
library(lmtp)
library(future)

gendata <- function(n, betap) {
	z1 <- rnorm(n)
	z2 <- rnorm(n)
	z3 <- rnorm(n)
	z4 <- rnorm(n)

	# x1 <- exp(z1)
	# x2 <- z2 / (1 + exp(z1)) + 10
	# x3 <- (z1*z3 / 25 + 0.6)^3
	# x4 <- (z2 + z4 + 20)^2

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
						 # x1 = x1,
						 # x2 = x2,
						 # x3 = x3,
						 # x4 = x4,
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

learners <- list("glm", "earth", "lightgbm", "ranger", list("nnet", trace = FALSE))
learners = c("glm", "earth")

set.seed(534)
dat <- gendata(1e3, 0)

htmle <- hmtp_tmle(dat, "A", "Y", paste0("z", 1:4),
									 shift = static_binary_on, folds = 1,
									 learners_trt = learners,
									 learners_zero = learners,
									 learners_positive = learners,
									boot = FALSE)

hmtp_aipw(dat, "A", "Y", paste0("z", 1:4),
									 shift = static_binary_on, folds = 1,
									 learners_trt = learners,
									 learners_zero = learners,
									 learners_positive = learners)

aipw <- lmtp_sdr(dat, "A", "Y", paste0("z", 1:4), shift = static_binary_on,
								 folds = 1, outcome_type = "continuous",
								 learners_trt = learners, learners_outcome = learners)

tmle <- lmtp_tmle(dat, "A", "Y", paste0("z", 1:4), shift = static_binary_on,
				 folds = 1, outcome_type = "continuous",
				 learners_trt = learners, learners_outcome = learners)

