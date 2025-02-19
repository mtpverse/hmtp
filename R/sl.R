run_ensemble <- function(data, y, learners, outcome_type, id, folds) {
	mlr3superlearner::mlr3superlearner(
		data = data,
		target = y,
		library = learners,
		outcome_type = outcome_type,
		folds = folds,
		group = id,
		discrete = FALSE
	)
}
