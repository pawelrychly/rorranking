.solver <- NULL

.onLoad <- function(libname, pkgname) {
  solvers <- ROI_installed_solvers()
  if (!is.na(solvers['glpk'])) {
	.solver <<- 'glpk'
  } else if (!is.na(solvers['symphony'])) {
	.solver <<- 'symphony'
  } else {
	stop("No ROI Symphony or GLPK plugin installed")
  }
}
