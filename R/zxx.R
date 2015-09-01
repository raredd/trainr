### internal utils: 15-000
# %ni%, upcase, strip_white, %inside%, surv_summary
###


'%ni%' <- Negate(`%in%`)

## uppercase first letter of string
upcase <- function(x)
  paste(toupper(substr(x, 1, 1)), substring(x, 2), sep = '', collapse = ' ')

## strip leading/trailing whitespace
strip_white <- function(x) gsub('^\\s+|\\s+$', '', x)

## numerics inside intervals
'%inside%' <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1] & x <= interval[2]
}

## survival:::print.summary.survfit
surv_summary <- function(s, digits = max(getOption('digits') - 4, 3), ...) {
  
  ## error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  
  x <- summary(s, ...)
  
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  if (!is.null(cl <- x$call)) {
    cat("Call: ")
    dput(cl)
  }
  omit <- x$na.action
  if (length(omit)) 
    cat(naprint(omit), "\n")
  if (x$type == "right" || is.null(x$n.enter)) {
    mat <- cbind(x$time, x$n.risk, x$n.event, x$surv)
    cnames <- c("time", "n.risk", "n.event")
  } else 
    if (x$type == "counting") {
      mat <- cbind(x$time, x$n.risk, x$n.event, x$n.enter, x$n.censor, x$surv)
      cnames <- c("time", "n.risk", "n.event", "entered", "censored")
    }
  if (is.matrix(x$surv)) 
    ncurve <- ncol(x$surv)
  else ncurve <- 1
  if (ncurve == 1) {
    cnames <- c(cnames, "survival")
    if (!is.null(x$std.err)) {
      if (is.null(x$lower)) {
        mat <- cbind(mat, x$std.err)
        cnames <- c(cnames, "std.err")
      } else {
        mat <- cbind(mat, x$std.err, x$lower, x$upper)
        cnames <- c(cnames, "std.err", 
                    paste("lower ", x$conf.int * 100, "% CI", sep = ""),
                    paste("upper ", x$conf.int * 100, "% CI", sep = ""))
      }
    }
  } else 
    cnames <- c(cnames, paste("survival", seq(ncurve), sep = ""))
  if (!is.null(x$start.time)) {
    mat.keep <- mat[ , 1] >= x$start.time
    mat <- mat[mat.keep, , drop = FALSE]
    if (is.null(dim(mat))) 
      stop(paste("No information available using start.time =", 
                 x$start.time, "."))
  }
  if (!is.matrix(mat)) 
    mat <- matrix(mat, nrow = 1)
  if (!is.null(mat)) {
    dimnames(mat) <- list(NULL, cnames)
    if (is.null(x$strata)) {
      cat("\n")
      invisible(prmatrix(mat, rowlab = rep("", nrow(mat))))
    } else {
      strata <- x$strata
      if (!is.null(x$start.time))
        strata <- strata[mat.keep]
      invisible(setNames(lapply(levels(strata), function(i) {
        who <- (strata == i)
        cat("\n               ", i, "\n")
        if (sum(who) == 1)
          prmatrix(mat[who, ])
        else prmatrix(mat[who, ], rowlab = rep("", sum(who)))
      }), levels(strata)))
    }
  } else 
    stop("There are no events to print. Use the option censored = TRUE ",
         "with the summary function to see the censored observations.")
}
