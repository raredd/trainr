### utils: 15-000
# intr, worst_tox, tabler_by, binconr, surv_table, psum, dmy, print_counts,
# ipr, num2char
###


#' Interval formatter
#' 
#' Calculate summary statistic with range or confidence interval.
#' 
#' @param ... numeric vector or string of numeric vectors
#' @param fun summary stat function, usually \code{\link{mean}} or 
#' \code{\link{median}}
#' @param conf width of confidence interval in \code{[0,1]}; if \code{NULL} 
#' (default), returns min and max of \code{...}
#' @param digits number of digits (includes trailing 0s)
#' @param na.rm logical; if \code{TRUE}, any \code{\link{NA}} and \code{NaN}
#' are removed from \code{...} before \code{fun} and \code{\link{quantile}} are
#' computed
#' 
#' @examples
#' intr(1:10)
#' intr(1:10, conf = .95)
#' 
#' @export

intr <- function(..., fun = median, conf = NULL, digits = 0, na.rm = FALSE) {
  lst <- list(...)
  if (is.null(conf) || conf == 0 || 
        findInterval(conf, c(0, 1), rightmost.closed = FALSE) != 1)
    conf <- 1
  sapply(lst, function(x) {
    bounds <- quantile(x, c((1 - conf) / 2 * c(1, -1) + c(0, 1)), na.rm = na.rm)
    bounds <- round(bounds, digits = digits)
    val <- round(fun(x, na.rm = na.rm), digits = digits)
    if (!conf %in% c(0, 1))
      sprintf('%s (%s%% CI: %s - %s)', val, conf * 100, bounds[1], bounds[2])
    else sprintf('%s (range: %s - %s)', val, bounds[1], bounds[2])
  })
}

#' Worst toxicities
#' 
#' Get worst toxicity grades by subject, toxicity.
#' 
#' @param dat toxicity data
#' @param id id variable
#' @param tox_desc toxicity description
#' @param tox_grade toxicity grade as a factor
#' 
#' @return
#' A list of length three with worst toxicities, the original data frame, and
#' indices of duplicates removed from \code{dat}.
#' 
#' @export

tox_worst <- function(dat, id = 'id', tox_desc = 'tox_desc',
                      tox_grade = 'tox_grade') {
  ## make sure grades are properly ordered
  if (!is.factor(dat[, tox_grade]))
    stop('\'tox_grade\' should be a factor')
  ## sort by id, toxicity, and grade
  dat <- dat[order(dat[, id], dat[, tox_desc], -xtfrm(dat[, tox_grade])), ]
  idx <- which(duplicated(dat[, c(id, tox_desc)]))
  list(tox_worst = dat[-idx, ], dat = dat, duplicates = idx)
}

#' tabler_by
#' 
#' This function is helpful to make simple stratified tables, faster and 
#' easier to use than \code{\link[tables]{tabular}}.
#' 
#' \code{varname} and \code{byvar} should be factors, and the levels will
#' appear in the output as they occur in \code{levels(x)}.
#' 
#' \code{n} is used to calculate the percentages. If missing, the output will
#' only show counts in the table. If given, \code{length(n)} should be one or
#' equal to the number of levels of \code{byvar}.
#' 
#' If one \code{n} is given, \code{tabler_by} assumes that this is the total
#' population for a subgroup, i.e., if creating a table for a subset of the 
#' data, it is only necessary to provide the total \code{n} for that group.
#' 
#' If more than one \code{n} is given, \code{tabler_by} assumes that the
#' entire data set is given to \code{dat} and will use the corresponding 
#' \code{n} to show percentages out of each respective subgroup.
#' 
#' @param dat a data frame; variables \code{varname} and \code{byvar} should
#' be factors
#' @param varname variable with subgroups to count
#' @param byvar stratification variable
#' @param n number in each group; see details
#' @param order logical; order the result by decreasing frequency
#' @param zeros optional character string replacement for cells which have
#' zero counts; will appear as \code{0 (0\%)} if not given
#' @param pct.col logical; if \code{TRUE}, percents are separated into new
#' columns
#' 
#' @examples
#' ## generate data
#' set.seed(1)
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' tox <- data.frame(id = rep(1:10, 10), phase = 1:2,
#'                   tox_code = f(rawr::ctcae_v4$tox_code[1:25]),
#'                   tox_grade = f(1:3, prob = c(.6, .3, .1)),
#'                   stringsAsFactors = FALSE)
#' 
#' n <- table(tox[1:10, ]$phase)
#' tox <- cbind(tox,
#'   rawr::match_ctc(tox$tox_code)$matches[, c('tox_cat', 'tox_desc')])
#' 
#' tox <- within(tox, {
#'   phase <- factor(phase)
#'   tox_grade <- factor(tox_grade)
#'   tox_cat <- factor(tox_cat)
#'   tox_desc <- factor(tox_desc)
#' })
#' 
#' ## get worst toxicities by casenum by grade
#' tox <- tox_worst(tox)$tox_worst
#' 
#' ## summarize and format matrix for printing
#' out <- cbind(tabler_by(tox, 'tox_desc',
#'                        'phase', n = n, zeros = '-')[, 1, drop = FALSE],
#'              tabler_by(tox[tox$phase == '1', ], 'tox_desc',
#'                        'tox_grade', n = n[1], zeros = '-'),
#'              tabler_by(tox[tox$phase == '2', ], 'tox_desc',
#'                        'tox_grade', n = n[2], zeros = '-'))
#' out <- out[order(as.numeric(out[, 1]), decreasing = TRUE), ]
#' 
#' cgroup <- c(sprintf('Total<br /><font size=1>n = %s</font>', sum(n)),
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2]))
#' 
#' library('htmlTable')
#' htmlTable(out, ctable = TRUE, cgroup = cgroup, n.cgroup = c(1, 4, 4),
#'     caption = 'Table 1: Toxicities<sup>&dagger;</sup> by phase and grade.',
#'     col.columns = rep(c('grey97','none','grey97'), times = c(1, 4, 4)),
#'     col.rgroup = rep(rep(c('none', 'grey97'), each = 5), 10),
#'     tfoot = paste0('<font size=1><sup>&dagger;</sup>Percentages represent ',
#'             'proportion of patients out of respective phase total.</font>'))
#'             
#' @export

tabler_by <- function(dat, varname, byvar, n, order = FALSE, zeros,
                      pct.col = FALSE) {
  if (!all(sapply(dat[, c(varname, byvar)], is.factor)))
    stop('\'varname\' and \'byvar\' must be factors')
  
  ## split data by varname, get totals overall and for each level of byvar
  l <- split(dat, dat[, varname])
  res <- do.call('rbind', lapply(l, function(x)
    c(Total = length(x[, varname]), tapply(x[, varname], x[, byvar], length))))
  res1 <- res[, -1]
  res1[is.na(res1)] <- 0
  
  ## this will add percents: N (x%) to each column where n for each group
  if (!missing(n)) {
    nr <- nrow(res1)
    ## if one n is given, assume this is the total of a subgroup
    ## if > 1, assume that these correspond to the size of each level of byvar
    ## i.e., if calculating overall totals, give the n for each group
    ## if calculating totals for a subgroup, give one n since this group
    ## will have the same overall n
    if (length(n) == 1L)
      n <- rep(n, ncol(res1))
    if (length(n) != nlevels(dat[, byvar]))
      stop('\'n\' should be 1 or equal to nlevels(byvar)')
    res1 <- matrix(res1)
    mat <- round(res1 / matrix(rep(n, each = nr)) * 100)
    res1 <- matrix(sprintf('%s (%s%%)', res1, mat),
                   nrow = nr, ncol = length(n))
    # if (!missing(zeros))
    #   res1 <- gsub('0 \\(0%\\)', zeros, res1)
  }
  
  zzz <- if (pct.col) {
    res2 <- apply(res1, 1, paste0, collapse = ' ')
    res2 <- as.matrix(setNames(read.table(text = gsub('\\(|\\)|%', '', res2),
                                          colClasses = 'character'),
                               interleave(colnames(res)[-1],
                                          rep('%', ncol(res1)))))
    cbind(Total = res[, 1, drop = FALSE], res2)
  } else `colnames<-`(cbind(Total = res[, 1], res1), colnames(res))
  
  if (order) {
    zzz[, 1] <- as.numeric(zzz[, 1])
    zzz <- zzz[order(zzz[, 1], decreasing = TRUE), ]
  }
  if (!missing(zeros))
    zzz <- gsub('0 \\(0%\\)|^0$', zeros, zzz)
  zzz
}

#' Exact binomial confidence interval formatter
#' 
#' Calculates exact binomial confidence interval (\code{\link[Hmisc]{binconf}})
#' and prints formatted string.
#' 
#' @param r successes
#' @param n observations
#' @param conf confidence level
#' @param digits number of digits after decimal
#' @param est logical; if \code{TRUE}, the point estimate is also printed
#' @param method method to use; see \code{\link[Hmisc]{binconf}}
#' 
#' @seealso
#' \code{\link[Hmisc]{binconf}}; \code{desmon::binci}
#' 
#' @examples
#' binconr(5, 10, est = FALSE)
#' binconr(5, 10, conf = .9)
#' 
#' @export

binconr <- function(r, n, conf = 0.95, digits = 0, est = TRUE, method = 'exact') {
  res <- round(Hmisc::binconf(r, n, alpha = 1 - conf, method = method) * 100, 
                digits = digits)
  zzz <- sprintf('%s%% CI: %s - %s%%', conf * 100, res[2], res[3])
  if (est) 
    zzz <- sprintf('%s%% (%s)', res[1], zzz)
  zzz
}

#' \code{survfit} summary table
#' 
#' Prints a formatted summary table with estimates and confidence intervals
#' for \code{\link{suvfit}} objects.
#' 
#' @param s an object of class \code{\link{survfit}}
#' @param digits number of digits after decimal
#' @param times vector of survival times
#' @param ... additional arguments passed to \code{\link{summary.surfit}}
#' 
#' @examples
#' \dontrun{
#' library('survival')
#' data(cancer)
#' fit <- survfit(Surv(time, status) ~ 1, data = cancer, conf.type = 'log-log')
#' surv_table(fit, times = 0:4 * 50, digits = 2)
#' }
#' 
#' @export

surv_table <- function(s, digits = 3, times = pretty(range(s$time)), ...) {
  tmp <- capture.output(summ <- surv_summary(s, digits = digits, 
                                             times = times, ...))
  f <- function(x, d = digits, vars = vars) {
    vars = colnames(x)
    tmpvar <- colnames(x)[grep('survival|std.err|lower|upper', 
                               colnames(x))]
    x[ , tmpvar] <- round(x[ , tmpvar], digits = d)
    surv <- sprintf('%s (%s, %s)', 
                    x[, colnames(x)[grepl('survival', colnames(x))]],
                    x[, colnames(x)[grepl('lower', colnames(x))]],
                    x[, colnames(x)[grepl('upper', colnames(x))]])
    `colnames<-`(cbind(x[, c(setdiff(vars, tmpvar), 'std.err')], surv),
                 c('Time','No. at risk','No. event','Std.Error',
                   sprintf('OR (%s%% CI)', s$conf.int * 100)))
  }
  if (is.list(summ))
    Map(f = f, summ) else f(summ)
}

#' Pairwise sum
#' 
#' Compute the pairwise sum of two or more vectors
#' 
#' Each vector passed in \code{...} must be equal in length. The function
#' coerces the vectors into a matrix and \code{\link{rowSums}} the rows.
#' 
#' @param ... numeric vectors
#' @param na.rm logical; if \code{TRUE}, omits missing values (including
#' \code{\link{NaN}}) from calculations
#' 
#' @return
#' A single vector of element-wise sums.
#' 
#' @seealso
#' \code{\link{pmin}}, \code{\link{pmax}}
#' 
#' @examples
#' x <- c(-1, NA, 4, 5)
#' y <- c(NA, NA, 6, -1)
#' 
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' @export

psum <- function(..., na.rm = FALSE) {
  dat <- do.call(cbind, list(...))
  res <- rowSums(dat, na.rm = na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}

#' Date parse
#' 
#' Parse day/month/year column data into standard date format.
#' 
#' For two-digit years, the origin year should be specified; otherwise, the
#' default of 1900 will be used. For NA year, month, or day, origin is used
#' for defaults, i.e., origin = c(15, 6, 2000) will convert missing days to
#' day 15, missing months to June, and missing years to 2000.
#' 
#' @param d,m,y day, month, year as single integers or vectors
#' @param origin vector of length 3 with origins for d, m, and y, respectively;
#' see details
#' 
#' @return A vector of \code{\link{Date}}-formatted strings.
#' 
#' @examples
#' dmy(25, 7, 87)
#' dmy(NA, NA, 2000:2005)
#' 
#' @export

dmy <- function(d, m, y, origin = c(1, 1, 1900)) {
  f <- function(a, b) {
    suppressWarnings(a <- as.numeric(a))
    ifelse(is.na(a), b, a)
  }
  y <- ifelse(nchar(y) <= 2, f(y, 0) + origin[3], f(y, 0))
  as.Date(sprintf('%04s-%02s-%02s', y, f(m, origin[2]), f(d, origin[1])))
}

#' Count formatter
#' 
#' Formats and prints vectors with Ns and percents.
#' 
#' @param x a vector of character strings (or factors)
#' @param sep separator; see \code{\link{ipr}}
#' 
#' @examples
#' table(tx <- rep(c('RCHOP','R-CVP','RCHOEP'), c(10, 6, 4)))
#' print_counts(tx)
#' 
#' @export

print_counts <- function(x, ...) {
  tt <- if (!is.table(x)) sort(table(x), decreasing = TRUE) else x
  ipr(sprintf('%s (n = %s, %s%%)', names(tt), tt, round(tt / sum(tt) * 100)), ...)
}

#' Collapse vectors for printing
#' 
#' Collapse a vector using commas and "and" for readability.
#' 
#' @param x a vector
#' @param sep separator
#' 
#' @examples
#' ipr(1)
#' ipr(1:2)
#' ipr(1:5, sep = ';')
#' 
#' ipr(Vectorize(num2char)(1:5, cap = FALSE))
#' 
#' @export

ipr <- function(x, sep = ',') {
  if (length(x) == 1) x else
    if (length(x) == 2) paste(x, collapse = ' and ') else
      sprintf('%s%s and %s', paste(x[-length(x)], sep = sep,
                                   collapse = paste0(sep, ' ')),
              sep, tail(x, 1))
}

#' Numeric to character string
#' 
#' Convert numeric to word representation.
#' 
#' @param num integer in [-999, 999]
#' @param informal logical; if \code{TRUE}, adds "and" before tens or ones
#' @param cap logical; if \code{TRUE}, capitalized first word
#' 
#' @examples
#' num2char(28, informal = TRUE)
#' 
#' nums <- c(-100, 110, 322, 012, 201, -152, 4)
#' Vectorize(num2char)(nums)
#' 
#' @export

num2char <- function(num, informal = FALSE, cap = TRUE) {
  if (num == 0) {if (cap) return('Zero') else return('zero')}
  neg <- FALSE
  if (num < 0) {neg <- TRUE; num <- abs(num)}
  if (!num %inside% c(1, 999)) 
    stop("I can't count that high")
  ## helpers
  key <- c('0'='','1'='one','2'='two','3'='three','4'='four','5'='five',
           '6'='six','7'='seven','8'='eight','9'='nine','10'='ten',
           '11'='eleven','12'='twelve','13'='thirteen','14'='fourteen',
           '15'='fifteen','16'='sixteen','17'='seventeen','18'='eighteen',
           '19'='nineteen','20'='twenty','30'='thirty','40'='forty','50'='fifty',
           '60'='sixty','70'='seventy','80'='eighty','90'='ninety',
           '100'='hundred')
  f1 <- function(x, informal = informal) { # for 1-99
    x <- as.numeric(x) # if string with leading 0s is passed
    z <- paste0(' and ',
                if (x %inside% c(21, 99) && (x %ni% seq(30, 100, 10)))
                  paste(key[as.character(as.numeric(substr(x, 1, 1)) * 10)], 
                        key[substr(x, 2, 2)], sep = '-')
                else 
                  key[as.character(as.numeric(x))])
    if (!informal) gsub(' and ', '', z) else z
  }
  f2 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(100, 999))
      paste0(key[substr(x, 1, 1)], ' hundred ', f1(substr(x, 2, 3), informal))
    else f1(x, informal = informal)
  }
  ## trim leading/trailing whitespace
  zzz <- trimws(f2(num, informal = informal))
  ## trim double whitespace
  zzz <- upcase(gsub('\\s{2,}', ' ', zzz))
  ## trim 'and' in special cases
  zzz <- ifelse(cap, upcase(gsub('And\ |\ and*$', '', zzz)),
                gsub('And\ |\ and*$', '', zzz))
  zzz <- ifelse(neg, paste0('negative ', tolower(zzz)), tolower(zzz))
  ifelse(cap, upcase(zzz), zzz)
}
