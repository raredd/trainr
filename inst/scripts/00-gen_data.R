## generate data
options(stringsAsFactors = FALSE)
f1 <- function(x, size = N, ...) sample(x, size = size, replace = TRUE, ...)


set.seed(1)
demo <- data.frame(
  id   = 1:N,
  site = c('DFCI','MGH','BWH')[f1(1:3)],
  sex  = c('Male','Female')[f1(1:2)],
  race = c('White','Black','Asian')[f1(1:3, prob = c(.7, .2, .1))],
  ethnic = c('Non-Hispanic','Hispanic')[f1(1:2, prob = c(.8, .2))],
  d_dob = f1(1:28),
  m_dob = f1(1:12),
  y_dob = f1(1920:1975),
  d_reg = f1(1:28),
  m_reg = f1(1:6),
  y_reg = f1(2012:2013),
  # subtype = c('GCB','ABC','Unspecified')[f1(1:3)],
  subtype = c('GCB','ABC')[f1(1:2)],
  ens = f1(0:3),
  stage = f1(1:4),
  ecog = f1(0:4),
  ldh = c('Elevated','Normal')[f1(1:2)])


## toxicity codes
tox <- data.frame(
  id = f1(1:N, size = N * 10),
  tox_code = f1(rawr::ctcae_v4$tox_code[1:16][-2], size = N * 10),
  tox_grade = factor(f1(1:5, size = N * 5, prob = c(.4, .3, .25, .05, 0))),
  stringsAsFactors = FALSE)
tox <- `rownames<-`(tox[with(tox, order(id, tox_code)), ], NULL)


## baseline lab data
blabs <- elabs <- data.frame(
  id = 1:N,
  timepoint = 'Baseline',
  ltd = round(runif(N, 50, 100), 1),
  wbc = round(runif(N, 5, 20), 1),
  hgb = round(runif(N, 5, 20), 1),
  platelets = rpois(N, 150),
  neutrophils = rpois(N, 50),
  lymphocytes = rpois(N, 30),
  monocytes = rpois(N, 10))

## eot labs
elabs[] <- lapply(elabs, function(x)
  if (is.numeric(x)) x * f1(seq(0, 2, length.out = N)) else x)
elabs <- within(elabs, {
  id <- 1:N
  timepoint <- 'EOT'
})


## outcome data
resp <- data.frame(
  id = 1:N,
  resp = c('NE','PD','SD','MR','PR','CR')[f1(1:6, prob = c(0, .2, .2, .2, .2, .2))],
  d_resp = f1(1:28),
  m_resp = f1(7:10),
  y_resp = 2013)

surv <- data.frame(
  id = 1:N,
  status = c('Alive','Dead')[f1(1:2, prob = c(.8, .2))],
  d_status = f1(1:28),
  m_status = f1(11:12),
  y_status = 2013)

fwup <- data.frame(
  id = 1:N,
  fwup = c(NA,'Lost to FU')[(surv$status %in% 'Alive') + 1L],
  d_fwup = 31,
  m_fwup = 12,
  y_fwup = 2013)

## clean-up
rm(f1)
