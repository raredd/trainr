#' Patient demographics data
#' 
#' Baseline demographics data for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{resp}}, \code{\link{surv}}, \code{\link{tox}}
#' \code{\link{blabs}}, \code{\link{elabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 16 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{site} \tab enrollment site \cr
#' \tab \code{sex} \tab sex \cr
#' \tab \code{race} \tab race \cr
#' \tab \code{ethnic} \tab ethnicity \cr
#' \tab \code{d_dob} \tab day of birth (dd) \cr
#' \tab \code{m_dob} \tab month of birth (mm) \cr
#' \tab \code{y_dob} \tab year of birth (yyyy) \cr
#' \tab \code{d_reg} \tab day of registration (dd) \cr
#' \tab \code{m_reg} \tab month of registration (mm) \cr
#' \tab \code{y_reg} \tab year of registration (yyyy) \cr
#' \tab \code{subtype} \tab genetic subtype, activated bcell-like or germinal
#' center bcell-like \cr
#' \tab \code{ens} \tab number of extra nodal sites \cr
#' \tab \code{stage} \tab ann arbor disease stage \cr
#' \tab \code{ecog} \tab ecog performance status \cr
#' \tab \code{ldh} \tab ldh > ULN, normal or elevated \cr
#' }
"demo"

#' Baseline labs
#' 
#' Baseline lab results for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{demo}}, \code{\link{resp}}, \code{\link{surv}}
#' \code{\link{tox}}, \code{\link{elabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 9 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{timepoint} \tab timepoint \cr
#' \tab \code{ltd} \tab sum of largest tumor diameters (cm) \cr
#' \tab \code{wbc} \tab white blood cell count (k/mcL) \cr
#' \tab \code{hgb} \tab hemoglobin (g/dL) \cr
#' \tab \code{platelets} \tab platelets  (k/mcL) \cr
#' \tab \code{neutrophils} \tab neutrophils (\%) \cr
#' \tab \code{lymphocytes} \tab lymphocytes (\%) \cr
#' \tab \code{monocytes} \tab monocytes (\%) \cr
#' }
"blabs"

#' End of treatment labs
#' 
#' End of treatment lab results for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{demo}}, \code{\link{resp}}, \code{\link{surv}}
#' \code{\link{tox}}, \code{\link{blabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 9 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{timepoint} \tab timepoint \cr
#' \tab \code{ltd} \tab sum of largest tumor diameters (cm) \cr
#' \tab \code{wbc} \tab white blood cell count (k/mcL) \cr
#' \tab \code{hgb} \tab hemoglobin (g/dL) \cr
#' \tab \code{platelets} \tab platelets  (k/mcL) \cr
#' \tab \code{neutrophils} \tab neutrophils (\%) \cr
#' \tab \code{lymphocytes} \tab lymphocytes (\%%) \cr
#' \tab \code{monocytes} \tab monocytes (\%) \cr
#' }
"elabs"

#' Response data
#' 
#' Response assessments for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{demo}}, \code{\link{tox}}, \code{\link{surv}},
#' \code{\link{blabs}}, \code{\link{elabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 5 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{resp} \tab response \cr
#' \tab \code{d_resp} \tab day of response assessment (dd) \cr
#' \tab \code{m_resp} \tab month of response assessment (mm) \cr
#' \tab \code{y_resp} \tab year of response assessment (yyyy) \cr
#' }
"resp"

#' Survival data
#' 
#' Survival assessments for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{demo}}, \code{\link{tox}}, \code{\link{resp}}
#' \code{\link{blabs}}, \code{\link{elabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 5 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{status} \tab status, alive or dead \cr
#' \tab \code{d_status} \tab day of survival assessment (dd) \cr
#' \tab \code{m_status} \tab month of survival assessment (mm) \cr
#' \tab \code{y_status} \tab year of survival assessment (yyyy) \cr
#' }
"surv"

#' Toxicity data
#' 
#' Treatment-related toxicity data for patients enrolled on 15-000.
#' 
#' @seealso \code{\link{demo}}, \code{\link{resp}}, \code{\link{surv}}
#' \code{\link{blabs}}, \code{\link{elabs}}
#' 
#' @format An object of class \code{data.frame} containing 50 observations and
#' 3 variables:
#' 
#' \tabular{rll}{
#' \tab \code{id} \tab patient id number \cr
#' \tab \code{tox_code} \tab toxicity code (CTCAE v4) \cr
#' \tab \code{tox_grade} \tab toxicity grade \cr
#' }
"tox"
