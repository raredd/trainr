## save as r data/csv file for package

N <- 50
path <- system.file(package = 'trainr')
source(file.path(path, 'scripts', '00-gen_data.R'))

f <- function(x)
  file.path('.', 'inst','extdata', 'rdata', x)

save(blabs, file = f('blabs.rda'))
save(demo, file = f('demo.rda'))
save(elabs, file = f('elabs.rda'))
save(fwup, file = f('fwup.rda'))
save(resp, file = f('resp.rda'))
save(surv, file = f('surv.rda'))
save(tox, file = f('tox.rda'))

## copy r data to /data directory
# system("cp ./inst/extdata/rdata/* ./data")

f <- function(x)
  file.path('.', 'inst','extdata', 'csv', x)

write.csv(blabs, file = f('blabs.csv'), row.names = FALSE)
write.csv(demo, file = f('demo.csv'), row.names = FALSE)
write.csv(elabs, file = f('elabs.csv'), row.names = FALSE)
write.csv(fwup, file = f('fwup.csv'), row.names = FALSE)
write.csv(resp, file = f('resp.csv'), row.names = FALSE)
write.csv(surv, file = f('surv.csv'), row.names = FALSE)
write.csv(tox, file = f('tox.csv'), row.names = FALSE)
