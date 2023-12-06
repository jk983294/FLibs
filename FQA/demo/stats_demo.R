library(data.table)
#library(FQA)

dt <- FQA::gen_test_data()
dim(dt)
FQA::cal_half_life(dt, cols = c('close', 'f0', 'f1'))
FQA::acf(dt, cols = c('close', 'f0', 'f1'))

cols <- c("open", "high", "low", "close", "f0", "f1")
ftp <- dt[, FM::pcor(.SD, threads = 2L), .SDcols = cols]
FQA::select_xx_xy(ftp, cols, 0.9)
