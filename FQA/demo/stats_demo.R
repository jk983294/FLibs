library(data.table)
#library(FQA)

dt <- FQA::gen_test_data()
dim(dt)
FQA::cal_half_life(dt, cols = c('close', 'f0', 'f1'))
FQA::acf(dt, cols = c('close', 'f0', 'f1'))

cols <- c("open", "high", "low", "close", "f0", "f1")
ftp <- dt[, FM::pcor(.SD, threads = 2L), .SDcols = cols]
FQA::select_xx_xy(ftp, cols, 0.9)

file_ <- "XX.20210915.csv"
fnames_file_ <- "test_teformula.def"
lines <- readLines(fnames_file_)
fnames <- as.data.table(FZ::expr_split_to_dt(lines, ",", TRUE))
dt <- FQA::parse_xx(file_, fnames$f0)
