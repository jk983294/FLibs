library(data.table)
#library(FQA)

dt <- FQA::gen_test_data()
dim(dt)
FQA::cal_half_life(dt, cols = c('close', 'f0', 'f1'))
FQA::acf(dt, cols = c('close', 'f0', 'f1'))
