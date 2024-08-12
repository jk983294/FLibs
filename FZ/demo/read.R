in_csv_path_ <- "~/github/barn/train/housing.csv"
cols <- c("longitude", "latitude")
(dt <- FZ::read(in_csv_path_))
(dt <- FZ::read(in_csv_path_, cols))
in_fst_path_ <- "/tmp/20210914.1.fst"
(dt <- FZ::read(in_fst_path_))
(dt <- FZ::read(in_fst_path_, cols))
in_feather_path_ <- "/tmp/20210914.1.feather"
(dt <- FZ::read(in_feather_path_))
(dt <- FZ::read(in_feather_path_, cols))
(dt <- FZ::reads("~/github/barn/train/bydates/${YYYYMMDD}.csv", c(20230703, 20230704, 20230705)))
(dates <- FZ::fz_read_trading_days("/dat/ctp/trading_day.txt", 20230703, 20230705))
