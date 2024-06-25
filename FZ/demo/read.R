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
