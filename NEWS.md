# iLab 0.2.1

Minor fixes to prevent `no visible binding for global variable 'x'` appearing in notes.
This occurred when using `names(df) <- "x"`, or dplyr functions and referring to column names. Switching `names()` to `colnames()` and setting variables with in dplyr functions using `.data$` or `.env$` from the `rlang` package fixes this issue. 

# iLab 0.2.0

Initial update following minor additions (**technically the same as last version of 0.1.0 to start using version tracking**)


# iLab 0.1.0

Initial package creation, functions (`get_dir()`, `make_campaign_dir()`, `make_datasheet()`, `na_cols()`, `ref_library()`, `skeleton_emobs`), and data (`families , `iLab:::parkID`)


