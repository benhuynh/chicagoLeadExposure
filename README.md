# Estimated childhood lead exposure from drinking water in Chicago 

Reproducible code for our manuscript `Estimated childhood lead exposure from drinking water in Chicago`. 

Data files can be found at `link`. After downloading necessary data files, run the code in numerical order.

Please direct any corrections or inquiries to bhuynh@jhu.edu.

Our session info is below.
```
> sessioninfo::session_info(pkgs = "attached")
─ Session info ───────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 3.6.3 (2020-02-29)
 os       macOS  10.16
 system   x86_64, darwin15.6.0
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       America/New_York
 date     2023-10-10
 rstudio  1.2.1335 (desktop)
 pandoc   NA

─ Packages ───────────────────────────────────────────────────────────────────────────────
 package      * version  date (UTC) lib source
 bonsai       * 0.2.1    2022-11-29 [1] CRAN (R 3.6.3)
 broom        * 1.0.4    2023-03-11 [1] CRAN (R 3.6.3)
 censusxy     * 1.1.1    2022-12-06 [1] CRAN (R 3.6.3)
 cowplot      * 1.1.1    2020-12-30 [1] CRAN (R 3.6.2)
 dials        * 1.2.0    2023-04-03 [1] CRAN (R 3.6.3)
 dplyr        * 1.1.1    2023-03-22 [1] CRAN (R 3.6.3)
 egg          * 0.4.5    2019-07-13 [1] CRAN (R 3.6.3)
 EnvStats     * 2.8.1    2023-08-22 [1] CRAN (R 3.6.3)
 forcats      * 0.5.1    2021-01-27 [1] CRAN (R 3.6.2)
 ggallin      * 0.1.1    2017-10-02 [1] CRAN (R 3.6.3)
 ggdensity    * 1.0.0    2023-02-09 [1] CRAN (R 3.6.3)
 ggeasy       * 0.1.4    2023-03-12 [1] CRAN (R 3.6.3)
 ggplot2      * 3.4.2    2023-04-03 [1] CRAN (R 3.6.3)
 ggthemes     * 4.2.4    2021-01-20 [1] CRAN (R 3.6.2)
 glmnet       * 4.1-3    2021-11-02 [1] CRAN (R 3.6.2)
 gridExtra    * 2.3      2017-09-09 [1] CRAN (R 3.6.0)
 infer        * 1.0.4    2022-12-02 [1] CRAN (R 3.6.3)
 janitor      * 2.1.0    2021-01-05 [1] CRAN (R 3.6.2)
 lightgbm     * 3.3.5    2023-01-16 [1] CRAN (R 3.6.3)
 lubridate    * 1.8.0    2021-10-07 [1] CRAN (R 3.6.2)
 Matrix       * 1.2-18   2019-11-27 [1] CRAN (R 3.6.3)
 mice         * 3.15.0   2022-11-19 [1] CRAN (R 3.6.3)
 modeldata    * 1.1.0    2023-01-25 [1] CRAN (R 3.6.3)
 naniar       * 1.0.0    2023-02-02 [1] CRAN (R 3.6.3)
 parsnip      * 1.1.0    2023-04-12 [1] CRAN (R 3.6.3)
 patchwork    * 1.1.2    2022-08-19 [1] CRAN (R 3.6.3)
 probably     * 1.0.2    2023-06-29 [1] CRAN (R 3.6.3)
 purrr        * 1.0.1    2023-01-10 [1] CRAN (R 3.6.3)
 R6           * 2.5.1    2021-08-19 [1] CRAN (R 3.6.2)
 ranger       * 0.14.1   2022-06-18 [1] CRAN (R 3.6.3)
 RColorBrewer * 1.1-3    2022-04-03 [1] CRAN (R 3.6.3)
 readr        * 1.4.0    2020-10-05 [1] CRAN (R 3.6.2)
 readxl       * 1.3.1    2019-03-13 [1] CRAN (R 3.6.0)
 recipes      * 1.0.7    2023-08-10 [1] CRAN (R 3.6.3)
 rsample      * 1.1.1    2022-12-07 [1] CRAN (R 3.6.3)
 rusps        * 0.1.0    2022-12-28 [1] Github (hansthompson/rusps@36090a4)
 scales       * 1.2.1    2022-08-20 [1] CRAN (R 3.6.3)
 sf           * 1.0-1    2021-06-29 [1] CRAN (R 3.6.2)
 shapviz      * 0.9.0    2023-06-09 [1] CRAN (R 3.6.3)
 stringr      * 1.5.0    2022-12-02 [1] CRAN (R 3.6.3)
 tibble       * 3.2.1    2023-03-20 [1] CRAN (R 3.6.3)
 tidycensus   * 1.3.1    2023-01-09 [1] CRAN (R 3.6.3)
 tidymodels   * 1.0.0    2022-07-13 [1] CRAN (R 3.6.3)
 tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 3.6.3)
 tidyverse    * 1.3.1    2021-04-15 [1] CRAN (R 3.6.2)
 tmaptools    * 3.1-1    2021-01-19 [1] CRAN (R 3.6.3)
 tune         * 1.1.1    2023-04-11 [1] CRAN (R 3.6.3)
 workflows    * 1.1.3    2023-02-22 [1] CRAN (R 3.6.3)
 workflowsets * 1.0.1    2023-04-06 [1] CRAN (R 3.6.3)
 XML          * 3.99-0.3 2020-01-20 [1] CRAN (R 3.6.0)
 yardstick    * 1.2.0    2023-04-21 [1] CRAN (R 3.6.3)
```
