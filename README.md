README
================
David J. Winkel
1/13/2020

``` r
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(xlsx)
library(readxl)
library(ggpubr)
library(openxlsx)
library(ROCR)
library(pROC)
library(caret)
library(corrplot)           
library(tictoc)
library(janitor)
library(DMwR)
```

This is the README file for the experiment.

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17134)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] DMwR_0.4.1       janitor_1.2.0    tictoc_1.0       corrplot_0.84   
    ##  [5] caret_6.0-84     lattice_0.20-38  pROC_1.15.3      ROCR_1.0-7      
    ##  [9] gplots_3.0.1.1   openxlsx_4.1.0.1 ggpubr_0.2.1     magrittr_1.5    
    ## [13] readxl_1.3.1     xlsx_0.6.1       forcats_0.4.0    stringr_1.4.0   
    ## [17] dplyr_0.8.3      purrr_0.3.2      readr_1.3.1      tidyr_0.8.3     
    ## [21] tibble_2.1.3     tidyverse_1.2.1  ggplot2_3.2.0   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-139       bitops_1.0-6       xts_0.11-2        
    ##  [4] lubridate_1.7.4    httr_1.4.0         tools_3.6.0       
    ##  [7] backports_1.1.4    R6_2.4.0           rpart_4.1-15      
    ## [10] KernSmooth_2.23-15 lazyeval_0.2.2     colorspace_1.4-1  
    ## [13] nnet_7.3-12        withr_2.1.2        tidyselect_0.2.5  
    ## [16] curl_3.3           compiler_3.6.0     cli_1.1.0         
    ## [19] rvest_0.3.4        xml2_1.2.0         caTools_1.17.1.2  
    ## [22] scales_1.0.0       digest_0.6.20      rmarkdown_1.14    
    ## [25] pkgconfig_2.0.2    htmltools_0.3.6    TTR_0.23-6        
    ## [28] rlang_0.4.0        quantmod_0.4-15    rstudioapi_0.10   
    ## [31] generics_0.0.2     zoo_1.8-6          jsonlite_1.6      
    ## [34] gtools_3.8.1       ModelMetrics_1.2.2 zip_2.0.3         
    ## [37] Matrix_1.2-17      Rcpp_1.0.1         munsell_0.5.0     
    ## [40] abind_1.4-5        stringi_1.4.3      yaml_2.2.0        
    ## [43] MASS_7.3-51.4      plyr_1.8.4         recipes_0.1.6     
    ## [46] gdata_2.18.0       crayon_1.3.4       haven_2.1.1       
    ## [49] splines_3.6.0      xlsxjars_0.6.1     hms_0.5.0         
    ## [52] zeallot_0.1.0      knitr_1.23         pillar_1.4.2      
    ## [55] ggsignif_0.5.0     reshape2_1.4.3     codetools_0.2-16  
    ## [58] stats4_3.6.0       glue_1.3.1         evaluate_0.14     
    ## [61] data.table_1.12.2  modelr_0.1.4       vctrs_0.2.0       
    ## [64] foreach_1.4.7      cellranger_1.1.0   gtable_0.3.0      
    ## [67] assertthat_0.2.1   xfun_0.8           gower_0.2.1       
    ## [70] prodlim_2018.04.18 broom_0.5.2        class_7.3-15      
    ## [73] survival_2.44-1.1  timeDate_3043.102  rJava_0.9-11      
    ## [76] iterators_1.0.12   lava_1.6.5         ipred_0.9-9
