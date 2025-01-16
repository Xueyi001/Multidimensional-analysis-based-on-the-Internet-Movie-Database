library(readr)
> data <- read.delim("/Users/chenxueyi/Desktop/title_basics_first_10000.tsv",delim="\t")
Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
                      unused argument (delim = "\t")
                    > data <- read_delim("/Users/chenxueyi/Desktop/title_basics_first_10000.tsv",delim="\t")
                    Rows: 10000 Columns: 9                                                        
                    ── Column specification ──────────────────────────────────────────────────────
                    Delimiter: "\t"
                    chr (7): tconst, titleType, primaryTitle, originalTitle, endYear, runtimeM...
                    dbl (2): isAdult, startYear
                    
                    ℹ Use `spec()` to retrieve the full column specification for this data.
                    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
                    Warning message:
                      One or more parsing issues, call `problems()` on your data frame for details,
                    e.g.:
                      dat <- vroom(...)
                    problems(dat) 
                    > library(tidyverse)
                    ── Attaching core tidyverse packages ────────────────────── tidyverse 2.0.0 ──
                    ✔ dplyr     1.1.4     ✔ purrr     1.0.2
                    ✔ forcats   1.0.0     ✔ stringr   1.5.1
                    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
                    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
                    ── Conflicts ──────────────────────────────────────── tidyverse_conflicts() ──
                    ✖ dplyr::filter() masks stats::filter()
                    ✖ dplyr::lag()    masks stats::lag()
                    ℹ Use the conflicted package to force all conflicts to become errors
                    > library(cluster)
                    > library(factoextra)
                    Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
                    > library(corrplot)
                    corrplot 0.95 loaded
                    > library(ggplot2)
                    > library(caret)
                    Loading required package: lattice
                    
                    Attaching package: ‘caret’
                    
                    The following object is masked from ‘package:purrr’:
                      
                      lift
                    
                    > library(randomForest)
                    randomForest 4.7-1.2
                    Type rfNews() to see new features/changes/bug fixes.
                    
                    Attaching package: ‘randomForest’
                    
                    The following object is masked from ‘package:dplyr’:
                      
                      combine
                    
                    The following object is masked from ‘package:ggplot2’:
                      
                      margin
                    
                    > library(glmnet)
                    Loading required package: Matrix
                    
                    Attaching package: ‘Matrix’
                    
                    The following objects are masked from ‘package:tidyr’:
                      
                      expand, pack, unpack
                    
                    Loaded glmnet 4.1-8
                    > cleaned_data <- data[, !(names(data) %in% c("tconst", "originalTitle"))]
                    > cleaned_data[cleaned_data == "\\N"] <- NA
                    > cleaned_data$runtimeMinutes <- ifelse(is.na(cleaned_data$runtimeMinutes), 0, as.numeric(cleaned_data$runtimeMinutes))
                    > cleaned_data$runtimeMinutes <- scale(cleaned_data$runtimeMinutes)
                    > library(tidyr)
                    > cleaned_data <- cleaned_data %>% separate_rows(genres, sep = ",") %>% mutate(genres = trimws(genres))
                    > unique_genres <- unique(cleaned_data$genres)
                    > unique_genres <- unique_genres[!is.na(unique_genres)]
                    > for (genre in unique_genres) {
                      +     cleaned_data[[genre]] <- as.integer(cleaned_data$genres == genre)
                      + }
                    > ibrary(dplyr)
                    Error in ibrary(dplyr) : could not find function "ibrary"
                    > library(dplyr)
                    > cleaned_data <- cleaned_data %>% group_by(primaryTitle, isAdult, startYear, endYear, runtimeMinutes, titleType) %>% summarise(across(all_of(unique_genres), ~ max(.x, na.rm = TRUE, default = 0)), .groups = "drop")
                    > write.csv(cleaned_data, file = "/Users/chenxueyi/Desktop/cleaned_data.csv", row.names = FALSE)
                    > cleaned_data$titleType <- as.factor(cleaned_data$titleType)
                    > titleType_dummies <- model.matrix(~ titleType - 1, data = cleaned_data)
                    > cleaned_data <- cbind(cleaned_data, titleType_dummies)
                    > cleaned_data$titleType <- NULL  
                    > cleaned_data <- cleaned_data[, !(names(cleaned_data) %in% c("endYear"))]
                    > output_dir <- "/Users/chenxueyi/Desktop/plots"
                    > if (!dir.exists(output_dir)) {
                      +     dir.create(output_dir, recursive = TRUE)
                      + }
                    > data_cleaned <- cleaned_data %>% na.omit()
                    > data_cleaned$runtimeMinutes <- as.numeric(data_cleaned$runtimeMinutes)
                    > data_cleaned$startYear <- as.factor(data_cleaned$startYear)
                    > p_start_year_bar <- ggplot(data_cleaned, aes(x = startYear)) + geom_bar(fill = "purple", color = "black", alpha = 0.7) + labs(title = "Bar Plot: Distribution of Start Year", x = "Start Year", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    > ggsave(filename = paste0(output_dir, "/start_year_bar_distribution.png"), plot = p_start_year_bar)
                    Saving 7 x 7 in image
                    > data_cleaned$isAdult <- factor(data_cleaned$isAdult, levels = c(0, 1), labels = c("No", "Yes"))
                    > p_start_year_bar <- ggplot(data_cleaned, aes(x = isAdult)) + geom_bar(fill = "purple", color = "black", alpha = 0.7) + labs(title = "Bar Plot: Distribution of isAdult", x = "isAdult", y = "Count") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    > ggsave(filename = paste0(output_dir, "/isAdult_bar_distribution.png"), plot = p_start_year_bar)
                    Saving 7 x 7 in image
                    > p_runtime <- ggplot(data_cleaned, aes(x = runtimeMinutes)) + geom_histogram(bins = 30, fill = "orange", color = "black", alpha = 0.7) + geom_density(aes(y = after_stat(density) * max(after_stat(count))), color = "red", linewidth = 1) + labs(title = "Distribution of Runtime Minutes", x = "Runtime Minutes", y = "Frequency") + theme_minimal()
                    > ggsave(filename = paste0(output_dir, "/runtime_minutes_distribution.png"), plot = p_runtime)
                    Saving 7 x 7 in image
                    > genre_data <- cleaned_data %>% select(all_of(unique_genres)) %>% summarise(across(everything(), sum, na.rm = TRUE)) %>% pivot_longer(cols = everything(), names_to = "Genre", values_to = "Count") %>% filter(Count > 0)
                    Warning message:
                      There was 1 warning in `summarise()`.
                    ℹ In argument: `across(everything(), sum, na.rm = TRUE)`.
                    Caused by warning:
                      ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
                    Supply arguments directly to `.fns` through an anonymous function instead.
                    
                    # Previously
                    across(a:b, mean, na.rm = TRUE)
                    
                    # Now
                    across(a:b, \(x) mean(x, na.rm = TRUE))
                    This warning is displayed once every 8 hours.
                    Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
                    generated. 
                    > p_genre <- ggplot(genre_data, aes(x = reorder(Genre, -Count), y = Count)) +
                      + geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) + labs(title = "Distribution of Movie Genres", x = "Genre", y = "Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1), minimal = TRUE)
                    > ggsave(filename = paste0(output_dir, "/movie_genres_distribution.png"), plot = p_genre)
                    Saving 7 x 7 in image
                    Warning messages:
                      1: In plot_theme(plot) :
                      The `minimal` theme element is not defined in the element hierarchy.
                    2: In plot_theme(plot) :
                      The `minimal` theme element is not defined in the element hierarchy.
                    > clustering_data <- data_cleaned %>% select(runtimeMinutes, all_of(unique_genres), starts_with("titleType")) %>% as.data.frame()
                    > clustering_data <- na.omit(clustering_data)
                    > set.seed(123) 
                    > wss <- sapply(1:10, function(k) {
                      +     kmeans(clustering_data, centers = k, nstart = 25, iter.max = 300)$tot.withinss
                      + })
                    > p_elbow <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) + geom_line(color = "blue") + geom_point(color = "red") + labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares") + theme_minimal()
                    > ggsave(filename = paste0(output_dir, "/elbow_method.png"), plot = p_elbow)
                    Saving 7 x 7 in image
                    > optimal_k <- 5  
                    > kmeans_result <- kmeans(clustering_data, centers = optimal_k, nstart = 25, iter.max = 300)
                    > data_cleaned$cluster <- as.factor(kmeans_result$cluster)
                    > pca_result <- prcomp(clustering_data, scale. = TRUE)
                    > pca_data <- as.data.frame(pca_result$x[, 1:2])
                    > pca_data$cluster <- data_cleaned$cluster
                    > p_clusters <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) + geom_point(alpha = 0.7, size = 2) + labs(title = "Clustering Results (PCA)", x = "Principal Component 1", y = "Principal Component 2") + theme_minimal()
                    > ggsave(filename = paste0(output_dir, "/clustering_results.png"), plot = p_clusters)
                    Saving 7 x 7 in image
                    > cluster_summary <- data_cleaned %>% group_by(cluster) %>% summarise(across(c(runtimeMinutes, all_of(unique_genres)), mean, na.rm = TRUE))
                    > write.csv(cluster_summary, file = paste0(output_dir, "/cluster_summary.csv"), row.names = FALSE)
                    > p_cluster_runtime <- ggplot(data_cleaned, aes(x = runtimeMinutes, fill = cluster)) + geom_histogram(bins = 30, position = "dodge", alpha = 0.7) + labs(title = "Runtime Minutes Distribution by Cluster", x = "Runtime Minutes", y = "Count") + theme_minimal()
                    > ggsave(filename = paste0(output_dir, "/runtime_minutes_by_cluster.png"), plot = p_cluster_runtime)
                    Saving 7 x 7 in image
                    > write.csv(cleaned_data, file = paste0(output_dir, "/cleaned_data_full.csv"), row.names = FALSE)
                    > write.csv(data_cleaned, file = paste0(output_dir, "/data_cleaned_full.csv"), row.names = FALSE)
                    > print("K-Means Clustering Results:")
                    [1] "K-Means Clustering Results:"
                    > print(kmeans_result)
                    K-means clustering with 5 clusters of sizes 1669, 3555, 2214, 42, 2493
                    
                    Cluster means:
                      runtimeMinutes Documentary     Short    Animation     Comedy    Romance
                    1     -0.5483858 0.081485920 0.0000000 0.0017974835 0.09466747 0.04254044
                    2     -0.3441256 0.064978903 0.9853727 0.0320675105 0.00000000 0.08607595
                    3     -0.3882863 0.001355014 0.9837398 0.1178861789 1.00000000 0.03839205
                    4      9.7756487 0.000000000 0.0000000 0.0000000000 0.00000000 0.04761905
                    5      1.0439324 0.005214601 0.0000000 0.0004011231 0.18852788 0.10589651
                    Sport         News      Drama    Fantasy      Horror   Biography
                    1 0.005392451 0.0000000000 0.47932894 0.01138406 0.003594967 0.004194128
                    2 0.004219409 0.0039381153 0.54767932 0.02137834 0.012939522 0.004500703
                    3 0.001806685 0.0000000000 0.03703704 0.01174345 0.003613369 0.000000000
                    4 0.000000000 0.0000000000 0.26190476 0.00000000 0.000000000 0.000000000
                    5 0.001604493 0.0004011231 0.76213398 0.01243482 0.010429202 0.010429202
                    Music         War       Crime    Western      Family   Adventure
                    1 0.0005991612 0.028759736 0.050329539 0.01917316 0.000000000 0.040742960
                    2 0.0022503516 0.019127989 0.037130802 0.15021097 0.004219409 0.019409283
                    3 0.0004516712 0.002258356 0.003161698 0.03477868 0.006323397 0.001355014
                    4 0.0000000000 0.023809524 0.119047619 0.00000000 0.000000000 0.428571429
                    5 0.0008022463 0.042920176 0.057761733 0.07420778 0.004412355 0.042117930
                    Action    History     Mystery      Sci-Fi      Musical     Thriller
                    1 0.017974835 0.01018574 0.018573996 0.001797484 0.0000000000 0.0047932894
                    2 0.015471167 0.01237693 0.008720113 0.003938115 0.0005625879 0.0095639944
                    3 0.001355014 0.00000000 0.000000000 0.001355014 0.0000000000 0.0004516712
                    4 0.523809524 0.00000000 0.047619048 0.000000000 0.0000000000 0.0714285714
                    5 0.018852788 0.02406739 0.029683113 0.003208985 0.0000000000 0.0104292018
                    titleTypemovie titleTypeshort
                    1      1.0000000    0.000000000
                    2      0.0000000    1.000000000
                    3      0.0000000    1.000000000
                    4      1.0000000    0.000000000
                    5      0.9987966    0.001203369
                    
                    Clustering vector:
                      1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
                    2    5    5    2    5    2    3    2    1    2    1    2    1    2    2 
                    16   17   18   19   20   21   22   23   24   25   26   27   28   29   30 
                    1    1    1    5    1    2    5    5    3    1    3    1    1    2    2 
                    31   32   33   34   35   36   37   38   39   40   41   42   43   44   45 
                    1    5    2    5    2    3    2    3    1    3    3    1    2    3    2 
                    46   47   48   49   50   51   52   53   54   55   56   57   58   59   60 
                    3    3    3    2    3    1    3    1    2    2    3    1    2    3    5 
                    61   62   63   64   65   66   67   68   69   70   71   72   73   74   75 
                    3    5    5    2    2    2    2    1    5    2    3    3    5    3    2 
                    76   77   78   79   80   81   82   83   84   85   86   87   88   89   90 
                    5    3    5    3    2    2    2    1    2    2    2    2    3    3    1 
                    91   92   93   94   95   96   97   98   99  100  101  102  103  104  105 
                    5    2    2    5    5    1    5    2    2    2    2    2    2    1    2 
                    106  107  108  109  110  111  112  113  114  115  116  117  118  119  120 
                    1    2    2    3    5    2    3    2    3    2    2    3    2    1    2 
                    121  122  123  124  125  126  127  128  129  130  131  132  133  134  135 
                    2    3    2    2    3    3    2    3    3    2    2    3    2    1    3 
                    136  137  138  139  140  141  142  143  144  145  146  147  148  149  150 
                    3    2    2    5    1    5    3    3    3    3    1    5    2    1    1 
                    151  152  153  154  155  156  157  158  159  160  161  162  163  164  165 
                    5    2    5    5    5    2    5    2    2    3    2    2    3    2    1 
                    166  167  168  169  170  171  172  173  174  175  176  177  178  179  180 
                    2    3    3    2    3    2    2    3    5    2    3    3    2    3    2 
                    181  182  183  184  185  186  187  188  189  190  191  192  193  194  195 
                    1    5    5    3    2    3    3    2    2    1    2    3    2    5    2 
                    196  197  198  199  200  201  202  203  204  205  206  207  208  209  210 
                    2    3    2    3    3    3    2    2    3    2    1    5    1    2    5 
                    211  212  213  214  215  216  217  218  219  220  221  222  223  224  225 
                    3    3    3    3    2    2    3    5    2    5    3    2    3    2    3 
                    226  227  228  229  230  231  232  233  234  235  236  237  238  239  240 
                    2    2    2    2    2    3    3    2    3    3    5    5    3    3    5 
                    241  242  243  244  245  246  247  248  249  250  251  252  253  254  255 
                    2    5    5    5    5    1    2    5    5    2    3    3    3    5    2 
                    256  257  258  259  260  261  262  263  264  265  266  267  268  269  270 
                    2    3    5    2    3    3    5    2    3    2    3    1    2    3    3 
                    271  272  273  274  275  276  277  278  279  280  281  282  283  284  285 
                    3    3    5    3    3    5    5    3    3    5    3    3    5    3    2 
                    286  287  288  289  290  291  292  293  294  295  296  297  298  299  300 
                    1    2    3    5    5    2    3    1    3    3    2    3    2    5    2 
                    301  302  303  304  305  306  307  308  309  310  311  312  313  314  315 
                    3    2    5    5    2    5    2    2    3    3    5    3    3    2    2 
                    316  317  318  319  320  321  322  323  324  325  326  327  328  329  330 
                    2    3    3    3    2    2    5    2    3    3    5    1    1    3    1 
                    331  332  333  334  335  336  337  338  339  340  341  342  343  344  345 
                    5    3    2    2    1    1    5    1    2    5    2    1    5    3    3 
                    346  347  348  349  350  351  352  353  354  355  356  357  358  359  360 
                    3    2    2    2    2    2    3    3    3    3    2    3    2    3    3 
                    361  362  363  364  365  366  367  368  369  370  371  372  373  374  375 
                    5    5    1    2    2    2    3    3    2    2    3    3    3    5    2 
                    376  377  378  379  380  381  382  383  384  385  386  387  388  389  390 
                    5    3    2    1    1    1    5    2    2    5    2    2    5    2    5 
                    391  392  393  394  395  396  397  398  399  400  401  402  403  404  405 
                    2    1    1    2    2    1    5    5    2    3    3    3    3    1    2 
                    406  407  408  409  410  411  412  413  414  415  416  417  418  419  420 
                    1    2    2    1    2    3    3    3    5    1    3    3    5    3    5 
                    421  422  423  424  425  426  427  428  429  430  431  432  433  434  435 
                    2    3    2    3    3    2    5    3    5    1    2    1    5    2    1 
                    436  437  438  439  440  441  442  443  444  445  446  447  448  449  450 
                    5    5    2    1    5    5    5    2    1    2    5    3    5    5    1 
                    451  452  453  454  455  456  457  458  459  460  461  462  463  464  465 
                    1    2    1    2    3    2    2    2    3    2    3    2    2    3    3 
                    466  467  468  469  470  471  472  473  474  475  476  477  478  479  480 
                    2    1    2    3    5    5    5    3    2    3    3    5    5    5    1 
                    481  482  483  484  485  486  487  488  489  490  491  492  493  494  495 
                    5    2    1    2    2    1    2    3    3    5    5    2    2    3    3 
                    496  497  498  499  500  501  502  503  504  505  506  507  508  509  510 
                    3    2    3    3    2    2    2    5    2    2    3    5    2    2    2 
                    511  512  513  514  515  516  517  518  519  520  521  522  523  524  525 
                    5    1    2    2    2    3    2    3    3    2    3    3    5    2    5 
                    526  527  528  529  530  531  532  533  534  535  536  537  538  539  540 
                    5    5    5    5    2    5    3    3    2    2    2    5    2    2    5 
                    541  542  543  544  545  546  547  548  549  550  551  552  553  554  555 
                    2    5    3    2    3    3    1    3    3    2    5    3    2    3    2 
                    556  557  558  559  560  561  562  563  564  565  566  567  568  569  570 
                    1    3    3    1    3    2    2    5    1    2    2    3    3    3    3 
                    571  572  573  574  575  576  577  578  579  580  581  582  583  584  585 
                    3    2    2    2    3    3    1    2    2    1    1    2    2    2    5 
                    586  587  588  589  590  591  592  593  594  595  596  597  598  599  600 
                    2    5    1    3    3    2    3    2    2    2    2    3    2    2    5 
                    601  602  603  604  605  606  607  608  609  610  611  612  613  614  615 
                    3    3    2    3    2    3    2    3    1    2    5    2    2    2    2 
                    616  617  618  619  620  621  622  623  624  625  626  627  628  629  630 
                    2    2    3    2    2    2    3    2    3    5    2    2    5    5    2 
                    631  632  633  634  635  636  637  638  639  640  641  642  643  644  645 
                    2    1    2    2    3    5    2    2    1    2    5    2    1    5    2 
                    646  647  648  649  650  651  652  653  654  655  656  657  658  659  660 
                    1    2    5    5    5    2    2    1    2    2    5    3    2    3    2 
                    661  662  663  664  665  666  667  668  669  670  671  672  673  674  675 
                    1    5    5    3    2    1    1    5    1    1    2    3    3    2    1 
                    676  677  678  679  680  681  682  683  684  685  686  687  688  689  690 
                    2    2    2    3    1    1    5    5    5    1    2    2    3    5    2 
                    691  692  693  694  695  696  697  698  699  700  701  702  703  704  705 
                    2    5    2    1    1    2    2    5    5    5    2    2    2    3    1 
                    706  707  708  709  710  711  712  713  714  715  716  717  718  719  720 
                    5    1    3    2    2    5    2    5    5    5    2    3    2    5    1 
                    721  722  723  724  725  726  727  728  729  730  731  732  733  734  735 
                    1    1    3    2    5    2    2    5    5    5    2    5    3    5    5 
                    736  737  738  739  740  741  742  743  744  745  746  747  748  749  750 
                    3    3    3    3    3    3    3    3    3    3    3    3    3    3    3 
                    751  752  753  754  755  756  757  758  759  760  761  762  763  764  765 
                    3    2    5    5    3    5    5    3    2    2    3    3    5    3    3 
                    766  767  768  769  770  771  772  773  774  775  776  777  778  779  780 
                    5    3    5    1    5    3    5    5    3    3    3    3    3    5    1 
                    781  782  783  784  785  786  787  788  789  790  791  792  793  794  795 
                    2    2    2    5    1    2    2    5    1    5    3    5    3    3    3 
                    796  797  798  799  800  801  802  803  804  805  806  807  808  809  810 
                    3    3    3    3    3    3    3    2    1    5    5    5    5    5    1 
                    811  812  813  814  815  816  817  818  819  820  821  822  823  824  825 
                    5    1    1    1    2    3    3    1    1    1    1    2    1    2    2 
                    826  827  828  829  830  831  832  833  834  835  836  837  838  839  840 
                    1    3    3    2    5    2    3    3    3    1    3    5    3    3    2 
                    841  842  843  844  845  846  847  848  849  850  851  852  853  854  855 
                    5    5    3    1    2    5    3    2    2    2    2    2    2    3    3 
                    856  857  858  859  860  861  862  863  864  865  866  867  868  869  870 
                    3    2    5    5    5    3    2    3    1    5    2    2    2    2    3 
                    871  872  873  874  875  876  877  878  879  880  881  882  883  884  885 
                    2    2    2    2    2    2    2    2    5    3    5    3    3    1    2 
                    886  887  888  889  890  891  892  893  894  895  896  897  898  899  900 
                    5    2    2    3    2    2    3    3    3    2    2    2    3    2    5 
                    901  902  903  904  905  906  907  908  909  910  911  912  913  914  915 
                    1    3    2    1    1    2    1    1    5    2    2    1    3    2    1 
                    916  917  918  919  920  921  922  923  924  925  926  927  928  929  930 
                    1    5    5    5    5    2    2    1    5    2    3    2    2    3    3 
                    931  932  933  934  935  936  937  938  939  940  941  942  943  944  945 
                    2    1    1    2    2    5    2    5    2    5    5    3    1    2    1 
                    946  947  948  949  950  951  952  953  954  955  956  957  958  959  960 
                    3    1    3    1    1    5    5    2    5    1    5    2    2    5    5 
                    961  962  963  964  965  966  967  968  969  970  971  972  973  974  975 
                    2    2    3    3    2    2    2    2    5    3    2    2    3    2    2 
                    976  977  978  979  980  981  982  983  984  985  986  987  988  989  990 
                    2    2    5    5    3    1    5    2    1    5    2    5    2    2    5 
                    991  992  993  994  995  996  997  998  999 1000 
                    1    2    2    2    5    2    1    2    2    3 
                    [ reached getOption("max.print") -- omitted 8973 entries ]
                    
                    Within cluster sum of squares by cluster:
                      [1] 1107.9645 2942.0144  699.4357 1161.5902 2347.5715
                    (between_SS / total_SS =  67.9 %)
                    
                    Available components:
                      
                      [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
                    [6] "betweenss"    "size"         "iter"         "ifault"      
                    > write.csv(data_cleaned, file = paste0(output_dir, "/data_with_clusters.csv"), row.names = FALSE)
                    > cluster_centers <- kmeans_result$centers
                    > write.csv(cluster_centers, file = paste0(output_dir, "/cluster_centers.csv"), row.names = TRUE)
                    > print("Cluster Centers:")
                    [1] "Cluster Centers:"
                    > print(cluster_centers)
                    runtimeMinutes Documentary     Short    Animation     Comedy    Romance
                    1     -0.5483858 0.081485920 0.0000000 0.0017974835 0.09466747 0.04254044
                    2     -0.3441256 0.064978903 0.9853727 0.0320675105 0.00000000 0.08607595
                    3     -0.3882863 0.001355014 0.9837398 0.1178861789 1.00000000 0.03839205
                    4      9.7756487 0.000000000 0.0000000 0.0000000000 0.00000000 0.04761905
                    5      1.0439324 0.005214601 0.0000000 0.0004011231 0.18852788 0.10589651
                    Sport         News      Drama    Fantasy      Horror   Biography
                    1 0.005392451 0.0000000000 0.47932894 0.01138406 0.003594967 0.004194128
                    2 0.004219409 0.0039381153 0.54767932 0.02137834 0.012939522 0.004500703
                    3 0.001806685 0.0000000000 0.03703704 0.01174345 0.003613369 0.000000000
                    4 0.000000000 0.0000000000 0.26190476 0.00000000 0.000000000 0.000000000
                    5 0.001604493 0.0004011231 0.76213398 0.01243482 0.010429202 0.010429202
                    Music         War       Crime    Western      Family   Adventure
                    1 0.0005991612 0.028759736 0.050329539 0.01917316 0.000000000 0.040742960
                    2 0.0022503516 0.019127989 0.037130802 0.15021097 0.004219409 0.019409283
                    3 0.0004516712 0.002258356 0.003161698 0.03477868 0.006323397 0.001355014
                    4 0.0000000000 0.023809524 0.119047619 0.00000000 0.000000000 0.428571429
                    5 0.0008022463 0.042920176 0.057761733 0.07420778 0.004412355 0.042117930
                    Action    History     Mystery      Sci-Fi      Musical     Thriller
                    1 0.017974835 0.01018574 0.018573996 0.001797484 0.0000000000 0.0047932894
                    2 0.015471167 0.01237693 0.008720113 0.003938115 0.0005625879 0.0095639944
                    3 0.001355014 0.00000000 0.000000000 0.001355014 0.0000000000 0.0004516712
                    4 0.523809524 0.00000000 0.047619048 0.000000000 0.0000000000 0.0714285714
                    5 0.018852788 0.02406739 0.029683113 0.003208985 0.0000000000 0.0104292018
                    titleTypemovie titleTypeshort
                    1      1.0000000    0.000000000
                    2      0.0000000    1.000000000
                    3      0.0000000    1.000000000
                    4      1.0000000    0.000000000
                    5      0.9987966    0.001203369
                    > print("Within-Cluster Sum of Squares (WSS):")
                    [1] "Within-Cluster Sum of Squares (WSS):"
                    > print(kmeans_result$tot.withinss)
                    [1] 8258.576
                    > wss_data <- data.frame(Cluster = 1:optimal_k, WSS = kmeans_result$withinss)
                    > write.csv(wss_data, file = paste0(output_dir, "/cluster_wss.csv"), row.names = FALSE)
                    > write.csv(pca_data, file = paste0(output_dir, "/pca_data_with_clusters.csv"), row.names = FALSE)
                    > list.files(output_dir)
                    [1] "cleaned_data_full.csv"            "cluster_centers.csv"             
                    [3] "cluster_summary.csv"              "cluster_wss.csv"                 
                    [5] "clustering_results.png"           "data_cleaned_full.csv"           
                    [7] "data_with_clusters.csv"           "elbow_method.png"                
                    [9] "isAdult_bar_distribution.png"     "movie_genres_distribution.png"   
                    [11] "pca_data_with_clusters.csv"       "runtime_minutes_by_cluster.png"  
                    [13] "runtime_minutes_distribution.png" "start_year_bar_distribution.png" 
                    > results <- list(
                      +     cleaned_data = cleaned_data,
                      +     data_cleaned = data_cleaned,
                      +     kmeans_result = kmeans_result,
                      +     cluster_centers = cluster_centers,
                      +     cluster_summary = cluster_summary,
                      +     pca_data = pca_data
                      + )
                    > save(results, file = paste0(output_dir, "/analysis_results.RData"))
                    > library(randomForest)
                    > data_rf <- data_cleaned
                    > data_rf$cluster <- as.factor(data_rf$cluster)
                    > X <- data_rf[, !(names(data_rf) %in% c("cluster"))]
                    > y <- data_rf$cluster
                    > set.seed(123)
                    > train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
                    > X_train <- X[train_indices, ]
                    > y_train <- y[train_indices]
                    > X_test <- X[-train_indices, ]
                    > y_test <- y[-train_indices]
                    > rf_model <- randomForest(
                      +     x = X_train,
                      +     y = y_train,
                      +     ntree = 100,         
                      +     mtry = sqrt(ncol(X)), 
                      +     importance = TRUE  
                      + )
                    > print(rf_model)
                    
                    Call:
                      randomForest(x = X_train, y = y_train, ntree = 100, mtry = sqrt(ncol(X)),      importance = TRUE) 
                    Type of random forest: classification
                    Number of trees: 100
                    No. of variables tried at each split: 5
                    
                    OOB estimate of  error rate: 0.15%
                    Confusion matrix:
                      1    2    3  4    5 class.error
                    1 1336    0    0  0    0 0.000000000
                    2    0 2844    0  0    0 0.000000000
                    3    0    0 1772  0    0 0.000000000
                    4    0    0    0 25    9 0.264705882
                    5    0    2    1  0 1992 0.001503759
                    > y_pred <- predict(rf_model, X_test)
                    > conf_matrix <- confusionMatrix(y_pred, y_test)
                    > print(conf_matrix)
                    Confusion Matrix and Statistics
                    
                    Reference
                    Prediction   1   2   3   4   5
                    1 333   0   0   0   0
                    2   0 711   0   0   0
                    3   0   0 442   0   0
                    4   0   0   0   8   0
                    5   0   0   0   0 498
                    
                    Overall Statistics
                    
                    Accuracy : 1          
                    95% CI : (0.9981, 1)
                    No Information Rate : 0.3569     
                    P-Value [Acc > NIR] : < 2.2e-16  
                    
                    Kappa : 1          
                    
                    Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity            1.0000   1.0000   1.0000 1.000000     1.00
Specificity            1.0000   1.0000   1.0000 1.000000     1.00
Pos Pred Value         1.0000   1.0000   1.0000 1.000000     1.00
Neg Pred Value         1.0000   1.0000   1.0000 1.000000     1.00
Prevalence             0.1672   0.3569   0.2219 0.004016     0.25
Detection Rate         0.1672   0.3569   0.2219 0.004016     0.25
Detection Prevalence   0.1672   0.3569   0.2219 0.004016     0.25
Balanced Accuracy      1.0000   1.0000   1.0000 1.000000     1.00
> accuracy <- conf_matrix$overall["Accuracy"]
> print(paste("Accuracy:", accuracy))
[1] "Accuracy: 1"
> importance <- importance(rf_model)
> print(importance)
                        1           2           3          4          5
primaryTitle   -0.3638014  4.03506019  1.68793929  1.6597821 -0.2199442
isAdult         0.0000000  0.00000000  0.00000000  0.0000000  0.0000000
startYear       2.1465099  4.31889996  3.07460482  1.8830402  4.9395557
runtimeMinutes 39.7597033  4.86225208  8.46259129 18.8269095 59.3864702
Documentary    -1.2619261  2.84269403  5.72124612  2.8348900  4.4831316
Short           6.6839968  5.87051284  3.41747842  4.4742038  5.3972540
Animation       2.2341535  3.05153178  1.44764407  1.4073856  3.5944689
Comedy          3.8320186 58.65201137 54.50062934  6.9400994  2.1468547
Romance         2.5010262  1.12599406  3.09332043  0.6455020  1.8236956
Sport          -0.3122310 -0.35311554  0.29066641  0.0000000  0.6275545
News            0.0000000  0.00000000  2.04050616  0.0000000  0.0000000
Drama           1.6594237  3.85246694  4.38094668  2.8183594  2.0951434
Fantasy         0.2186695  0.07938348  2.69554934  0.0000000  0.8438633
Horror          1.9559879  0.91182730  3.65305572  0.0000000  0.6385145
Biography      -1.3885250  1.61654304  1.77413081  1.4139250 -1.1628240
Music          -1.0050378  1.00503782  0.02374687  0.0000000 -1.0050378
War            -1.2362438  1.51253263  5.06322068 -2.2199145 -1.3953693
Crime          -2.3578322  0.56340760  4.08187505 -1.9054088 -1.6239943
Western         5.4769217  3.47668805  4.88735711  3.7482151  0.9515607
Family          3.1443902 -0.02211947  1.15595948  0.0000000  1.6868233
Adventure      -0.8025823  2.91398458  5.66415549  0.8151652 -2.7390727
Action          0.5510010 -1.19132940  3.24957138  3.4785629 -4.4888139
History         1.6229213  1.49238691  3.33930184  1.6437515  1.1586180
Mystery        -0.7420562  0.24049413  3.62013446 -1.7397110  0.3984940
Sci-Fi          1.1544836  0.05213736 -0.89842579  1.0050378  1.6264275
Musical         0.0000000  0.00000000  0.00000000  0.0000000  0.0000000
Thriller        0.8475743  1.91238826  3.04755319 -1.5363822 -1.0563397
titleTypemovie 10.7499086  8.24189152  7.87920870  7.4502272 10.3902672
titleTypeshort 10.9862647  8.91404995  8.16344809  5.3865720  7.1266135
               MeanDecreaseAccuracy MeanDecreaseGini
primaryTitle            2.604257493       33.6797450
isAdult                 0.000000000        0.0000000
startYear               6.655476776      226.3071724
runtimeMinutes         40.534221098     1462.9109992
Documentary             7.408836402       31.2598398
Short                   6.782317258      454.7690996
Animation               4.151634416       19.6219891
Comedy                 59.759241419     1496.5822357
Romance                 4.557313642       11.1647616
Sport                   0.004252817        1.0767992
News                    2.041123843        0.5627092
Drama                   5.572234567      272.2671746
Fantasy                 2.673484694        3.8456770
Horror                  3.854677707        3.4750894
Biography               0.777976728        0.8932907
Music                  -0.380038918        0.3285891
War                     4.413528886        3.5771698
Crime                   2.723891498        6.8847732
Western                 6.007492466       60.1606111
Family                  3.005194835        1.0635836
Adventure               4.919467898        9.0917888
Action                  2.766858599        8.2790225
History                 3.494057799        2.7389242
Mystery                 2.501111443        4.1461616
Sci-Fi                  0.976797809        0.9055059
Musical                 0.000000000        0.1170742
Thriller                2.074443646        3.0770463
titleTypemovie         10.687446957      748.8886895
titleTypeshort          9.879450390      781.1979056
> varImpPlot(rf_model)
> rf_tune <- train(
+     x = X_train,
+     y = y_train,
+     method = "rf",
+     trControl = trainControl(method = "cv", number = 5), 
+     tuneGrid = expand.grid(mtry = c(2, 4, 6)) 
+ )
> print(rf_tune$bestTune)
  mtry
3    6
> y_pred_tuned <- predict(rf_tune, X_test)
> conf_matrix_tuned <- confusionMatrix(y_pred_tuned, y_test)
> print(conf_matrix_tuned)
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1 333   0   0   0   0
         2   0 711   0   0   0
         3   0   0 442   0   0
         4   0   0   0   8   0
         5   0   0   0   0 498

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9981, 1)
    No Information Rate : 0.3569     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
                                     
 Mcnemar's Test P-Value : NA         
                    
                    Statistics by Class:
                      
                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
                    Sensitivity            1.0000   1.0000   1.0000 1.000000     1.00
                    Specificity            1.0000   1.0000   1.0000 1.000000     1.00
                    Pos Pred Value         1.0000   1.0000   1.0000 1.000000     1.00
                    Neg Pred Value         1.0000   1.0000   1.0000 1.000000     1.00
                    Prevalence             0.1672   0.3569   0.2219 0.004016     0.25
                    Detection Rate         0.1672   0.3569   0.2219 0.004016     0.25
                    Detection Prevalence   0.1672   0.3569   0.2219 0.004016     0.25
                    Balanced Accuracy      1.0000   1.0000   1.0000 1.000000     1.00
                    > save(rf_model, file = paste0(output_dir, "/random_forest_model.RData"))
                    > write.csv(conf_matrix$table, file = paste0(output_dir, "/confusion_matrix.csv"))
                    > library(glmnet) 
                    > library(caret)
                    > data_ridge <- data_cleaned
                    > data_ridge$runtimeMinutes <- as.numeric(data_ridge$runtimeMinutes)
                    > X <- as.matrix(data_ridge[, !(names(data_ridge) %in% c("runtimeMinutes"))]) 
                    > y <- data_ridge$runtimeMinutes
                    > set.seed(123) 
                    > train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
                    > X_train <- X[train_indices, ]
                    > y_train <- y[train_indices]
                    > X_test <- X[-train_indices, ]
                    > y_test <- y[-train_indices]
                    > ridge_model <- glmnet(
                      +     x = X_train, 
                      +     y = y_train, 
                      +     alpha = 0,           
                      +     lambda = 10^seq(10, -2, length = 100) 
                      + )
                    Warning message:
                      In storage.mode(xd) <- "double" : NAs introduced by coercion
                    > print(ridge_model)
                    
                    Call:  glmnet(x = X_train, y = y_train, alpha = 0, lambda = 10^seq(10,      -2, length = 100)) 
                    
                    Df  %Dev    Lambda
                    1   27  0.00 1.000e+10
                    2   27  0.00 7.565e+09
                    3   27  0.00 5.722e+09
                    4   27  0.00 4.329e+09
                    5   27  0.00 3.275e+09
                    6   27  0.00 2.477e+09
                    7   27  0.00 1.874e+09
                    8   27  0.00 1.417e+09
                    9   27  0.00 1.072e+09
                    10  27  0.00 8.111e+08
                    11  27  0.00 6.136e+08
                    12  27  0.00 4.642e+08
                    13  27  0.00 3.511e+08
                    14  27  0.00 2.656e+08
                    15  27  0.00 2.009e+08
                    16  27  0.00 1.520e+08
                    17  27  0.00 1.150e+08
                    18  27  0.00 8.697e+07
                    19  27  0.00 6.579e+07
                    20  27  0.00 4.977e+07
                    21  27  0.00 3.765e+07
                    22  27  0.00 2.848e+07
                    23  27  0.00 2.154e+07
                    24  27  0.00 1.630e+07
                    25  27  0.00 1.233e+07
                    26  27  0.00 9.326e+06
                    27  27  0.00 7.055e+06
                    28  27  0.00 5.337e+06
                    29  27  0.00 4.037e+06
                    30  27  0.00 3.054e+06
                    31  27  0.00 2.310e+06
                    32  27  0.00 1.748e+06
                    33  27  0.00 1.322e+06
                    34  27  0.00 1.000e+06
                    35  27  0.00 7.565e+05
                    36  27  0.00 5.722e+05
                    37  27  0.00 4.329e+05
                    38  27  0.00 3.275e+05
                    39  27  0.00 2.477e+05
                    40  27  0.00 1.874e+05
                    41  27  0.00 1.417e+05
                    42  27  0.00 1.072e+05
                    43  27  0.00 8.111e+04
                    44  27  0.00 6.136e+04
                    45  27  0.00 4.642e+04
                    46  27  0.01 3.511e+04
                    47  27  0.01 2.656e+04
                    48  27  0.01 2.009e+04
                    49  27  0.01 1.520e+04
                    50  27  0.02 1.150e+04
                    51  27  0.03 8.697e+03
                    52  27  0.03 6.579e+03
                    53  27  0.05 4.977e+03
                    54  27  0.06 3.765e+03
                    55  27  0.08 2.848e+03
                    56  27  0.10 2.154e+03
                    57  27  0.14 1.630e+03
                    58  27  0.18 1.233e+03
                    59  27  0.24 9.330e+02
                    60  27  0.32 7.060e+02
                    61  27  0.42 5.340e+02
                    62  27  0.55 4.040e+02
                    63  27  0.73 3.050e+02
                    64  27  0.96 2.310e+02
                    65  27  1.26 1.750e+02
                    66  27  1.65 1.320e+02
                    67  27  2.16 1.000e+02
                    68  27  2.81 7.600e+01
                    69  27  3.65 5.700e+01
                    70  27  4.71 4.300e+01
                    71  27  6.03 3.300e+01
                    72  27  7.66 2.500e+01
                    73  27  9.63 1.900e+01
                    74  27 11.95 1.400e+01
                    75  27 14.62 1.100e+01
                    76  27 17.59 8.000e+00
                    77  27 20.81 6.000e+00
                    78  27 24.16 5.000e+00
                    79  27 27.54 4.000e+00
                    80  27 30.86 3.000e+00
                    81  27 34.03 2.000e+00
                    82  27 36.97 2.000e+00
                    83  27 39.63 1.000e+00
                    84  27 41.98 1.000e+00
                    85  27 44.00 1.000e+00
                    86  27 45.66 0.000e+00
                    87  27 46.99 0.000e+00
                    88  27 48.01 0.000e+00
                    89  27 48.77 0.000e+00
                    90  27 49.32 0.000e+00
                    91  27 49.69 0.000e+00
                    92  27 49.95 0.000e+00
                    93  27 50.11 0.000e+00
                    94  27 50.22 0.000e+00
                    95  27 50.29 0.000e+00
                    96  27 50.33 0.000e+00
                    97  27 50.36 0.000e+00
                    98  27 50.38 0.000e+00
                    99  27 50.39 0.000e+00
                    100 27 50.39 0.000e+00
                    > cv_ridge <- cv.glmnet(
                      +     x = X_train, 
                      +     y = y_train, 
                      +     alpha = 0, 
                      +     nfolds = 5 
                      + )
                    There were 11 warnings (use warnings() to see them)
                    > best_lambda <- cv_ridge$lambda.min
                    > print(paste("Best Lambda:", best_lambda))
                    [1] "Best Lambda: 0.0607890878030403"
                    > plot(cv_ridge)
                    > final_ridge_model <- glmnet(
                      +     x = X_train, 
                      +     y = y_train, 
                      +     alpha = 0, 
                      +     lambda = best_lambda
                      + )
                    Warning message:
                      In storage.mode(xd) <- "double" : NAs introduced by coercion
                    > y_pred <- predict(final_ridge_model, newx = X_test)
                    Warning message:
                      In cbind2(1, newx) %*% nbeta : NAs introduced by coercion
                    > mse <- mean((y_test - y_pred)^2)
                    > print(paste("Mean Squared Error:", mse))
                    [1] "Mean Squared Error: 0.690267787614011"
                    > ss_total <- sum((y_test - mean(y_test))^2)
                    > ss_residual <- sum((y_test - y_pred)^2)
                    > r_squared <- 1 - (ss_residual / ss_total)
                    > print(paste("R-squared:", r_squared))
                    [1] "R-squared: 0.449630701812435"
                    > coefficients <- as.matrix(coef(final_ridge_model))
                    > print(coefficients)
                    s0
                    (Intercept)    -19.227466779
                    primaryTitle     0.000000000
                    isAdult          0.000000000
                    startYear        0.009557557
                    Documentary     -0.063330429
                    Short           -0.042228594
                    Animation       -0.207548126
                    Comedy          -0.313436630
                    Romance          0.006133512
                    Sport            0.078370081
                    News             0.099766995
                    Drama           -0.019303380
                    Fantasy         -0.012329865
                    Horror           0.035475926
                    Biography        0.121314698
                    Music           -0.032997820
                    War              0.066674289
                    Crime           -0.008040232
                    Western         -0.062840999
                    Family           0.067762129
                    Adventure        0.210029671
                    Action           1.194851012
                    History          0.271900286
                    Mystery         -0.050343081
                    Sci-Fi           0.454548175
                    Musical         -0.144128279
                    Thriller         0.263293884
                    titleTypemovie   0.133478646
                    titleTypeshort  -0.129035455
                    cluster          0.374235422
                    > coeff_df <- data.frame(
                      +     Feature = rownames(coefficients),
                      +     Coefficient = coefficients[, 1]
                      + )
                    > library(ggplot2)
                    > ggplot(coeff_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Feature Importance", x = "Features", y = "Coefficient")
                    > save(final_ridge_model, file = paste0(output_dir, "/ridge_model.RData"))
                    > prediction_results <- data.frame(
                      +     Actual = y_test,
                      +     Predicted = y_pred
                      + )
                    > write.csv(prediction_results, file = paste0(output_dir, "/ridge_predictions.csv"), row.names = FALSE)