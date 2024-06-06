# as.data.frame for correlation output

    Code
      as.data.frame(correlation(ggplot2::msleep))
    Output
          Parameter1  Parameter2          r   CI      CI_low     CI_high  method
      1  sleep_total   sleep_rem  0.7517550 0.95  0.61667557  0.84383201 pearson
      2  sleep_total sleep_cycle -0.4737127 0.95 -0.70581894 -0.14975542 pearson
      3  sleep_total       awake -0.9999986 0.95 -0.99999908 -0.99999779 pearson
      4  sleep_total     brainwt -0.3604874 0.95 -0.56942242 -0.10780364 pearson
      5  sleep_total      bodywt -0.3120106 0.95 -0.49442632 -0.10327118 pearson
      6    sleep_rem sleep_cycle -0.3381235 0.95 -0.61438094  0.01198335 pearson
      7    sleep_rem       awake -0.7517713 0.95 -0.84384279 -0.61669876 pearson
      8    sleep_rem     brainwt -0.2213348 0.95 -0.47556189  0.06701441 pearson
      9    sleep_rem      bodywt -0.3276507 0.95 -0.53530394 -0.08264933 pearson
      10 sleep_cycle       awake  0.4737127 0.95  0.14975542  0.70581894 pearson
      11 sleep_cycle     brainwt  0.8516203 0.95  0.70882870  0.92736294 pearson
      12 sleep_cycle      bodywt  0.4178029 0.95  0.08089399  0.66902912 pearson
      13       awake     brainwt  0.3604874 0.95  0.10780364  0.56942242 pearson
      14       awake      bodywt  0.3119801 0.95  0.10323781  0.49440083 pearson
      15     brainwt      bodywt  0.9337822 0.95  0.88916423  0.96081138 pearson
         df_error            t             p
      1        59     8.756396  3.783810e-11
      2        30    -2.946170  4.934837e-02
      3        81 -5328.711772 3.627785e-225
      4        54    -2.839979  4.934837e-02
      5        81    -2.955645  4.085332e-02
      6        30    -1.967883  1.167709e-01
      7        59    -8.756832  3.783810e-11
      8        46    -1.539344  1.305716e-01
      9        59    -2.663776  4.934837e-02
      10       30     2.946170  4.934837e-02
      11       28     8.597296  2.662362e-08
      12       30     2.518773  5.202211e-02
      13       54     2.839979  4.934837e-02
      14       81     2.955326  4.085332e-02
      15       54    19.175704  1.281756e-24

