# as.data.frame for correlation output

    Code
      as.data.frame(correlation(ggplot2::msleep))
    Output
          Parameter1  Parameter2          r   CI      CI_low     CI_high            t
      1  sleep_total   sleep_rem  0.7517550 0.95  0.61667557  0.84383201     8.756396
      2  sleep_total sleep_cycle -0.4737127 0.95 -0.70581894 -0.14975542    -2.946170
      3  sleep_total       awake -0.9999986 0.95 -0.99999908 -0.99999779 -5328.711772
      4  sleep_total     brainwt -0.3604874 0.95 -0.56942242 -0.10780364    -2.839979
      5  sleep_total      bodywt -0.3120106 0.95 -0.49442632 -0.10327118    -2.955645
      6    sleep_rem sleep_cycle -0.3381235 0.95 -0.61438094  0.01198335    -1.967883
      7    sleep_rem       awake -0.7517713 0.95 -0.84384279 -0.61669876    -8.756832
      8    sleep_rem     brainwt -0.2213348 0.95 -0.47556189  0.06701441    -1.539344
      9    sleep_rem      bodywt -0.3276507 0.95 -0.53530394 -0.08264933    -2.663776
      10 sleep_cycle       awake  0.4737127 0.95  0.14975542  0.70581894     2.946170
      11 sleep_cycle     brainwt  0.8516203 0.95  0.70882870  0.92736294     8.597296
      12 sleep_cycle      bodywt  0.4178029 0.95  0.08089399  0.66902912     2.518773
      13       awake     brainwt  0.3604874 0.95  0.10780364  0.56942242     2.839979
      14       awake      bodywt  0.3119801 0.95  0.10323781  0.49440083     2.955326
      15     brainwt      bodywt  0.9337822 0.95  0.88916423  0.96081138    19.175704
         df_error             p              Method n_Obs
      1        59  3.783810e-11 Pearson correlation    61
      2        30  4.934837e-02 Pearson correlation    32
      3        81 3.627785e-225 Pearson correlation    83
      4        54  4.934837e-02 Pearson correlation    56
      5        81  4.085332e-02 Pearson correlation    83
      6        30  1.167709e-01 Pearson correlation    32
      7        59  3.783810e-11 Pearson correlation    61
      8        46  1.305716e-01 Pearson correlation    48
      9        59  4.934837e-02 Pearson correlation    61
      10       30  4.934837e-02 Pearson correlation    32
      11       28  2.662362e-08 Pearson correlation    30
      12       30  5.202211e-02 Pearson correlation    32
      13       54  4.934837e-02 Pearson correlation    56
      14       81  4.085332e-02 Pearson correlation    83
      15       54  1.281756e-24 Pearson correlation    56

