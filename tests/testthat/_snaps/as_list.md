# as.list

    Code
      print(out, table_width = Inf)
    Output
       r 
      ---
      Parameter |  carb |  gear |    am |    vs |  qsec |    wt |  drat |    hp |  disp |   cyl
      -----------------------------------------------------------------------------------------
      mpg       | -0.55 |  0.48 |  0.60 |  0.66 |  0.42 | -0.87 |  0.68 | -0.78 | -0.85 | -0.85
      cyl       |  0.53 | -0.49 | -0.52 | -0.81 | -0.59 |  0.78 | -0.70 |  0.83 |  0.90 |      
      disp      |  0.39 | -0.56 | -0.59 | -0.71 | -0.43 |  0.89 | -0.71 |  0.79 |       |      
      hp        |  0.75 | -0.13 | -0.24 | -0.72 | -0.71 |  0.66 | -0.45 |       |       |      
      drat      | -0.09 |  0.70 |  0.71 |  0.44 |  0.09 | -0.71 |       |       |       |      
      wt        |  0.43 | -0.58 | -0.69 | -0.55 | -0.17 |       |       |       |       |      
      qsec      | -0.66 | -0.21 | -0.23 |  0.74 |       |       |       |       |       |      
      vs        | -0.57 |  0.21 |  0.17 |       |       |       |       |       |       |      
      am        |  0.06 |  0.79 |       |       |       |       |       |       |       |      
      gear      |  0.27 |       |       |       |       |       |       |       |       |      
      
      
       n_Obs 
      -------
      Parameter |  carb |  gear |    am |    vs |  qsec |    wt |  drat |    hp |  disp |   cyl
      -----------------------------------------------------------------------------------------
      mpg       | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00
      cyl       | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 |      
      disp      | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 |       |      
      hp        | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 |       |       |      
      drat      | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 |       |       |       |      
      wt        | 32.00 | 32.00 | 32.00 | 32.00 | 32.00 |       |       |       |       |      
      qsec      | 32.00 | 32.00 | 32.00 | 32.00 |       |       |       |       |       |      
      vs        | 32.00 | 32.00 | 32.00 |       |       |       |       |       |       |      
      am        | 32.00 | 32.00 |       |       |       |       |       |       |       |      
      gear      | 32.00 |       |       |       |       |       |       |       |       |      
      
      
       p 
      ---
      Parameter | carb | gear |   am |   vs | qsec |   wt | drat |   hp | disp |  cyl
      -------------------------------------------------------------------------------
      mpg       | 0.02 | 0.10 | 0.01 | 0.00 | 0.22 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00
      cyl       | 0.04 | 0.08 | 0.04 | 0.00 | 0.01 | 0.00 | 0.00 | 0.00 | 0.00 |     
      disp      | 0.30 | 0.02 | 0.01 | 0.00 | 0.20 | 0.00 | 0.00 | 0.00 |      |     
      hp        | 0.00 | 1.00 | 1.00 | 0.00 | 0.00 | 0.00 | 0.17 |      |      |     
      drat      | 1.00 | 0.00 | 0.00 | 0.19 | 1.00 | 0.00 |      |      |      |     
      wt        | 0.20 | 0.01 | 0.00 | 0.02 | 1.00 |      |      |      |      |     
      qsec      | 0.00 | 1.00 | 1.00 | 0.00 |      |      |      |      |      |     
      vs        | 0.02 | 1.00 | 1.00 |      |      |      |      |      |      |     
      am        | 1.00 | 0.00 |      |      |      |      |      |      |      |     
      gear      | 1.00 |      |      |      |      |      |      |      |      |     
      
      

---

    Code
      print(out, table_width = Inf)
    Output
      =======
       carni 
      =======
      
       rho 
      -----
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      carni | sleep_total |  -0.48 |   -0.59 | -1.00 |        0.31 |      0.95
      carni |   sleep_rem |  -0.72 |   -0.26 | -0.95 |        0.46 |          
      carni | sleep_cycle |  -0.56 |   -0.80 | -0.31 |             |          
      carni |       awake |   0.48 |    0.59 |       |             |          
      carni |     brainwt |   0.82 |         |       |             |          
      
      
       n_Obs 
      -------
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      carni | sleep_total |  19.00 |    9.00 | 19.00 |        5.00 |     10.00
      carni |   sleep_rem |  10.00 |    6.00 | 10.00 |        5.00 |          
      carni | sleep_cycle |   5.00 |    4.00 |  5.00 |             |          
      carni |       awake |  19.00 |    9.00 |       |             |          
      carni |     brainwt |   9.00 |         |       |             |          
      
      
       p 
      ---
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      carni | sleep_total |   0.37 |    0.73 |  0.00 |        1.00 |      0.00
      carni |   sleep_rem |   0.20 |    1.00 |  0.00 |        1.00 |          
      carni | sleep_cycle |   1.00 |    1.00 |  1.00 |             |          
      carni |       awake |   0.37 |    0.73 |       |             |          
      carni |     brainwt |   0.09 |         |       |             |          
      
      
      
      =======
       herbi 
      =======
      
       rho 
      -----
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      herbi | sleep_total |  -0.77 |   -0.86 | -1.00 |       -0.44 |      0.92
      herbi |   sleep_rem |  -0.72 |   -0.75 | -0.92 |       -0.48 |          
      herbi | sleep_cycle |   0.74 |    0.74 |  0.44 |             |          
      herbi |       awake |   0.77 |    0.86 |       |             |          
      herbi |     brainwt |   0.98 |         |       |             |          
      
      
       n_Obs 
      -------
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      herbi | sleep_total |  32.00 |   20.00 | 32.00 |       12.00 |     24.00
      herbi |   sleep_rem |  24.00 |   16.00 | 24.00 |       12.00 |          
      herbi | sleep_cycle |  12.00 |   11.00 | 12.00 |             |          
      herbi |       awake |  32.00 |   20.00 |       |             |          
      herbi |     brainwt |  20.00 |         |       |             |          
      
      
       p 
      ---
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      herbi | sleep_total |   0.00 |    0.00 |  0.00 |        0.35 |      0.00
      herbi |   sleep_rem |   0.00 |    0.00 |  0.00 |        0.35 |          
      herbi | sleep_cycle |   0.03 |    0.04 |  0.35 |             |          
      herbi |       awake |   0.00 |    0.00 |       |             |          
      herbi |     brainwt |   0.00 |         |       |             |          
      
      
      
      =========
       insecti 
      =========
      
       rho 
      -----
      Group   |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      --------------------------------------------------------------------------
      insecti | sleep_total |  -0.60 |   -0.60 | -1.00 |        0.50 |     -0.40
      insecti |   sleep_rem |   0.80 |    0.80 |  0.40 |       -1.00 |          
      insecti | sleep_cycle |  -0.50 |   -0.50 | -0.50 |             |          
      insecti |       awake |   0.60 |    0.60 |       |             |          
      insecti |     brainwt |   1.00 |         |       |             |          
      
      
       n_Obs 
      -------
      Group   |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      --------------------------------------------------------------------------
      insecti | sleep_total |   5.00 |    5.00 |  5.00 |        3.00 |      4.00
      insecti |   sleep_rem |   4.00 |    4.00 |  4.00 |        3.00 |          
      insecti | sleep_cycle |   3.00 |    3.00 |  3.00 |             |          
      insecti |       awake |   5.00 |    5.00 |       |             |          
      insecti |     brainwt |   5.00 |         |       |             |          
      
      
       p 
      ---
      Group   |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      --------------------------------------------------------------------------
      insecti | sleep_total |   1.00 |    1.00 |  0.00 |        1.00 |      1.00
      insecti |   sleep_rem |   1.00 |    1.00 |  1.00 |        0.00 |          
      insecti | sleep_cycle |   1.00 |    1.00 |  1.00 |             |          
      insecti |       awake |   1.00 |    1.00 |       |             |          
      insecti |     brainwt |   0.00 |         |       |             |          
      
      
      
      ======
       omni 
      ======
      
       rho 
      -----
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      omni  | sleep_total |  -0.10 |   -0.28 | -1.00 |       -0.24 |      0.14
      omni  |   sleep_rem |  -0.20 |   -0.39 | -0.14 |       -0.46 |          
      omni  | sleep_cycle |   0.80 |    0.92 |  0.24 |             |          
      omni  |       awake |   0.10 |    0.28 |       |             |          
      omni  |     brainwt |   0.91 |         |       |             |          
      
      
       n_Obs 
      -------
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      omni  | sleep_total |  20.00 |   17.00 | 20.00 |       11.00 |     18.00
      omni  |   sleep_rem |  18.00 |   17.00 | 18.00 |       11.00 |          
      omni  | sleep_cycle |  11.00 |   11.00 | 11.00 |             |          
      omni  |       awake |  20.00 |   17.00 |       |             |          
      omni  |     brainwt |  17.00 |         |       |             |          
      
      
       p 
      ---
      Group |   Parameter | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      omni  | sleep_total |   1.00 |    1.00 |  0.00 |        1.00 |      1.00
      omni  |   sleep_rem |   1.00 |    1.00 |  1.00 |        1.00 |          
      omni  | sleep_cycle |   0.04 |    0.00 |  1.00 |             |          
      omni  |       awake |   1.00 |    1.00 |       |             |          
      omni  |     brainwt |   0.00 |         |       |             |          
      
      
      

---

    Code
      print(out, table_width = Inf)
    Output
      ===
       0 
      ===
      
       r 
      ---
      Group | Parameter |   hp
      ------------------------
      0     |       cyl | 0.87
      0     |        wt | 0.83
      
      
       n_Obs 
      -------
      Group | Parameter |    hp
      -------------------------
      0     |       cyl | 19.00
      0     |        wt | 19.00
      
      
       p 
      ---
      Group | Parameter |   hp
      ------------------------
      0     |       cyl | 0.00
      0     |        wt | 0.00
      
      
      
      ===
       1 
      ===
      
       r 
      ---
      Group | Parameter |   hp
      ------------------------
      1     |       cyl | 0.83
      1     |        wt | 0.80
      
      
       n_Obs 
      -------
      Group | Parameter |    hp
      -------------------------
      1     |       cyl | 13.00
      1     |        wt | 13.00
      
      
       p 
      ---
      Group | Parameter |   hp
      ------------------------
      1     |       cyl | 0.00
      1     |        wt | 0.00
      
      
      

