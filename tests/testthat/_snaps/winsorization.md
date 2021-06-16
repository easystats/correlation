# testing Winsorized correlation

    Code
      correlation(ggplot2::msleep, winsorize = 0.2, p_adjust = "none")
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter1  |  Parameter2 |     r |         95% CI |     t | df |         p
      ---------------------------------------------------------------------------
      sleep_total |   sleep_rem |  0.72 | [ 0.57,  0.82] |  7.87 | 59 | < .001***
      sleep_total | sleep_cycle | -0.45 | [-0.69, -0.11] | -2.72 | 30 | 0.011*   
      sleep_total |       awake | -1.00 | [-1.00, -1.00] |  -Inf | 81 | < .001***
      sleep_total |     brainwt | -0.55 | [-0.71, -0.33] | -4.83 | 54 | < .001***
      sleep_total |      bodywt | -0.48 | [-0.63, -0.29] | -4.87 | 81 | < .001***
      sleep_rem   | sleep_cycle | -0.39 | [-0.65, -0.05] | -2.35 | 30 | 0.025*   
      sleep_rem   |       awake | -0.72 | [-0.82, -0.57] | -7.87 | 59 | < .001***
      sleep_rem   |     brainwt | -0.41 | [-0.62, -0.14] | -3.06 | 46 | 0.004**  
      sleep_rem   |      bodywt | -0.37 | [-0.57, -0.14] | -3.10 | 59 | 0.003**  
      sleep_cycle |       awake |  0.45 | [ 0.11,  0.69] |  2.72 | 30 | 0.011*   
      sleep_cycle |     brainwt |  0.90 | [ 0.79,  0.95] | 10.80 | 28 | < .001***
      sleep_cycle |      bodywt |  0.78 | [ 0.59,  0.89] |  6.78 | 30 | < .001***
      awake       |     brainwt |  0.55 | [ 0.33,  0.71] |  4.83 | 54 | < .001***
      awake       |      bodywt |  0.48 | [ 0.29,  0.63] |  4.87 | 81 | < .001***
      brainwt     |      bodywt |  0.91 | [ 0.85,  0.95] | 16.17 | 54 | < .001***
      
      p-value adjustment method: none
      Observations: 30-83

