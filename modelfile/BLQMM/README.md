Model files
================
(from new to old version)

5. *BQLMM_normal_v2.txt*: similar to BQLMM_DP_v3.txt but the prior for the REs are normal instead of CDPM; input data is in wide format.

4. *BQLMM_DP_v3.txt*: model file that include random intercept and slope, both REs are modeled using CDPM priors; the prior for alpha is gamma(1, 2); input data is in wide format.

3. *BQLMM_DP_v2.txt*: model file that include only random intercept, which is modeled using CDPM prior. the prior for alpha is gamma(1, 2); input data is in wide format.

2. *BQLMM_DP.txt*: different from v2 and v3 in that the input data is in long format instead of wide format; the prior for alpha is uniform(0.3, 10)

1. *BQLMM_normal.txt*: similar to BQLMM_DP.txt but the prior for random intercept is normal instead of DPM


