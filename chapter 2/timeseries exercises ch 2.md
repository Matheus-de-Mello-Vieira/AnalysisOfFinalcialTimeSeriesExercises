# Chapter 2
## Question 2.1
$R_t = a_t + 0.2 a_{t-1}$
A $MA(1)$ model, then $q=1$
$c_0 = 0$
$\theta_1 = -0.2$

Expected value:
$\hat r_{100} (1) = c_0 - \theta_1 a_{100} = 0.2 * 100 = 20$
$\hat r_{100} (2) = c_0 = 0$

Variance
$V[e_{100}(1)] = \sigma_a^2 = 0.025^2 = 0.000625$
$V[e_{100} (2)] = (1 - \theta_1^2) \sigma_a^2 = (1 - (-0.2)^2) * 0.025^2 = 0.0006$

ACF:
$\rho_1 = \frac{-\theta_1}{1 + \theta_1^2} =  \frac{-(-0.2)}{1 + 0.2^2} = 0.1923$
$\rho_2 = 0$, because $l =2>1=q$

## Question 2.2
$r_t = 0.01 + 0.2 r_{t-2} + a_t$
$a_t \sim N(\mu = 0, \sigma^2 = 0.02)$
An $AR(2)$ model
$\phi_0 = 0.01$
$\phi_1=0$
$\phi_2 = 0.2$
$E(r_t) = \frac{\phi_0}{1 - \sum_{i=1}^p \phi_i} = \frac{0.01}{1 - 0 - 0.2} = 0.0125$

$V(r_t) = \frac{\sigma_a^2}{1 - \sum_{i=1}^p \phi_i^2} = \frac{0.02}{1 - 0.2^2} = 0.20833$

ACF:
$\rho_0 = 1$
$\rho_1 = \frac{\phi_1}{1-\phi_2} = \frac{0}{1 - 0.2} = 0$
$\rho_2 = \phi_1 \rho_{2-1} + \phi_2 \rho_{2-2}= \phi_1\rho_1 + \phi_2 \rho_0 = \phi_2 = 0.2$

Forecast:
$r_{100} = -0.01$
$r_{99} = 0.02$
$\hat r_h(1) = \phi_0 + \sum_{i=1}^p \phi_i r_{h+1-i} = 0.01 + 0 \times r_{100+1 - 1} + 0.2 \times r_{100 + 1 - 2} = 0.01 + 0.2 \times r_{99} = 0.01 + 0.2 \times 0.02 = 0.014$
$r_{100}(2) = \phi_0 + \phi_1 \hat r_{100}(1) + \phi_2 r_{100} = 0.01 + 0 \times \hat r_{100}(1) + 0.2 \times (-0.01) = 0.01 - 0.002 = 0.008$

Forecast variance:
$V[e_{100}(1)]= \sigma^2_a = 0.02$
$V[e_{100}(2)] = (1 + \phi_1^2)\sigma_a^2 =(1 + 0^2) \sigma^2_a = 0.2$
