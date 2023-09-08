simu_data_NegB <- function(I, K, S, r, alpha_0, alpha_1, gamma_0, gamma_1, X_t, sigma2, chi2){
        
    J = length(gamma_0)   # number of treatments 
    T = length(X_t)   # number of observation date

    trials = paste("trial", c(1 : I))
    treatments = paste("treatment", c(1 : J))
    times = paste("time", c(1 : T))
    blocks = paste("block", c(1 : K))
    beets = paste("beet", c(1 : S))

    data_simu = expand.grid(trial = trials, treatment = treatments, time = times, block = blocks, beet = beets) %>%
      mutate(trial_treatment = paste(trial, treatment))

    names(gamma_0) = treatments
    names(gamma_1) = treatments

    names(X_t) = times

    beta = rnorm(I, sd = sqrt(sigma2));   names(beta) = trials
    u_ij = rnorm(I * J, sd = sqrt(chi2));   names(u_ij) = unique(data_simu$trial_treatment)


    data_simu = data_simu %>%
        mutate(r = r) %>%
        mutate(N = S) %>%
        mutate(alpha_0 = alpha_0) %>%
        mutate(alpha_1 = alpha_1) %>%
        mutate(gamma_0 = recode(treatment, !!!gamma_0),
               gamma_1 = recode(treatment, !!!gamma_1),
               X_t = recode(time, !!!X_t),
               beta = recode(trial, !!!beta),
               u_ij = recode(trial_treatment, !!!u_ij)) %>%

        mutate(X_t = scale(X_t)) %>%

        mutate(lambda = exp(alpha_0 + beta + gamma_0 + (alpha_1 + gamma_1) * X_t + u_ij)) %>%

        mutate(W = rnbinom(n = I * J * K * T * S, 
                           mu = lambda,
                           size = r)) %>%

        group_by(trial, time, treatment, block, X_t, N) %>%
        summarise(Y = sum(W), Z = sum(W > 0), .groups = "drop") %>%

        mutate(trial_treatment = paste(trial, treatment)) %>%

        mutate(ligne = row_number())

    
    return(data_simu)
}