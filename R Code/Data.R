# Save as Data.R

disc_df <- read_csv("data/AdjAmt_MCQ.csv") |> select(-1)

# Group Level Data Frame
disc_grp_df <- disc_df |>
  group_by(provider,procedure,amt,iv) |>
  mutate(iv = ifelse(procedure=="mcq", log(iv),iv)) |>
  summarise(mean_sv=mean(value), med_sv=median(value)) |>
  arrange(procedure, provider) 
# Individual Level Data Frame
behav <- group_by(filter(disc_df, procedure == "aa" & iv != 730), id,procedure,amt) |> 
  mutate(
    # Calculate Area under the Curve for each participant in the Adj-Amt procedure
    delay_pr=iv/180,
    atheoretical=auc(delay_pr,value),
    # Calculate log k based on simple hyperbolic model for each participant in the Adj-Amt procedure
    theoretical=coef(nlsLM(log(value) ~ -log(1 + exp(k)*iv),
                           start=list(k=-4), control=nls.lm.control(maxiter = 1024)))[[1]]) |>
  group_by(id, amt) |> slice(n()) |> 
  select(-c(iv,delay_pr,value)) |>
  # Combine the Adj-Amt data with the MCQ data
  rbind(
    group_by(filter(disc_df, procedure == "mcq"), id,amt) |>
      
      mutate(
        # Calculate delayed-choice proportions for each participant in the MCQ
        atheoretical = sum(value), 
        # Calculate log k for each participant in the MCQ
        n = ifelse(iv == 0.00016, sum((value == 0 & iv <= 0.00016) | (value == 1 & iv>=0.00016)),  
                   ifelse(iv == 0.0004, sum((value == 0 & iv <= 0.0004) | (value == 1 & iv>=0.0004)), 
                          ifelse(iv == 0.001, sum((value == 0 & iv <= 0.001) | (value == 1 & iv>=0.001)), 
                                 ifelse(iv == 0.0025, sum((value == 0 & iv <= 0.0025) | (value == 1 & iv>=0.0025)), 
                                        ifelse(iv == 0.006, sum((value == 0 & iv <= 0.006) | (value == 1 & iv>=0.006)), 
                                               ifelse(iv == 0.016, sum((value == 0 & iv <= 0.016) | (value == 1 & iv>=0.016)), 
                                                      ifelse(iv == 0.04100, sum((value == 0 & iv <= 0.04100) | (value == 1 & iv>=0.04100)), 
                                                             ifelse(iv == 0.10000, sum((value == 0 & iv <= 0.10000) | (value == 1 & iv>=0.10000)), 
                                                                    (sum((value == 0 & iv <= 0.25000) | (value == 1 & iv>=0.25000)))))))))))
      ) |>
      slice_max(n) |> mutate(theoretical = log(geometric.mean(iv))) |> select(-c(n,iv,value)) |>  distinct(.keep_all = TRUE)
  )

