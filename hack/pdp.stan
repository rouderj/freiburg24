data {
  int N;
  array[2*N] int Y;
}
parameters {
    vector<lower=0,upper=1>[N] R;
    vector<lower=0,upper=1>[N] A;
}
transformed parameters{
    vector[N] R_probit = Phi(R);
    vector[N] A_probit = Phi(A);
    vector[N] p_inc;
    vector[N] p_exc;
    for (n in 1:N){
        p_inc[n] = R[n] + (1 - R[n]) * A[n];
        p_exc[n] = (1 - R[n]) * A[n];
    }
} 
model {
    R_probit ~ std_normal(); 
    A_probit ~ std_normal(); 
    for (n in 1:N){
        Y[(n*2) -1] ~ binomial(50, p_inc[n]);
        Y[(n*2)] ~ binomial(50, p_exc[n]);
    }
}
