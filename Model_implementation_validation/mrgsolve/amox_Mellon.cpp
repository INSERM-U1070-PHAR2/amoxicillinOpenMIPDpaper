$PARAM @annotated
Fpop : 0.797 : oral bioavailability
Mtt_pop : 1 : mean transit time (h)
Ktr_pop : 1.6 : transit rate (h-1)
Ka_pop : 1.7 : absorption rate constant (h)
beta_weight : -3.1 : weight effect on Ka (log h-1)
Qpop : 4.2 : Intercompartmental clearance (L/h)
Vcpop : 9 : Typical central volume of distribution (L)
Vppop : 6.4 : Peripheral volume of distribution (L)
CLpop : 14.6 : Clearance (L/h)
MIC : 1 : minimal inhibitory concentration (mg/L)

$PARAM @covariates @annotated
BW    : 109.3 : body weight (kg)

$OMEGA @block @annotated
ECL : 0.0729      : Variance of ETA(1) (ECL) - clearance
EVC : 0 0.25 : Variance of (ETA(2)) - central volume
EVP : 0 0 0.09 : Variance of (ETA(3)) - peripheral volume
EQ : 0.143 0 0 0.3481 : Covariance of ETA(1) and ETA(4), and variance of ETA(4) (Q)
EF : 0 0 0 0 1 : eta on F
EMTT : 0 0 0 0 0 0.0625 : eta on MTT
EKTR : 0 0 0 0 0 0 0.2401 : eta on Ktr
EKA : 0 0 0 0 0 0 0 0.0625 : eta on Ka

$SIGMA
0.383 // Proportional error
0.131 // Additive error (mg/L)


$CMT DEPOT CENTRAL PERIPHERIQUE TOVERMIC AUC_calc

$GLOBAL 
int NDOSE = 0;
double dosetime[100];
double dose[100];

$MAIN

F_DEPOT = 0; //necessary for transit compartment 
double logit_F = log(Fpop/(1-Fpop)) + ETA(5);
double F = exp(logit_F)/(1 + exp(logit_F));
double Mtt = Mtt_pop*exp(ETA(6));
double Ktr = Ktr_pop*exp(ETA(7));
double Ka = Ka_pop*pow((BW/109.3),beta_weight)*exp(ETA(8));

if(NEWIND < 2) NDOSE = 0;

if(self.amt > 0 && self.cmt==1) {
NDOSE = NDOSE + 1;
dosetime[NDOSE] = self.time;
dose[NDOSE] = self.amt;
}

double NTR = (Mtt*Ktr) - 1;

int j = 0;
while(NTR<0) {
  if(++j > 100) break;
  simeta();
Mtt = Mtt_pop*exp(ETA(6));
Ktr = Ktr_pop*exp(ETA(7));
NTR = (Mtt*Ktr) - 1;
}
 
double NFAC = exp(lgamma(NTR+1));
double KINPT = F * pow(Ktr,NTR) / NFAC;
double CL = CLpop * exp(ETA(1));
double Vc = Vcpop * exp(ETA(2));

double Vp = Vppop * exp(ETA(3));
double Q = Qpop * exp(ETA(4));
double Cmax = 0;

if(NEWIND <= 1 || EVID==1 || EVID==4) {
  Cmax = 0;   // Track Cmax in $DES
} 

$ODE
double INP = 0;
int i = 0;
while(i <= NDOSE) {
double IPT = 0;
if(SOLVERTIME >= dosetime[i]) {
double delta = SOLVERTIME - dosetime[i];
IPT = dose[i] * pow(delta, NTR) * exp(-Ktr * delta);
}
INP = INP + IPT;
++i;
}
dxdt_DEPOT = Ktr*KINPT * INP - Ka * DEPOT;
dxdt_CENTRAL =  Ka * DEPOT - CL / Vc * CENTRAL - Q / Vc * CENTRAL + Q / Vp * PERIPHERIQUE;
dxdt_PERIPHERIQUE = Q / Vc * CENTRAL - Q / Vp * PERIPHERIQUE;
double CP = CENTRAL/Vc;
double free_CP = 0.8*CP;
if(CP > Cmax) Cmax = CP;
double OVERMIC = 0;
if(free_CP>MIC) OVERMIC = 1;
dxdt_TOVERMIC = OVERMIC;
dxdt_AUC_calc = CENTRAL / Vc;

$TABLE
double IPRED = CENTRAL / Vc;              // Individual prediction (mg/L)
double Y = IPRED * (1 + EPS(1)) + EPS(2); // Prediction with residual error (mg/L)
capture AUC = AUC_calc;                   // Area under the curve (exposure) (mg.h/L)

$CAPTURE 
IPRED Y Vc CL  Q Vp AUC Cmax MIC
