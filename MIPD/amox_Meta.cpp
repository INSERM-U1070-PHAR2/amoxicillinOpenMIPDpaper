$PARAM @annotated
CLpop : 15.29 : Clearance(L/h)
Vcpop : 9.47 : Typical central volume of distribution(L)
Qpop : 11.96 : Intercompartimental clearance (L/h)
Vppop : 9.46 : Peripheral volume of distribution (L)
CL_CREAT : -0.53 : effect of creatinine on clearance
CL_WT : 0.0029 : effect of body weight on clearance

$PARAM @covariates @annotated
MIC: 1 : CMI value for PK/PD index
CREAT :0.77 : creatinine clearance
WT : 79.94 : body weight

$CMT CENTRAL PERIPHERIQUE1 TOVERMIC AUC_calc

$OMEGA @ name IIV @annotated
ETA1 : 0.47 : ETA1 variance - clearance
ETA2 : 0.49 : ETA2 variance VC
ETA3 : 2.14 : ETA3 variance VP
ETA4 : 1.1 : Variance of ETA4 on intercompartimental clearance

$MAIN 
double CL = CLpop*exp(ETA1) * pow(CREAT/0.77, CL_CREAT) * pow(WT/79.94, CL_WT);
double Vc = Vcpop*exp(ETA2);
double Vp = Vppop*exp(ETA3);
double Q = Qpop*exp(ETA4);

double Cmax = 0;

if(NEWIND <= 1 || EVID==1 || EVID==4) {
  Cmax = 0;   // Track Cmax in $DES
}

$ODE 
dxdt_CENTRAL = - CL/Vc*CENTRAL - Q/Vc*CENTRAL + Q/Vp*PERIPHERIQUE1;
dxdt_PERIPHERIQUE1 = Q/Vc*CENTRAL - Q/Vp*PERIPHERIQUE1 ;
double CP = CENTRAL/Vc;
if(CP > Cmax) Cmax = CP;
double OVERMIC = 0;
if(CP>MIC) OVERMIC = 1;
dxdt_TOVERMIC = OVERMIC;
dxdt_AUC_calc = CENTRAL / Vc;

$SIGMA
0.00025   // Log-normal proportional error

$TABLE
double IPRED = CENTRAL/Vc;      // Individual prediction (mg/L)
double Y = IPRED + IPRED * EPS(1); // Individual prediction with residual error (mg/L)
capture AUC = AUC_calc;         // Area under the curve (exposure) (mg.h/L)

$CAPTURE IPRED, Y, Vc, CL, AUC, Cmax