$PARAM @annotated
Q             : 20.1 : Typical intercompartimental clearance (L/h)
Vcpop         : 9.73 : Typical central volume of distribution (L)
Vp            : 17.6 : Typical peripheral volume of distribution (L)
CLpop         : 13.6 : Typical clearance (L/h)
theta_CRCL_CL : 0.0057 : Covariate effect of creatinine clearance on amoxicillin clearance
fup           : 0.82 : Plasma unbound fraction

$PARAM @covariates @annotated
CREAT : 1 : sérum creatinine en mg/dL
MIC: 1 : CMI value for PK/PD index
SEX : 0 : male
AGE : 60 : in years
BSA : 1.73 : body surface area in m2
WT : 72 : body weight in kg
  
$OMEGA @ name IIV @annotated
ECL : 0.130 : eta on clearance

$SIGMA
0.344 // Proportional error
0.59 // Additive error
  
$CMT CENTRAL PERIPHERIQUE TOVERMIC AUC_calc

$MAIN 

double sex_factor;
if (SEX == 0) {
  sex_factor = 1;
} else {
  sex_factor = 0.85;
}

double CRCL = ((140 - AGE) * WT / (72 * CREAT)) * sex_factor;

if(NEWIND <=1) {

  capture CL = (CLpop*(1 + (theta_CRCL_CL*(CRCL-110)))) *exp(ECL);

  int i = 0;

    while(CL > 120) {
    if(++i > 100) {
      break;
    }
    simeta();
    CL = (CLpop*(1 + (theta_CRCL_CL*(CRCL-110)))) *exp(ECL);
  }
}

double Vc = Vcpop*(WT/70);
double Cmax = 0;

if(NEWIND <= 1 || EVID==1 || EVID==4) {
  Cmax = 0;   // Track Cmax in $DES
}

$ODE 
dxdt_CENTRAL = - CL/Vc*CENTRAL - Q/Vc*CENTRAL + Q/Vp*PERIPHERIQUE ;
dxdt_PERIPHERIQUE = Q/Vc*CENTRAL - Q/Vp*PERIPHERIQUE ;
double CP = CENTRAL/Vc;
double free_CP = fup*CP;
if(CP > Cmax) Cmax = CP;
double OVERMIC = 0;
if(free_CP>MIC) OVERMIC = 1;
dxdt_TOVERMIC = OVERMIC;
dxdt_AUC_calc = CENTRAL / Vc;

$TABLE
double IPRED = CENTRAL/Vc;                // Individual prediction (mg/L)
double Y = IPRED * (1 + EPS(1)) + EPS(2); // Individual prediction with residual error (mg/L)
capture AUC = AUC_calc;                   // Area under the curve (exposure) (mg.h/L)

$CAPTURE IPRED, Y, Vc, CL, Q, Vp, AUC, Cmax, MIC