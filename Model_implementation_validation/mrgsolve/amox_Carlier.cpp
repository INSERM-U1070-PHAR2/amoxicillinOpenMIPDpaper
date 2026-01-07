$PARAM @annotated
Q : 15.6 : Intercompartimental clearance (L/h)
Vcpop : 13.7 : Typical central volume of distribution(L)
Vp : 13.7 : Peripheral volume of distribution (L)
CLpop : 10 : Clearance(L/h)
$PARAM @covariates @annotated
CREAT : 1 : sérum creatinine en mg/dL
MIC: 1 : CMI value for PK/PD index
SEX : 0 : male
AGE : 60 : in years
BSA : 1.73 : body surface area in m2

$CMT CENTRAL PERIPHERIQUE TOVERMIC AUC_calc

$MAIN 

double sex_factor;
if (SEX == 0) {
  sex_factor = 1;
} else {
  sex_factor = 0.742;
}

double CRCL = 175 * pow(CREAT, -1.154) * pow(AGE, -0.203) * sex_factor * (BSA / 1.73);

if(NEWIND <=1) {
  int i = 0;

  capture CL = CLpop*(CRCL/102)*exp(ECL);

  while(CL > 120) {
    if(++i > 100) {
      break;
    }
    simeta();
    CL = CLpop*(CRCL/102)*exp(ECL);
  }

  i = 0; 

  capture Vc = Vcpop*exp(EVC);

  while(Vc < 3) {
    if(++i > 100) {
      break;
    }
    simeta();
    Vc = Vcpop*exp(EVC);
  }
}

double Cmax = 0;

if(NEWIND <= 1 || EVID==1 || EVID==4) {
  Cmax = 0;   // Track Cmax in $DES
}

$ODE 
dxdt_CENTRAL = - CL/Vc*CENTRAL - Q/Vc*CENTRAL + Q/Vp*PERIPHERIQUE ;
dxdt_PERIPHERIQUE = Q/Vc*CENTRAL - Q/Vp*PERIPHERIQUE ;
double CP = CENTRAL/Vc;
if(CP > Cmax) Cmax = CP;
double OVERMIC = 0;
if(CP>MIC) OVERMIC = 1;
dxdt_TOVERMIC = OVERMIC;
dxdt_AUC_calc = CENTRAL / Vc;

$OMEGA @name IIV @annotated
ECL : 0.148 : eta on clearance TVCL
EVC : 0.140 : eta on central volume of distribution Vc

$SIGMA
0.0473 // proportional error

$TABLE
double IPRED = CENTRAL/Vc;      // Individual prediction (mg/L)
double Y = IPRED * exp(EPS(1)); // Individual prediction with residual error (mg/L)
capture AUC = AUC_calc;         // Area under the curve (exposure) (mg.h/L)

$CAPTURE IPRED, Y, Vc, CL, Q, Vp, AUC, Cmax, MIC, CRCL