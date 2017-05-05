// Run Christians RNG, if 0, the standard c stuff will be used.
//
#define walck 1
#define sanity 0

// The structure of a photon
//
typedef struct {
  double x;         // Current photon position
  double y;
  double z;
  
  double dx;        // Current photon direction
  double dy;
  double dz;

  double path;      // Integrated length travelled during life time
  
  float e_ortho;    // Polarisation vector orthogonal to track
  float e_paral;    // Polarisation vector parallell to track

  unsigned int nscat;

} photon;


// Results for each wavlength bin
//
typedef struct {
  float lambda;
  double scatdust;
  double absdust;
  double avt;

  unsigned int sum_cs;
  unsigned int sum_abs;

} dustresult;


// typedef *dust = dustpt;

// Parse command line arguments
//
//  -h           print help message
//
//  -a <lambda>  minimum wavelength
//  -b <lambda>  maximum wavelength
//
//  -P           polarisation
//  -r <r>       dust resolution
//  -R <R>       inner radius defined 0 < R < 1
//  -t <t>       dust type (LMC, SMC, MW3, AMC, FOR, MIX)
//  -n <n>       number of photons for each wavelength
//  -p <file>    output file name
//
//  -A           save total scattering angle
//  -N           save total number of scatter
//
void usage(void) {
  printf("Usage: cscattercs -t <model> -n <nphot> -r <int> -a <maxlambda> -b <minlambda> -R <iradius> -P -A -N -p <photon.fits>\n");
  exit(0);
}

#ifdef walck
extern void rnclck_(int*);
extern void rninit_(int*);
extern float rnunif_(float*);
#endif

void randinit(void) {                // Initialize RNG
#ifdef walck
  int iseed;
  rnclck_(&iseed);
  rninit_(&iseed);
#else
  srand(time(NULL));
#endif
}

double randunit(void) {              // Return uniformly distributed value between 0 and 1
  float dumheter;
  
#ifdef walck
  return (double) rnunif_(&dumheter);
#else
  return ((double) (rand() % 100))/99.0;
#endif
}

double rnexp(double beta) {
  double rn;
  rn = 0.0;
  while (rn == 0.0 || rn == 1.0) rn = randunit();
  return -beta * log(rn);
}

void printerror(int status);

double cthenyey_greenstein(double tau);

void transpose(double step,
	       double sintheta,double costheta,
	       double sinphi,  double cosphi,
	       double *dx,double *dy,double *dz);
void transpose_lbe(double theta,double phi,double x1,double y1,double z1,
		   double *x2,double *y2,double *z2);
void findBVindices(int nidx, float *wavel, int *B_i, int *V_i);
