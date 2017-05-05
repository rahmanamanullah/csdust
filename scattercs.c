#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fitsio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "scattercs.h"
#include "draine.h"
#include "versions.h"

int main(int argc, char *argv[]) {
  unsigned int dustresol = 1;

  // Due to the chosen fits-file format where each field is called xY.ZZZZ where
  // Y.ZZZZ is the wavelength in mu we can currently not handle wavelengths that
  // cannot be written in this form, i.e. 0.0001 <= lambda <= 9.9999 mu.
  //
  // float minlambda = 0.24, maxlambda = 1.6;
  float minlambda = 0.17, maxlambda = 2.5; // Range for SN2011fe template

  // Set to 0 to ignore absorption (for debugging)
  // const int absorption = 1;

  // Effective wavelength (in microns)
  const float B_eff = 0.4405, V_eff = 0.547;

  const double pi=3.14159265;
  const double flux2mag = 1.085736; // 2.5/log(10.)
  const double radius=1000.0, cosmax=1.0, cosmin=-1.0;
  const int nmax=50000;             // Maximum number of interactions per photon
  int nphot=1000;

  int i, k, jj, iphot, istep, ncols = 1;
  FILE *ccstest, *cistest, *scstest;

  // Result arrays (we might want to replace these with a struct?)
  float *fractime = NULL, *totangle = NULL, *firstangle = NULL;
  unsigned short *nscatter = NULL, savescatter = 0, saveangles = 0, savefirstangle = 0;
  double *scatdust, *absdust, *avt, *avg_pol_i;
  unsigned int *nhit, *sum_cs, *sum_abs;
  double ebv_cs, ebv_is, ebv_cs_err, ebv_is_err;
  double *abs_x_is, *abs_x_cs;

  // Unused variables
  //
  // const int NWPAW=5000000, intup=12;
  // const double tmax=5000.0;
  // float vtup[intup];
  // double geodis;
  // double xcross, ycross, zcross;
    
  char *dusttype = NULL;

  char *photfname = NULL;
  char *overwrite = "!";
  char *finalphotfname = NULL;
  char card[FLEN_CARD], newcard[FLEN_CARD];
  fitsfile *photfptr;
  int status = 0, hdutype, keytype;
  char hduname[] = "PHOTONS";
  char **ttype = NULL, **tform = NULL, **tunit = NULL;

  double ebv = 0.2;
  double iradius = 0.0;

  // Photon properties
  double x, y, z, dx, dy, dz, odx, ody, odz;
  double e_ortho, e_paral, path, s, q, d, paththroughdust;
  
  double gamma, theta, phi;
  double sintheta, costheta, tottheta = 0.0, sinphi, cosphi;
  
  double tau_dust;
  double step, sdust, smin, astop;
  double rnow, rnext, rnorm, impact;

  double al_B,al_V,a_B,a_V;
  double factor, scatconst;

  unsigned int dopaw = 0, nextphoton = 0, absphoton = 0;

  int B_i, V_i;

  scatext *draine;
  unsigned int nwavel;
  
  int dopolar = 0;
  polarisation *polar;
  float pvalue;

  // Variable and structures needed for sanity checks and debugging
  //
#ifdef sanity
  double foo;

  // Define a point structure that will allow us to 
  // follow the path of each photon.
  //
  const short int scattering = 1; // photon scatter
  const short int absorption = 2; // photon is absorbed
  const short int adios = 3;      // photon leaves the shell

  // If debug =
  //
  //    10  : print the trace for the first photon that is absorbed without
  //          scattering
  //
  unsigned int debug = 0;

  typedef struct {
    float x, y, z;          // position
    float dx, dy, dz;       // displacement vector
    short int type;         // interaction type
    float paththroughdust,path,sdust;
    float theta,phi;
    float q,gamma;
  } point;
  typedef point *pointpointer;
  
  typedef struct {
    point *history[nmax];
    float astop;            // absorption lengths
    float dustpath,totpath; // travelled path in dust and total path    
    unsigned int iterations;
  } photonstruct;

  // Print the photon history, useful for debugging
  //
  void printphotonhistory(unsigned int iphot, photonstruct *photon) {
    float foo, bar;
    float x, y, z, dx, dy, dz;
    fprintf(stderr,"PHOTON: %d (astop = %.2f)\n",iphot,photon->astop);
    for (i=0; i<photon->iterations+1; i++) {
      x = photon->history[i]->x;
      y = photon->history[i]->y;
      z = photon->history[i]->z;
      dx = photon->history[i]->dx;
      dy = photon->history[i]->dy;
      dz = photon->history[i]->dz;
      foo = sqrt(x*x + y*y + z*z);
      bar = sqrt((dx+x)*(dx+x) + (dy+y)*(dy+y) + (dz+z)*(dz+z));
      fprintf(stderr,"%5d (%5.2f,%5.2f,%5.2f) + (%5.2f,%5.2f,%5.2f) = (%5.2f,%5.2f,%5.2f), rnow/rnext=%05.2f/%05.2f, gamma=%03.0f, q=%05.2f path/dust=%.2f/%.2f sdust=%.2f %d\n",
	      i,x,y,z,dx,dy,dz,dx+x,dy+y,dz+z,foo,bar,(int) 180.*photon->history[i]->gamma/pi,
	      photon->history[i]->q,
	      photon->history[i]->path, photon->history[i]->paththroughdust,
	      photon->history[i]->sdust, photon->history[i]->type);
    }
  }

  photonstruct photon;
  for (jj=0; jj<nmax; jj++)
    photon.history[jj] = (point*)malloc(sizeof(point));
#endif

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
  //  -F           save first scattering angle
  //  -N           save total number of scatter
  //
  while ((i = getopt (argc, argv, "Phr:R:t:n:p:e:a:b:ANF")) != -1) {
    switch (i) {
    case 'a':
      minlambda = atof(optarg);
      break;
    case 'b':
      maxlambda = atof(optarg);
      break;
    case 'r':
      dustresol = atoi(optarg);
      break;
    case 'R':
      iradius = atof(optarg);
      break;
    case 'n':
      nphot = atoi(optarg);
      break;
    case 't':
      dusttype = optarg;
      break;
    case 'e':
      ebv = atof(optarg);
      break;
    case 'p':
      photfname = optarg;
      break;
    case 'P':
      dopolar = 1;
      break;      
    case 'A':
      saveangles = 1;
      break;
    case 'N':
      savescatter = 1;
      break;
    case 'F':
      savefirstangle = 1;
      break;
    case 'h':
      usage();
      break;
      /*
	case '?':
	if (isprint(optopt))
	fprintf(stderr, "Unknown option `-%c'.\n", optopt);
	else
	fprintf(stderr,
	"Unknown option character `\\x%x'.\n",
	optopt);
      */
      return 1;
    default:
      abort ();
    }
  }

  // See comment where minlambda and maxlambda are declared!
  //
  if (minlambda < 0.0001 || minlambda > 9.9999) {
    fprintf(stderr,"Warning: wavelengths < 0.0001 or > 9.9999 mu not supported!\n");
    minlambda = 0.0001;
  }
  if (maxlambda > 9.9999 || maxlambda < 0.0001) {
    fprintf(stderr,"Warning: wavelengths < 0.0001 or > 9.9999 mu not supported!\n");
    maxlambda = 9.9999;
  }
  
  // Make sure we specified a dust model
  //
  if (dusttype == NULL) {
    fprintf(stderr,"Error: no dust model file specified!\n");
    usage();
    return 1;
  }

  // Make sure that iradius is between 0 and 1
  //
  if (iradius < 0.0 || iradius > 1.0) {
    fprintf(stderr,"Error: -R must be given a value between 0.0 and 1.0!\n");
    usage();
    return 1;
  } else {
    iradius *= radius;
  }

  // Initialize the scattering, extinction, albedo table
  //
  draine = NULL;
  draine = init_draine_extscat(dusttype,minlambda,maxlambda,dustresol,&nwavel);
  if (draine == NULL) {
    fprintf(stderr,"Error: failed to load Draine's extinction/scattering table!\n");
    exit(1);
  }

  // Initialize polarisation table
  //
  polar = NULL;
  if (dopolar) {
    polar = init_draine_pol(dusttype);
    if (polar == NULL) {
      fprintf(stderr,"Error: failed to load the polarisation table!\n");
      exit(1);
    }
  }

  // Find which indices correspond to B and V filters
  //
  find_draine_lambda_idx(draine,nwavel,B_eff,&B_i);
  find_draine_lambda_idx(draine,nwavel,V_eff,&V_i);


  // Things to keep track of for each wavelength
  //
  scatdust   = (double*) malloc(sizeof(double) * nwavel);
  absdust    = (double*) malloc(sizeof(double) * nwavel);
  avt        = (double*) malloc(sizeof(double) * nwavel);
  avg_pol_i  = (double*) malloc(sizeof(double) * nwavel);
  abs_x_cs   = (double*) malloc(sizeof(double) * nwavel);
  abs_x_is   = (double*) malloc(sizeof(double) * nwavel);
  sum_cs     = (unsigned int*) malloc(sizeof(unsigned int) * nwavel);
  sum_abs    = (unsigned int*) malloc(sizeof(unsigned int) * nwavel);
  nhit       = (unsigned int*) malloc(sizeof(unsigned int) * nwavel);
  fractime   = (float*) malloc(sizeof(float) * nphot);
  if (saveangles)
    totangle   = (float*) malloc(sizeof(float) * nphot);
  if (savescatter)
    nscatter   = (unsigned short*) malloc(sizeof(unsigned short) * nphot);
  if (savefirstangle)
    firstangle = (float*) malloc(sizeof(float)* nphot);

  // Number of columns to be saved for each wavelength
  //
  ncols = 1;
  if (saveangles)  ncols++;
  if (savescatter) ncols++;
  if (savefirstangle) ncols++;

  // Maximum number of fields for fits format must not be
  // exceeded
  //
  if (ncols*nwavel > 999) {
    fprintf(stderr,"Error: number of columns (%d) exceeds allowed for fits!\n",ncols*nwavel);
    photfname = NULL;
  }

  if (photfname != NULL) {
          
    ttype = (char**) malloc(sizeof(char*)*ncols*nwavel);
    tunit = (char**) malloc(sizeof(char*)*ncols*nwavel);
    tform = (char**) malloc(sizeof(char*)*ncols*nwavel);

    // Columns to be written
    //
    for (jj=0; jj<nwavel; jj++) {
      k = 0;

      ttype[ncols*jj+k] = (char*) malloc(sizeof(char)*8);
      sprintf(ttype[ncols*jj+k],"p%6.4f",draine[jj].lambda);
      tform[ncols*jj+k] = "E";
      tunit[ncols*jj+k] = "path/radius";
      k++;
      
      if (saveangles) {
	ttype[ncols*jj+k] = (char*) malloc(sizeof(char)*8);
	sprintf(ttype[ncols*jj+k],"A%6.4f",draine[jj].lambda);
	tform[ncols*jj+k] = "E";
	tunit[ncols*jj+k] = "radians";
	k++;
      }

      if (savescatter) {
	ttype[ncols*jj+k] = (char*) malloc(sizeof(char)*8);
	sprintf(ttype[ncols*jj+k],"N%6.4f",draine[jj].lambda);
	tform[ncols*jj+k] = "I";
	tunit[ncols*jj+k] = "number";
	k++;
      }

      if (savefirstangle) {
	ttype[ncols*jj+k] = (char*) malloc(sizeof(char)*8);
	sprintf(ttype[ncols*jj+k],"F%6.4f",draine[jj].lambda);
	tform[ncols*jj+k] = "E";
	tunit[ncols*jj+k] = "radians";
	k++;
      }
    }
  }

  printf("Will use %d wavelengths between %.2f and %.2f microns\n",
	 nwavel,minlambda,maxlambda);
    
  if (dopaw) {
  /*
  // BOOK NTUPLE
       call hlimit(nwpaw)
       IQUEST(10) = 65000 !Big File ?
       Call Hropen(60,'cs','/data/ariel/cs.hbook','NQ',4096,Istat)
       print *,'Book ntuples'
       call hbookn(1,'cs scattered photons',intup,'cs',1000,tags)
       call hbookn(2,'unscattered  photons',intup,'cs',1000,tags)
  */
  }

  randinit(); // initilaize random number generation
  
  /*
     - adjust scattering length so that K-band has ndust scattering lengths
              scatconst = (radius/xscat)*abs_K(1)/(1./albedo(1)-1.)
     - Adjust scattering length to given value of E(B-V)
  */
  al_B = draine[B_i].albedo;
  al_V = draine[V_i].albedo;
  a_B  = draine[B_i].abs_k;
  a_V  = draine[V_i].abs_k;
  factor = (double) a_B/(1.- al_B) - a_V/(1. - al_V);

  //  factor = (double) draine[B_i].c_ext - draine[V_i].c_ext;
  //  printf("factor: %f\n",(factor-(draine[B_i].c_ext-draine[V_i].c_ext))/factor);

  scatconst = ebv/flux2mag/(radius-iradius)/factor;  // This is the number density
  // printf("scatconst %.2e\n",scatconst);

  //  factor = exp(-flux2mag*ebv);
  //  scatconst = 3*(1-factor)/(a_B/(1.-al_B) - a_V/(1.-al_V)*factor)/radius;

  printf("Initial E(B-V) = %.2f\n",ebv);
  
  // Setup FITS output
  //
  if (photfname != NULL) {
    
    // Open the FITS file containing a primary array and an ASCII table
    finalphotfname = (char*) calloc(strlen(photfname)+strlen(overwrite)+1,sizeof(char));
    strcat(finalphotfname,overwrite);
    strcat(finalphotfname,photfname);
    
    if ( fits_create_file(&photfptr, finalphotfname, &status) ) 
      printerror( status );
    
    // Append a new empty binary table onto the FITS file
    if ( fits_create_tbl( photfptr, BINARY_TBL, nphot, ncols*nwavel, ttype, tform,
      			  tunit, hduname, &status) )
      printerror( status );
  }
  
  for (jj=0; jj<nwavel; jj++) {           // loop over wavelengths
    
    // Initialize
    sum_cs[jj]    = 0.;
    sum_abs[jj]   = 0.;
    avg_pol_i[jj] = 0.;
    avt[jj]       = 0.;
    nhit[jj]      = 0;
    
    // Mean free path
    //
    scatdust[jj] = ((double) 1./draine[jj].abs_k*(1./draine[jj].albedo-1.))/scatconst;
    absdust[jj]  = ((double) 1./draine[jj].abs_k)/scatconst;
  
    // printf("%.2f %.3f %.3f\n",draine[jj].lambda*10000.,scatdust[jj]/radius,absdust[jj]/radius);

    //    printf("Delta_Albedo = %.f\n",
    //	   absdust[jj] - 1.0/numbdens/(draine[jj].c_ext*(1.0/draine[jj].albedo - 1.0)));
    tau_dust     = (double) draine[jj].cost;
    smin         = scatdust[jj]/10000.0;
    
    for (iphot=0; iphot<nphot; iphot++) { // loop over photons
      // Start from (0,0,0)
      x        = 0.;
      y        = 0.;
      z        = 0.;
      path     = 0.;
      paththroughdust = 0.;

      absphoton = 0;
      nextphoton = 0;

      e_ortho = 1.0/sqrt(2.0);
      e_paral = 1.0/sqrt(2.0);
      
      astop = rnexp(absdust[jj]); // absorption length
      fractime[iphot] = 0.0;      // keeps track of absorbed photons as well

#ifdef sanity
      photon.astop = astop/radius;
      photon.iterations = 0;
#endif

      // propagate photon until it is absorbed or leaves the
      // the dust sphere
      //
      istep = 0;
      
      while (istep < nmax && nextphoton == 0) {	

	// generate an azimuthal (isotropic) angle, this can be seen as the
	// angle of the scattering plane
	//
	// phi      = 2.*pi * randunit();
	cosphi = (cosmax-cosmin) * randunit() + cosmin;
	phi    = acos(cosphi);
	sinphi = sin(phi);

	// Generate scattering angle
	//  - random angle for first and reemission of absorbed photon
	//  - henyey-greenstein scattering angle for dust scattering
	//
	if (istep == 0 || absphoton == 1)  { // random angle
	  costheta = (cosmax-cosmin) * randunit() + cosmin;
	  theta    = acos(costheta); // 0.0 < theta < pi
	  tottheta = 0.0;
	  
	  if (savefirstangle) firstangle[iphot] = theta;
	} else { 	                     // generate a scattering angle with dust
	  costheta = cthenyey_greenstein(tau_dust);
	  theta    = acos(costheta);
	  tottheta += fabs(theta);

	  if (istep == 1 && savefirstangle)
	    firstangle[iphot] = theta;
	}
	sintheta = sin(theta);

#ifdef sanity
	photon.iterations = istep;
	photon.history[istep]->theta = theta;
	photon.history[istep]->phi   = phi;
#endif

	// Now we have a scattering angle in the coordinate system of the 
	// incoming photon.  Now calculate how far the photon will travel
	// during this iteration.

	// q = 0.5 * the length of the path the photon will travel inside the shell
	// (see note for explanation).
	//
	q = 0.0;

	// If the photon was absorbed in the previous step, we will assume that it
	// is reemitted immediately at much longer wavelengths where the extinction
	// cross-section is negligiable.  We wish to keep these photons to allow
	// for studies of reemission, and therefore set sdust to something
	// large and let it propagate through the code.
	//
	if (absphoton == 1) {
	  sdust = 100 * radius;
	  astop = 100 * sdust;                     // must have astop > sdust below
	  nextphoton = 1;                          // last iteration photon will leave
	  step  = sdust;                           // this will take us out of the sphere
	} else {
	  // At this stage the photon is within the dust sphere and we need to
	  // calculate the scattering angles and the path length to next 
	  // interaction.
	  //
	  sdust = rnexp(scatdust[jj]);             // length to next scatter with dust
	  if (sdust < smin) sdust = smin;
#ifdef sanity
	  photon.history[istep]->sdust = sdust/radius;
#endif

	  // allow photon to travel 'step' wo interaction which could be longer than
	  // sdust/astop since the photon could travel within the shell where there is no
	  // dust.
	  //
	  if ( astop <= sdust + paththroughdust && absphoton == 0 ) {  // photon will get absorbed or leave sphere
	    absphoton = 1;                            // allow one more iteration for remitted photon
	    step = astop - paththroughdust;
	  } else {                                    // photon will scatter or leave sphere
	    step = sdust;
	  }
	}

	rnow  = sqrt(x*x + y*y + z*z);             // current position of photon

	// Calculate where the photon will end up in stellar system coordinates.
	//
	if (istep == 0) {

	  // If this is the first iteration, i.e., the photon is emitted from the origin,
	  // we need to add iradius, to step into the dust region
	  //
	  step += iradius;
	  q = 0.5*iradius;  // 0.5 * distance travelled inside the shell in this step

	  odx = 0.;
	  ody = 0.;
	  odz = 0.;
	  dx = step*sintheta*cosphi;
	  dy = step*sintheta*sinphi;
	  dz = step*costheta;

	  // These will never be used, only set to avoid compilation warnings.
	  //
	  gamma = 0.;
	} else {
	  odx = dx;
	  ody = dy;
	  odz = dz;
	  transpose(step,sintheta,costheta,sinphi,cosphi,&dx,&dy,&dz);

	  // If the photon is currently in the dust shell (which it should be) we
	  // must check whether it will get closer to the origin than iradius during
	  // the next step.  If it does, it means that it will cross the dust shell,
	  // and we need to calculate the length of the path before it enters the shell
	  // again.
	  //
	  // See note for this calculation.
	  // 
	  rnext = sqrt((x+dx)*(x+dx) + (y+dy)*(y+dy) + (z+dz)*(z+dz));
	  rnorm = sqrt(dx*dx + dy*dy + dz*dz);
	  gamma = acos( - (x*dx + y*dy + z*dz)/(rnorm*rnow) ); // 0.0 < gamma < pi
	  
	  // This should not happen
	  //
	  if (gamma > pi || gamma < 0.0)
	    fprintf(stderr,"Warning: gamma is out of range!\n");

	  // Calculate the impact parameter, i.e. the shortest distance from the
	  // origin of the photon track.
	  //
	  impact = rnow;
	  if (gamma < 0.5*pi) impact = rnow*sin(gamma);

	  // Photon will cross the shell and then re-enter.  Add the path
	  // spent outside the shell to the step (where we are not affected by
	  // dust) and then re-calculate the displacement vector.
	  // 
	  if (impact < iradius) {
	    q = sqrt(iradius*iradius - impact*impact);
	    
	    // is the step length enough to cross the shell?
	    //
	    if (sqrt(rnow*rnow - impact*impact) - q < step) {
	      step += 2.0*q;
	      dx = odx;
	      dy = ody;
	      dz = odz;
	      transpose(step,sintheta,costheta,sinphi,cosphi,&dx,&dy,&dz);
	    } else { // reset the q-value
	      q = 0.;
	    }
	  }
	} // end of calculation where photon will end up.

	//  This is the distance from the origin to the point where the 
	//  photon will end up
	//
	rnext = sqrt((x+dx)*(x+dx) + (y+dy)*(y+dy) + (z+dz)*(z+dz));

#ifdef sanity
	photon.history[istep]->gamma = gamma;
	photon.history[istep]->q = q/radius;
	photon.history[istep]->x = x/radius;
	photon.history[istep]->y = y/radius;
	photon.history[istep]->z = z/radius;
	photon.history[istep]->dx = dx/radius;
	photon.history[istep]->dy = dy/radius;
	photon.history[istep]->dz = dz/radius;
#endif	

	// Walk through all possible cases for the photon!
	//
	if (rnow <= radius && rnext > radius) {               // photon will leave sphere
	  
	  // If the absorption condition above was fulfilled but the photon will
	  // leave the sphere rather than getting absorbed, we have to reset
	  // the absphoton flag.
	  //
	  // If this is the iteration for a reemitted photon that was absorbed
	  // in the previous iteration, we set astop to something huge above.
	  //
	  if ( astop <= sdust + paththroughdust && absphoton == 1) absphoton = 0;
	  
	  // calculate 's' and 'd'
	  //
	  if (istep > 0) {
	    theta = asin(sin(gamma) * rnow/radius);           // -pi/2 < theta < pi/2
	    
	    // This should never happen for the reason described above.
	    //
	    if (theta < 0.0)
	      fprintf(stderr,"Error: theta < 0.0 : theta = %.3f!\n", theta);
	    
	    // This is the total length the photon travels from its current position
	    // within the dust region until the edge of the sphere upon exiting.
	    //
	    s = sqrt(rnow*rnow + radius*radius - 2.0*rnow*radius*cos(pi-gamma-theta));
	    if (s > 2*radius)
	      fprintf(stderr,"Error: s = %.2f > 2w = %.2f should not happen!\n",
		      s,2*radius);
	    
	    // Add the length of the photon will travel after leaving the shell and
	    // before crossing the plane perpendicular to the line of sight.  This is
	    // is the distance 'd' in the note.
	    //
	    d = 2.0*radius*sin(0.5*theta)*sin(0.5*theta);
	    if (d > radius) 
	      fprintf(stderr,"Error: d/r = %.2f > 1 should not happen!\n",d/radius);
	    
	    // This is a sanity check that will make sure that we land on the
	    // sphere after displacing the distance s.
	    //
	    /*
	    dx = odx;
	    dy = ody;
	    dz = odz;
	    transpose(s,sintheta,costheta,sinphi,cosphi,&dx,&dy,&dz);
	    foo = fabs(sqrt((x+dx)*(x+dx) + (y+dy)*(y+dy) + (z+dz)*(z+dz))/radius - 1.0);
	    if ( foo > 0.01) 
	      fprintf(stderr,"Error: displacement inconsistent with sphere radius: %.3f\n!",rnext);
	      
	    // Restore displacement vector
	    //
	    dx = odx;
	    dy = ody;
	    dz = odz;
	    transpose(step,sintheta,costheta,sinphi,cosphi,&dx,&dy,&dz);
	    */
	  } else { // photon does not interact with dust and leaves the sphere immediately
	    // if (jj == B_i && istep == 0) printf("%.5e\n",sdust);
	    s = radius; // - iradius;
	    d = 0;
	  }

#ifdef sanity
	  photon.history[istep]->type = adios;
#endif

	  path += s + d;

	  // We need to subtract the part of the path that did not go through
	  // the dust layer (this variable will never be used again though)
	  //
	  // if (jj == B_i && iphot < 3) printf("s/2*q/d = %.2f/%.2f/%.2f\n",s/radius,2.0*q/radius,d);
	  if (absphoton == 0) paththroughdust += s - 2.0*q;

	  nextphoton = 1;                   // next photon please!
	} else if (rnext >= iradius && rnext <= radius) {     // photon will not leave sphere
	  if (absphoton == 1) {                     // photon will be absorbed
#ifdef sanity
	    photon.history[istep]->type = absorption;

	    // sanity check, this should be zero if step > 0
	    //
	    /*
	      This original condition was correct before we set q = 0.5*iradius for all
	      photons at step == 0 (before it was q=0 which resulted in path=radius-iradius
	      and not path=radius for photons that did not interact at all.

	    if ( (istep == 0 && fabs(astop-paththroughdust - (step-iradius-2.0*q)) > 0.01) ||
		 (istep > 0  && fabs(astop-paththroughdust - (step-2.0*q)) > 0.01) ) {
	    */
	    if ( fabs(astop-paththroughdust - (step-2.0*q)) > 0.01 ) {
	      fprintf(stderr,"Error: astep - (step-2.0*q) = %.2f\n",astop-(step-2.0*q));
	      printphotonhistory(iphot,&photon);
	      exit(1);
	    }
#endif

	    paththroughdust = astop;
	  } else {                                  // photon will scatter	    
	    // We need to keep track of how much of the travelled path that has been
	    // through the dust layer.
	    //
	    paththroughdust += sdust;

#ifdef sanity
	    photon.history[istep]->type = scattering;
	    // sanity check, this should be zero
	    //
	    /*
	      This original condition was correct before we set q = 0.5*iradius for all
	      photons at step == 0 (before it was q=0 which resulted in path=radius-iradius
	      and not path=radius for photons that did not interact at all.

	    if ( (istep == 0 && fabs(sdust - (step-iradius-2.0*q)) > 0.01) ||
		 (istep > 0 && fabs(sdust - (step-2.0*q)) > 0.01) ) {
	    */
	    if ( fabs(sdust - (step-2.0*q)) > 0.01 ) {
	      fprintf(stderr,"Error: sdust - (step-2.0*q) != 0, sdust/step/2*q = %.1f/%.1f/%.1f\n",
		      sdust,step,2.0*q);
	      printphotonhistory(iphot,&photon);
	      exit(1);
	    }
#endif

	    // Update polarisation vectors.
	    //
	    if (dopolar) {
	      pvalue = draine_pvalue(polar,draine[jj].lambda,acos(costheta)*180./pi);
	      if (pvalue == -1) {
		fprintf(stderr,"Warning: failed to calculate P for %2f microns and %1f deg!\n",
			draine[jj].lambda,acos(costheta)*180./pi);
	      } else {
		e_ortho = sqrt(1-pvalue);
		e_paral = sqrt(1+pvalue);
	      }
	    }
	  }
	  path += step;                 // integrated path length
	} else if (rnext < iradius) {	// rnext < iradius should never happen!
	  fprintf(stderr,"Error: photon cannot interact within dust shell r = %.2f!\n",
		  rnext/radius);
	  printf("rnow = %.2f,rnext = %.2f\n",rnow/radius,rnext/radius);
#ifdef sanity
	  printphotonhistory(iphot,&photon);
	  exit(1);
#endif
	}  // end of if photon leaves the sphere or not
	  
	if (nextphoton == 1) { 
	  
	  // This is the relative time ratio compared to a dust-free
	  // scenario (i.e = 1.0 for no interaction)
	  //
	  if (absphoton == 1)
	    fractime[iphot] = (float) -path/radius;  // Absorbed photons have negative pathlength
	  else 
	    fractime[iphot] = (float) path/radius;

	  if (saveangles)
	    totangle[iphot] = (float) tottheta;
	  if (savescatter)
	    nscatter[iphot] = (unsigned short) istep;
	  avt[jj] += fractime[iphot];
	  
	  if (absphoton == 0) { // Photon is leaving the sphere
	    sum_cs[jj]++;       // Photons that left the sphere
	    if (istep == 0) {    // Photon left sphere without interacting at all
	      sum_abs[jj]++; }
	    
	    // Update the deg of polarisation for given lambda
	    //
	    e_paral = e_paral * e_paral;   // Amplitude to intensity
	    e_ortho = e_ortho * e_ortho;
	    avg_pol_i[jj] += (e_paral-e_ortho)/(e_paral+e_ortho);
	  }
	}

#ifdef sanity
	// Sanity check that the displacement vector is defined.  If it has been
	// transposed from the origin dx = dy = dz = 0, this is not going to be
	// the case.  Then this probably means that the transpose function has
	// been executed when it should not.
	//
	if (isnan(dx) || isnan(dy) || isnan(dz) ) {
	  fprintf(stderr,"Error: photon = %7d has udefined displacement vectors!\n",iphot);
	  nextphoton = 1;
	}
#endif

	// Propagate photon to the next position
	//
	x += dx;
	y += dy;
	z += dz;

#ifdef sanity
	photon.history[istep]->path = path/radius;
	photon.history[istep]->paththroughdust = paththroughdust/radius;
	
	// Sanity check! Make sure that the photon is where it should be!
	//
	foo = sqrt(x*x + y*y + z*z);
	if ( photon.history[istep]->type == scattering && (foo < iradius*0.95 || foo > radius*1.05) ) {
	  fprintf(stderr,"Error: photon scattered outside of dust region: %f (pos/radius) istep = %d, photon = %d!\n",
		  foo/radius,istep,iphot);
	  printphotonhistory(iphot,&photon);
	} else if (photon.history[istep]->type == absorption && (foo < iradius*0.95 || foo > radius*1.05) ) {
	  fprintf(stderr,"Error: photon absorbed outside of dust region: %f (pos/radius) istep = %d, photno = %d!\n",
		  foo/radius,istep,iphot);
	  printphotonhistory(iphot,&photon);
	} else if ( photon.history[istep]->type == adios && foo < radius*0.95 ) {
	  fprintf(stderr,"Error: photon should have left but is still within dust region!\n");
	  printphotonhistory(iphot,&photon);
	}

	// sanity check, paththroughdust can never exceed the astop
	//
	if (paththroughdust > astop && absphoton == 0) {
	  fprintf(stderr,"Error: path through dust has exceeded the absorption length!\n");
	  printphotonhistory(iphot,&photon);
	}
#endif

	istep++;
      } // loop over steps

#ifdef sanity
      // Print the photon history
      //
      if (debug == 10 && jj==B_i && iphot < 5) {
	printphotonhistory(iphot,&photon);
      }
#endif

    } // loop over photons
    
    if (photfname != NULL) {
      k = 1;
      if ( fits_write_col(photfptr, TFLOAT, ncols*jj+k, 1, 1, nphot, fractime, &status) )
	printerror(status);
      k++;
      if (saveangles) {
	if ( fits_write_col(photfptr, TFLOAT, ncols*jj+k, 1, 1, nphot, totangle, &status) )
	  printerror(status);
	k++;
      }
      if (savescatter) {
	if ( fits_write_col(photfptr, TUSHORT, ncols*jj+k, 1, 1, nphot, nscatter, &status) )
	  printerror(status);
	k++;
      }
      if (savefirstangle) {
	if ( fits_write_col(photfptr, TFLOAT, ncols*jj+k, 1, 1, nphot, firstangle, &status) )
	  printerror(status);
	k++;
      }
    }
    
  } // loop over wavelengths
  
  
  // Calculate E(B-V) both for both CS and IS dust
  //
#ifdef sanity
  fprintf(stderr,"%d/%d %d/%d\n",sum_abs[B_i],nphot,sum_abs[V_i],nphot);
  fprintf(stderr,"%d/%d %d/%d\n",sum_cs[B_i],nphot,sum_cs[V_i],nphot);
#endif
  ebv_cs =   -2.5*log10((double) sum_cs[B_i]/((double) nphot)) + 
    2.5*log10((double) sum_cs[V_i]/((double) nphot));
  ebv_cs_err = (2.5/log(10.)) * sqrt( 1./sum_cs[B_i] + 1./sum_cs[V_i] );
  ebv_is =   -2.5*log10((double) sum_abs[B_i]/((double) nphot)) + 
    2.5*log10((double) sum_abs[V_i]/((double) nphot));
  ebv_is_err = (2.5/log(10.)) * sqrt( 1./sum_abs[B_i] + 1./sum_abs[V_i] );

  printf("E(B-V)_CS = %.2f (%.2f), E(B-V)_IS = %.2f (%.2f)\n",ebv_cs,ebv_cs_err,ebv_is,ebv_is_err);

 
  // Output files (save these in fits extension as well)?
  //
  scstest = fopen("scattering_test_LMC_CS.out","w");
  ccstest = fopen("color_test_LMC_CS.out","w");     // Output file for CS dust
  cistest = fopen("color_test_LMC_IS.out","w");     // Output for IS sanity
  
  fprintf(ccstest,"# Summary of colors \n");
  fprintf(cistest,"# Summary of colors \n");
  fprintf(scstest,"lambda tau_s tau_a albedo costhe time EBV\n");
  fprintf(scstest,"- - - - - - - - - - - - - - - - - - - - - - -\n");
  
  
  for (i=nwavel-1; i>=0; i--) {
    abs_x_is[i] = -2.5*log10(sum_abs[i]/((double) nphot));
    abs_x_cs[i] = -2.5*log10(sum_cs[i]/((double) nphot));
    avt[i]      = avt[i]/sum_cs[i];
    fprintf(scstest,"%10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
	    ebv_cs,draine[i].lambda,scatdust[i]/radius,absdust[i]/radius,
	    draine[i].albedo,draine[i].cost,avt[i]);
  } 
  fprintf(ccstest,"# E(B-V) = %6.3f\n# R_V = %6.3f\n#\n",
	  ebv_cs,abs_x_cs[V_i]/ebv_cs);
  fprintf(cistest,"# E(B-V) = %6.3f\n# R_V = %6.3f\n#\n",
	  ebv_is,abs_x_is[V_i]/ebv_is);
  fprintf(ccstest,"# lambda (microns) :\n# A_X (mag):\n");
  fprintf(cistest,"# lambda (microns) :\n# A_X (mag):\n");
  for (k=nwavel-1; k>=0; k--) {
    fprintf(ccstest,"%6.4f %6.3f\n",draine[k].lambda,abs_x_cs[k]);
    fprintf(cistest,"%6.4f %6.3f\n",draine[k].lambda,abs_x_is[k]);
  }
  fprintf(ccstest,"\n");
  fprintf(cistest,"\n");
  
  fclose(ccstest);
  fclose(cistest);
  fclose(scstest);
  //
  // end of saving output
  
  
  /*
    if (dopaw) {
    CALL HROUT(0,ICYCLE,' ')
    CALL HREND('cs')
    }
  */
  
  
  // FITS header
  // 
  if ( photfname != NULL ) {
    if ( fits_movabs_hdu(photfptr, 1, &hdutype, &status) )  // move to Primary HDU
      printerror( status );

    // Number of photons used for simulation
    //
    sprintf(newcard,"NPHOT = %d / Number of photons per wavelength",nphot);
    // reformat the keyword string to conform to FITS rules
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    // overwrite the keyword with the new value
    if ( fits_update_card(photfptr, "NPHOT", card, &status) )
      printerror(status);
      
    // Dust model used
    //
    sprintf(newcard,"DUSTMD = %s / Dust model used for simulation",dusttype);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "DUSTMD", card, &status) )
      printerror(status);
    
    // E(B-V) used
    //
    sprintf(newcard,"EBV = %.3f / Input E(B-V)",ebv);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "EBV", card, &status) )
      printerror(status);

    // Calculated E(B-V) for photons that leave the sphere wihtout scattering
    //
    sprintf(newcard,"EBVIS = %.3f / E(B-V) for photons with no scattering",ebv_is);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "EBVIS", card, &status) )
      printerror(status);

    // Calculated E(B-V) for all photons leaving the sphere
    //
    sprintf(newcard,"EBVCS = %.3f / E(B-V) for all photons leaving the sphere",ebv_cs);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "EBVCS", card, &status) )
      printerror(status);


    // CVS version of files used
    //
    sprintf(newcard,"DRAINEH = %f / CVS version of draine.h",DRAINEH);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "DRAINEH", card, &status) )
      printerror(status);
    sprintf(newcard,"DRAINEC = %f / CVS version of draine.c",DRAINEC);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "DRAINEC", card, &status) )
      printerror(status);
    sprintf(newcard,"SCATTERH = %f / CVS version of scattercs.h",SCATTERH);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "SCATTERH", card, &status) )
      printerror(status);
    sprintf(newcard,"SCATTERC = %f / CVS version of scattercs.c",SCATTERC);
    if ( fits_parse_template(newcard, card, &keytype, &status) )
      printerror(status);
    if ( fits_update_card(photfptr, "SCATTERC", card, &status) )
      printerror(status);


    // Close the fits file
    //
    if ( fits_close_file(photfptr, &status) )
      printerror( status );
    
  }
  
  // Free up the memory allocated
  //
  if (photfname != NULL) {
    for (jj=0; jj<nwavel; jj++)
      free(ttype[jj]);

    free(ttype);
    free(tform);
    free(tunit);
    free(finalphotfname);
  }

  if (polar != NULL) free_draine_polar(polar);
  free(draine);

  free(scatdust);
  free(absdust);
  free(avt);
  free(abs_x_is);
  free(abs_x_cs);
  free(sum_cs);
  free(sum_abs);
  free(avg_pol_i);
  free(fractime);
  if (saveangles) free(totangle);
  if (savescatter) free(nscatter);
  if (savefirstangle) free(firstangle);
  free(nhit);

#ifdef sanity
  for (jj=0; jj<nmax; jj++)
    free(photon.history[jj]);
#endif

  return 0;
}

void printerror( int status) {

  /*****************************************************/
  /* Print out cfitsio error messages and exit program */
  /*****************************************************/  
  
  if (status) {
    fits_report_error(stderr, status); /* print error report */
    
    exit( status );    /* terminate the program, returning error status */
  }
  return;
}


/*
 *  Calculate the displacement vector in the coordinate system of the star
 *  from the step length, and the scattering angles of the photon, relative
 *  to its track.  Note, that hte direction of the track is given by the 
 *  vector (dx,dy,dz) that is then replaced by the new displacement vector.
 */
void transpose(double step, 
	       double sintheta, double costheta,
	       double sinphi,   double cosphi,
	       double *dx,double *dy,double *dz) {
  
  double cthetr, thetr, phitr, rnorm;
  double dxx, dyy, dzz;
  double dxtr, dytr, dztr;

  // Just to make the code slightly cleaner
  //
  dxx = *dx;
  dyy = *dy;
  dzz = *dz;

  // This are the coordinate changes with respect to the TRACK
  //
  dxtr = step*sintheta*cosphi;
  dytr = step*sintheta*sinphi;
  dztr = step*costheta;

  // coordinates in the photon track system
  if (dxx == 0. && dyy == 0.)
    phitr = 0.;
  else
    phitr  = atan2(dyy,dxx);
  cthetr = dzz/sqrt(dxx*dxx+dyy*dyy+dzz*dzz);
  thetr = acos(cthetr);   // 0.0 < thetr < pi
  
  // these are in the coordinate system of the star
  transpose_lbe(thetr,phitr,dxtr,dytr,dztr,&dxx,&dyy,&dzz);
  rnorm=sqrt((dxx*dxx+dyy*dyy+dzz*dzz)/(dxtr*dxtr+dytr*dytr+dztr*dztr));
  if (fabs(rnorm-1.) > 0.0001)
    printf("Matrix normalization OFF: %f\n",rnorm);

  *dx = dxx;
  *dy = dyy;
  *dz = dzz;
}


void transpose_lbe(double theta,double phi,
		   double x1,double y1,double z1,
		   double *x2,double *y2,double *z2) {
  double cphi, sphi, cthe, sthe;
  
  cphi = cos(phi);
  sphi = sin(phi);
  sthe = sin(theta);
  cthe = cos(theta);
  *x2 = (cphi*cphi * cthe + sphi*sphi)*x1 + (cphi*sphi*cthe-cphi*sphi)*y1;
  *x2 = (*x2) + sthe*cphi*z1;
  *y2 = (sphi*cphi*cthe-cphi*sphi)*x1;
  *y2 = (*y2) + (sphi*sphi * cthe + cphi*cphi)*y1+sphi*sthe*z1;
  *z2 = -sthe*cphi*x1-sthe*sphi*y1+cthe*z1;
}
  
  
// ************************************************
// * formula taken from manuscript for applied optics
// *
// **************************************************
//
// L.G. Henyey, J.L. Greenstein,
// Diffuse radiation in the galaxy, ApJ 93:70-83, 1941.
//
//
double cthenyey_greenstein(double tau) {
  double r, rt, ct;
  r  = randunit();
  rt = r*tau;
  ct = (1. - tau + rt)*r*pow((1. + tau),2)/pow((1. - tau + 2.*rt),2);
  ct = 2.*ct-1.;
  if (fabs(ct) > 1.0) {
    printf("Warning !! ct=%f\n",ct);
    if (ct > 1.) {
      ct = 1.;
    } else {
      if (ct < -1.) ct = -1.;
    }
  }
  return ct;
}






/*
  FUNCTION ATTICE(WAVEL)
  REAL wavel
  * Function due to Lars Bergstroem, output in m^-1 input in nm
  attice = 1./(8100.*exp(-6700./wavel))
  end  
*/


/*
  c       SUBROUTINE TRANSPOSE(THETA,PHI,X1,Y1,Z1,X2,Y2,Z2)
  c       cphi=cos(phi)
  c       sphi=sin(phi)
  c       cthe=cos(theta)
  c       sthe=sin(theta)  
  c       x2 =  x1*cphi*cthe +y1*sphi*cthe -z1*sthe
  c       y2 = -x1*sphi      +y1*cphi 
  c       z2 =  x1*sthe*cphi +y1*sthe*sphi +z1*cthe
  c       END
  


  SUBROUTINE TRANSPOSE(THETA,PHI,X1,Y1,Z1,X2,Y2,Z2)
  * It is actually the inverse or transponate of the matrix above !!
  cphi=cos(phi)
  sphi=sin(phi)
  cthe=cos(theta)
  sthe=sin(theta)  
  x2 =   x1*cphi*cthe -y1*sphi   +z1*sthe*cphi
  y2 =   x1*cthe*sphi +y1*cphi   +z1*sthe*sphi
  z2 =  -x1*sthe                 +z1*cthe
  END
  
*/
