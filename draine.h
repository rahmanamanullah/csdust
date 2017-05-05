// Extinction, scattering and absorption properties
//
// ftp://ftp.astro.princeton.edu/draine/dust/mix/
//


// Polarisation structure
//
typedef struct {
  // Polarisation table
  //   - first index is wavlelength (microns)
  //   - second index is scattering angle (deg)
  float **table;

  float *lambda;           // Wavlength vector (microns)
  float *angles;           // Scattering angle (degrees)

  unsigned int nlambda;    // Length of wavelength vector
  unsigned int nangles;    // Length of angle vector

  float minlambda;         // Min value of wavelength vector
  float maxlambda;         // Max value of wavelength vector

  float minangles;         // Min value of angle vector
  float maxangles;         // Max value of angle vector
  
} polarisation;


// Structure for extinction and scattering
//
typedef struct {
  float lambda;
  float albedo;
  float c_ext;
  float abs_k;
  float cost;
  float cos2t;
  
} scatext;


// Read extinction and scattering table
//
scatext* init_draine_extscat(char *dusttype,float minlambda, float maxlambda,
			     unsigned int resolution, unsigned int *nwavel);
void find_draine_lambda_idx(scatext *list, unsigned int len, float lambda, int *idx);


// Read polarisation table and return pointer to struct
//
polarisation* init_draine_pol(char *dusttype);
float draine_pvalue(polarisation *p, float l, float a);
void free_draine_polar(polarisation *p);
void readvector(const char *filename, float *vec, int *vlen, unsigned int *maxvlen);

polarisation* failed(char *tablef, char *lambdaf, char *anglesf, polarisation* p,
	       float *angles, float *lambda, float **table) {
  if (tablef  != NULL) free(tablef);
  if (lambdaf != NULL) free(lambdaf);
  if (anglesf != NULL) free(anglesf);
  if (p       != NULL) free(p);
  if (angles  != NULL) free(angles);
  if (lambda  != NULL) free(lambda);
  if (table   != NULL) free(table);
  return NULL;
}
