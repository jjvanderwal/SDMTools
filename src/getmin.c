/* File: getmin.c */ 
#include <R.h> 
#include <Rinternals.h> 
SEXP getmin(SEXP x, SEXP y) 
{
    //define the pointers for the data
	PROTECT(x = coerceVector(x, REALSXP));
	double *xdata = REAL(x); //suitability cost matrix
	PROTECT(y = coerceVector(y, REALSXP));
	double *ydata = REAL(y); //suitability cost matrix
	
	int *dims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrows = dims[0]; int ncols = dims[1]; //assign the number of rows and columns in the matrix
	
	//define other variables
	int row, col;
	double xval, yval;
	
	//setup the output
	double *out; SEXP ans;
	PROTECT(ans = allocMatrix(REALSXP, nrows, ncols));
	out = REAL(ans); //pointer to output dataset
	
	//cycle through the data of input matrix
	for (row=0; row<nrows; row++){
        for (col=0; col<ncols; col++){
			xval = xdata[row+nrows*col];
			yval = ydata[row+nrows*col];
			if (ISNA(xval) || ISNA(yval)) {
				out[row+nrows*col] = NA_REAL;
			} else {
				if (xval<yval) {
					out[row+nrows*col] = xval;
				} else {
					out[row+nrows*col] = yval;
				}
			}
        }
    } 
	//return the output data
	UNPROTECT(3);
    return(ans); 
} 
