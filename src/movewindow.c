/* File: movewindow.c */ 
#include <R.h> 
#include <Rinternals.h> 
SEXP movewindow(SEXP x, SEXP mwx, SEXP mwy, SEXP mwcost) 
{
    //define the pointers for the data
	PROTECT(x = coerceVector(x, REALSXP));
	double *data = REAL(x); //suitability cost matrix
	double *cost = REAL(coerceVector(mwcost,REALSXP)); //moving window costs associated with distance
	int *X = INTEGER(coerceVector(mwx,INTSXP)), *Y = INTEGER(coerceVector(mwy,INTSXP)); //the shifts in positions associated with the moving window
	int *dims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrows = dims[0]; int ncols = dims[1]; //assign the number of rows and columns in the matrix
	
	//define other variables
	int mwshifts = length(mwcost); //get the number of moves in the moving window 
	int row, col, prow, pcol, ii;
	double v;
	
	//setup the output
	double *out; SEXP ans;
	PROTECT(ans = allocMatrix(REALSXP, nrows, ncols));
	out = REAL(ans); //pointer to output dataset
	
	//cycle through the data of input matrix
	for (row=0; row<nrows; row++){
        for (col=0; col<ncols; col++){
			if (ISNA(data[row+nrows*col])) {
				out[row+nrows*col] = NA_REAL;
			} else {
				v = 1e100;
				for (ii=0; ii<mwshifts; ii++){
					prow = row + Y[ii]; pcol = col + X[ii];
					if (prow >=0 && prow<nrows){
						if (pcol>=0 && pcol<ncols){
							if (data[prow+nrows*pcol]+cost[ii]<v){
								v = data[prow+nrows*pcol]+cost[ii];
							}
						}
					}
				
				}
				out[row+nrows*col] = v; 
			}
        }
    } 
	//return the output data
	UNPROTECT(2);
    return(ans); 
} 
