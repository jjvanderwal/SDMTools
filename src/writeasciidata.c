/* Fuction that writes out ascii grid files quickly
*/

#include <R.h> 
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
 
//function to get row minimums from a matrix
SEXP writeascdata(SEXP nrows, SEXP ncols, SEXP xllcorner, SEXP yllcorner, SEXP cellsize, SEXP tdata, SEXP filename, SEXP sigdigits) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is a binary matrix of data
	
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrow = dims[0]; int ncol = dims[1]; //assign the number of rows and columns in the matrix
	int sigdig = (int)REAL(sigdigits)[0]; // get the number of significant decimal points
	
	//write out the file
	FILE * fp; 
	fp = fopen(CHAR(STRING_ELT(filename,0)),"w+"); //open the file
		fprintf(fp,"ncols         %d\n",INTEGER(ncols)[0]);
		fprintf(fp,"nrows         %d\n",INTEGER(nrows)[0]);
		fprintf(fp,"xllcorner     %s\n",CHAR(STRING_ELT(xllcorner,0)));
		fprintf(fp,"yllcorner     %s\n",CHAR(STRING_ELT(yllcorner,0)));
		fprintf(fp,"cellsize      %s\n",CHAR(STRING_ELT(cellsize,0)));
		fprintf(fp,"NODATA_value  -9999\n");
		//cycle through and write out the data
		int row, col;
		for (col=(ncol-1); col >=0; --col) {
			for (row=0; row<nrow; row++) {
				if (ISNA(data[row+nrow*col])) {
					fprintf(fp,"-9999 ");
				} else {
					fprintf(fp,"%.*f ",sigdig,data[row+nrow*col]);
				}
			}
			fprintf(fp,"\n");
		}		
		
	fclose(fp); //close the file	
	

	//setup the output matrix
	SEXP ans; PROTECT(ans = allocVector(REALSXP, 1));
	double *out = REAL(ans); //pointer to output dataset
	out[0] = 1;
	
	//return the output data
	UNPROTECT(2);
	return(R_NilValue); 
}

