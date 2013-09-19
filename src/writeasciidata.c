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
	//PROTECT(nrows = coerceVector(nrows, INTSXP)); int nrow = INT(nrows)[0];
	//PROTECT(ncols = coerceVector(ncols, INTSXP)); int ncol = INT(ncols)[0];
	
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
/*
cat("ncols         ", nrow(x), "\n", sep = "", file = zz)
    cat("nrows         ", ncol(x), "\n", sep = "", file = zz)
    cat("xllcorner     ", as.character(attr(x, "xll") - attr(x,
        "cellsize")/2), "\n", sep = "", file = zz)
    cat("yllcorner     ", as.character(attr(x, "yll") - attr(x,
        "cellsize")/2), "\n", sep = "", file = zz)
    cat("cellsize      ", as.character(attr(x, "cellsize")),
        "\n", sep = "", file = zz)
    cat("NODATA_value  ", -9999, "\n", sep = "", file = zz)



int main()
{
	double * ptr_data;
	int n = 150000000;
	ptr_data = (double*) calloc ( n,sizeof(double) );
	int i, j;
	for (0;i<n;i++){ ptr_data[i] = rand() / 3;}
	printf("finished initializing with data\n");
	FILE * fp;

	fp = fopen ("file.txt", "w+");
	fprintf(fp, "%s %s %s %d", "We", "are", "in", 2012);
	printf("wrote first line\n");
	for (i=0;i<n;i++){
		//printf("%d ",multi_dimensional_array[i]);
		fprintf(fp,"%f ",ptr_data[i]);
	}
	printf("finished writing data\n");
	fclose(fp);

	free (ptr_data);
	return 0;
}
*/
