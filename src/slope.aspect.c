/* File: slope.aspect.c */ 
/*
this calculates slope and aspect of a raster based on ArcGIS 9.3 documentation
http://webhelp.esri.com/arcgiSDEsktop/9.3/index.cfm?TopicName=How%20Slope%20works
http://webhelp.esri.com/arcgisdesktop/9.3/index.cfm?TopicName=How%20Aspect%20(3D%20Analyst)%20works
*/

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>

const double small_num = 0.00000000000001;

/*
The Slope & Aspect algorithm
The rate of change (delta) of the surface in the horizontal (dz/dx) and vertical (dz/dy) directions from the center cell determines the slope. The basic algorithm used to calculate the slope is:
slope_radians = ATAN ( sqrt ( [dz/dx]2 + [dz/dy]2 ) )

Slope is commonly measured in degrees, which uses the algorithm:
slope_degrees = ATAN ( sqrt ( [dz/dx]2 + [dz/dy]2 ) ) * 57.29578

The slope algorithm can also be interpreted as:
slope_degrees = ATAN (rise_run) * 57.29578 
where: 
rise_run = sqrt ( [dz/dx]2 + [dz/dy]2 ] )

The values of the center cell and its eight neighbors determine the horizontal and vertical deltas. The neighbors are identified as letters from 'a' to 'i', with 'e' representing the cell for which the aspect is being calculated.
a b c
d e f
g h i

The rate of change in the x direction for cell 'e' is calculated with the algorithm:
[dz/dx] = ((c + 2f + i) - (a + 2d + g) / (8 * x_cell_size)

The rate of change in the y direction for cell 'e' is calculated with the following algorithm:
[dz/dy] = ((g + 2h + i) - (a + 2b + c)) / (8 * y_cell_size)

Taking the rate of change in both the x and y direction for cell 'e', aspect is calculated using:
aspect = 57.29578 * atan2 ([dz/dy], -[dz/dx])

The aspect value is then converted to compass direction values (0–360 degrees), according to the following rule:
if aspect < 0 { cell = 90.0 - aspect 
} else if aspect > 90.0 { cell = 360.0 - aspect + 90.0
} else { cell = 90.0 - aspect }

Burrough, P. A. and McDonell, R.A., 1998. Principles of Geographical Information Systems (Oxford University Press, New York), p. 190.
*/

/*
x is the data matrix with top is north, and right is east
widths are the cell widths
heights are the cell heights
*/

SEXP slope (SEXP x, SEXP widths, SEXP heights) {
    //define the pointers for the data
	PROTECT(x = coerceVector(x, REALSXP)); //data values in the matrix
	PROTECT(widths = coerceVector(widths, REALSXP)); //data values in the matrix
	PROTECT(heights = coerceVector(heights, REALSXP)); //data values in the matrix
	double *data = REAL(x), *width = REAL(widths), *height = REAL(heights); //create pointers to the data
	int *dims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrows = dims[0]; int ncols = dims[1]; //assign the number of rows and columns in the matrix
	SEXP ans; PROTECT(ans = allocMatrix(REALSXP, nrows, ncols)); ;//setup the output
	double *out = REAL(ans); //pointer to output dataset
	
	double a,b,c,d,f,e,g,h,i; //neighboring cell values
	
	//cycle through the data of input matrix & calculate the slope
	for (int row=0; row<nrows; row++){
        for (int col=0; col<ncols; col++){
			if (ISNA(data[row+nrows*col])) { out[row+nrows*col] = NA_REAL; //if input is NA, set output to NA
			} else { //calculate the slope
				e = data[row+nrows*col]; // define the value of the current cell
				//define the values from the neighboring cells
				a = (row==0 || col==0 || ISNA(data[(row-1)+nrows*(col-1)])) ? e : data[(row-1)+nrows*(col-1)]; 
				b = (row==0 || ISNA(data[(row-1)+nrows*col])) ? e : data[(row-1)+nrows*col]; 
				c = (row==0 || col==ncols-1 || ISNA(data[(row-1)+nrows*(col+1)])) ? e : data[(row-1)+nrows*(col+1)];
				d = (col==0 || ISNA(data[row+nrows*(col-1)])) ? e : data[row+nrows*(col-1)];
				f = (col==ncols-1 || ISNA(data[row+nrows*(col+1)])) ? e : data[row+nrows*(col+1)];
				g = (row==nrows-1 || col==0 || ISNA(data[(row+1)+nrows*(col-1)])) ? e : data[(row+1)+nrows*(col-1)]; 
				h = (row==nrows-1 || ISNA(data[(row+1)+nrows*col])) ? e : data[(row+1)+nrows*col]; 
				i = (row==nrows-1 || col==ncols-1 || ISNA(data[(row+1)+nrows*(col+1)])) ? e : data[(row+1)+nrows*(col+1)];
				//get the rise_run
				double dz_dx = ((c+2*f+i) - (a+2*d+g)) / (8*width[row]);
				double dz_dy = ((g+2*h+i) - (a+2*b+c)) / (8*height[row]);
				out[row+nrows*col] = sqrt( dz_dx * dz_dx + dz_dy * dz_dy ); //store the rise/run info
			}
        }
    } 
	//return the output data
	UNPROTECT(4);
    return(ans); 
}

SEXP aspect (SEXP x, SEXP widths, SEXP heights) {
    //define the pointers for the data
	PROTECT(x = coerceVector(x, REALSXP)); //data values in the matrix
	PROTECT(widths = coerceVector(widths, REALSXP)); //data values in the matrix
	PROTECT(heights = coerceVector(heights, REALSXP)); //data values in the matrix
	double *data = REAL(x), *width = REAL(widths), *height = REAL(heights); //create pointers to the data
	int *dims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrows = dims[0]; int ncols = dims[1]; //assign the number of rows and columns in the matrix

	SEXP ans; PROTECT(ans = allocMatrix(REALSXP, nrows, ncols)); ;//setup the output
	double *out = REAL(ans); //pointer to output dataset
	
	double a,b,c,d,f,e,g,h,i; //neighboring cell values
	
	//cycle through the data of input matrix & calculate the slope
	for (int row=0; row<nrows; row++){
        for (int col=0; col<ncols; col++){
			if (ISNA(data[row+nrows*col])) { out[row+nrows*col] = NA_REAL; //if input is NA, set output to NA
			} else { //calculate the slope
				e = data[row+nrows*col]; // define the value of the current cell
				//define the values from the neighboring cells
				a = (row==0 || col==0 || ISNA(data[(row-1)+nrows*(col-1)])) ? e : data[(row-1)+nrows*(col-1)]; 
				b = (row==0 || ISNA(data[(row-1)+nrows*col])) ? e : data[(row-1)+nrows*col]; 
				c = (row==0 || col==ncols-1 || ISNA(data[(row-1)+nrows*(col+1)])) ? e : data[(row-1)+nrows*(col+1)];
				d = (col==0 || ISNA(data[row+nrows*(col-1)])) ? e : data[row+nrows*(col-1)];
				f = (col==ncols-1 || ISNA(data[row+nrows*(col+1)])) ? e : data[row+nrows*(col+1)];
				g = (row==nrows-1 || col==0 || ISNA(data[(row+1)+nrows*(col-1)])) ? e : data[(row+1)+nrows*(col-1)]; 
				h = (row==nrows-1 || ISNA(data[(row+1)+nrows*col])) ? e : data[(row+1)+nrows*col]; 
				i = (row==nrows-1 || col==ncols-1 || ISNA(data[(row+1)+nrows*(col+1)])) ? e : data[(row+1)+nrows*(col+1)];
				//get the rise_run
				double dz_dx = ((c+2*f+i) - (a+2*d+g)) / (8*width[row]);
				double dz_dy = ((g+2*h+i) - (a+2*b+c)) / (8*height[row]);
				if (dz_dy<small_num && dz_dx<small_num) { out[row+nrows*col] = -1;  //do this if area is flat
				} else { //if area is not flat
					double aspect = 57.29578 * atan2 (dz_dy, -dz_dx); //calculate the aspect info
					if (aspect < 0) { out[row+nrows*col] = 90.0 - aspect; 
					} else if (aspect > 90.0) { out[row+nrows*col] = 360.0 - aspect + 90.0;
					} else { out[row+nrows*col] = 90.0 - aspect; }
				}
			}
        }
    } 
	//return the output data
	UNPROTECT(4);
    return(ans); 
}

