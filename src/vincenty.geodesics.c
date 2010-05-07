/* File: vincenty.geodesics.c */

#include <R.h> 
#include <Rinternals.h>
#include <Rmath.h>

#include <math.h>

//define some global constants
double a = 6378137, b = 6356752.3142,  f = 1/298.257223563;  // WGS-84 ellipsiod

/*
 * Calculate destination point given start point lat/long (numeric degrees), 
 * bearing (numeric degrees) & distance (in m).
 *
 * from: Vincenty direct formula - T Vincenty, "Direct and Inverse Solutions of Geodesics on the 
 *       Ellipsoid with application of nested equations", Survey Review, vol XXII no 176, 1975
 *       http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
 */

SEXP dest(SEXP latitude1, SEXP longitude1, SEXP bearing, SEXP distance) {
	//bring in the key data
	latitude1 = coerceVector(latitude1, REALSXP); double lat1 = REAL(latitude1)[0] * (PI/180); //first lat in radians
	longitude1 = coerceVector(longitude1, REALSXP); double lon1 = REAL(longitude1)[0] * (PI/180); //first lon in radians
	bearing = coerceVector(bearing, REALSXP); double alpha1 = REAL(bearing)[0] * (PI/180); //bearing in radians
	distance = coerceVector(distance, REALSXP); double s = REAL(distance)[0]; //distance in m
	
	//define all the variables
	double sinAlpha1, cosAlpha1, tanU1, cosU1, sinU1, sigma1, sinAlpha, cosSqAlpha;
	double uSq, A, B, sigma, sigmaP, cos2SigmaM, sinSigma, cosSigma, deltaSigma;
	double tmp, lat2, lambda, C, L, revAz;
	
	//start doing some of the calculations
	sinAlpha1 = sin(alpha1);
	cosAlpha1 = cos(alpha1);
	tanU1 = (1-f) * tan(lat1);
	cosU1 = 1 / sqrt((1 + tanU1*tanU1));
	sinU1 = tanU1*cosU1;
	sigma1 = atan2(tanU1, cosAlpha1);
	sinAlpha = cosU1 * sinAlpha1;
	cosSqAlpha = 1 - sinAlpha*sinAlpha;
	uSq = cosSqAlpha * (a*a - b*b) / (b*b);
	A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
	B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));

	sigma = s / (b*A), sigmaP = 2*PI;
	cos2SigmaM = cos(2*sigma1 + sigma);
	sinSigma = sin(sigma);
	cosSigma = cos(sigma);
	while (abs(sigma-sigmaP) > 1e-12) {
		cos2SigmaM = cos(2*sigma1 + sigma);
		sinSigma = sin(sigma);
		cosSigma = cos(sigma);
		deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)-B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)));
		sigmaP = sigma;
		sigma = s / (b*A) + deltaSigma;
	}

	tmp = sinU1*sinSigma - cosU1*cosSigma*cosAlpha1;
	lat2 = atan2(sinU1*cosSigma + cosU1*sinSigma*cosAlpha1, (1-f)*sqrt(sinAlpha*sinAlpha + tmp*tmp));
	lambda = atan2(sinSigma*sinAlpha1, cosU1*cosSigma - sinU1*sinSigma*cosAlpha1);
	C = f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha));
	L = lambda - (1-C) * f * sinAlpha * (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)));

	revAz = atan2(sinAlpha, -tmp);  // final bearing
	
	SEXP ans; PROTECT(ans = allocVector(REALSXP, 3));
	REAL(ans)[0] = lat2 * (180 / PI);
	REAL(ans)[1] = lon1 * (180 / PI) + L * (180 / PI);
	REAL(ans)[2] = revAz * (180 / PI);
	
	UNPROTECT(1);
	return(ans);
}

