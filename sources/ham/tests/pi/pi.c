// Approximates the value of pi using a Taylor series.
#include <stdio.h>
#include <stdlib.h>

#define VERSION "1.0.0"

// converges very slowly, (500K it gets 3.14159065)
double pi_compute_gregory_leibniz(int prec) {
	double value = 0.0;
	double denom = 1.0;
	double sig = 1.0;
	int i;
	for (i = 0; i < prec; ++i) {
		value += sig/denom;
		sig = -sig;
		denom += 2.0;
	}
	value *= 4.0;
	return value;
}

double pi_compute(int prec) {
    return pi_compute_gregory_leibniz(prec);
}

int main(int argc, char** argv) {
	int prec = 500000;
	double value = 0.0;

	printf("PI version %s (argc:%d,argv:%p)\n",VERSION,argc,argv);
	if (argc > 1) {
		prec = atol(argv[1]);
    }

	value = pi_compute(prec);

	printf("pi ~= %.8f, with %d iterations\n",value,prec);
	return 0;
}
