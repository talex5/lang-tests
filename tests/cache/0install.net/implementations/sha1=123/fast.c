#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
	int i;

	for (i = 0; i < argc; i++) {
		char *last_slash = strrchr(argv[i], '/');
		puts(last_slash == NULL ? argv[i] : last_slash);
	}

	fflush(stdout);

	if (system("util arg-to-util") || system("$TESTUTIL arg-to-test-util")) {
		fprintf(stderr, "util failed");
		return 1;
	}

	return 0;
}
