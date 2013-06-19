#include <sys/wait.h>
#include <unistd.h>
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

	int child;

	child = fork();
	if (child == 0) {
		char *child_args[] = {"util", "arg-to-util", NULL};
		execvp("util", child_args);
		_exit(1);
	}
	if (child != waitpid(child, NULL, 0)) {
		return 1;
	}

	char *new_args[] = {getenv("TESTUTIL"), "arg-to-test-util", NULL};
	execvp(new_args[0], new_args);
	return 1;
}
