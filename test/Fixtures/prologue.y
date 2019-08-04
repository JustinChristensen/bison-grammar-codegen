%{

const char *string = "it works \t on strings with braces { ";

/*

it works on comments with braces {{



}
*/
const char character = '{';

bool handles_blocks = false;

// each block should start a new nesting context }}}
if (!handles_blocks) {
    handles_blocks = true;
}

#define YYLLOC_DEFAULT(Current, Rhs, N) \
	do { \
		if ((N) > 0) \
			(Current) = (Rhs)[1]; \
		else \
			(Current) = (-1); \
	} while (0)

%}
