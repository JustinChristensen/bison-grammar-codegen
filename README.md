1. Lex vs straight parse? Bison uses a lexer first

grammatical classification
    symbol
        terminal, or token type: UPPERCASE conventionally
        nonterminal: non-UPPERCASE conventionally

three types of terminals:
    named token type: a C-like identifier, defined with %token
    character token type: 'a' '\n'
    literal string token: "foo" "bar", can be aliased to a named token with %token

Symbol names (named token type) can contain letters, underscores, periods, and non-initial digits and dashes. 

{...} braced code, or an action, used for determining the semantic value of a rule

%type defines a new type
%printer declares a block of c code to print the type
%define a variable to adjust bison's behavior

=========================

=========================
Goals:
1. Use the bison grammar scanner defined in src/scan-gram.l/c/h to tokenize the input
        - token types defined in parse-gram.h
        - should bison output a stream of tokens on stdout? or should this be a custom tokenizer
2. Use the bison grammar parser defined in src/parse-gram.y/c/h to parse the stream of input tokens
        - the token types and value types (declared using angle brackets, <uniqstr>) are generated
            with bison -d gram.y, see #1.
3. With the productions, rules, symbols (terminals and non terminals), and token types, provide a 
    DSL for mapping/sorting/partitioning/filtering and finally emitting (rendering) a particular node 
    to a template file.

Refs:
** 1. https://www.gnu.org/software/bison/manual/bison.html#Token-Type-Names
** 2. https://www.gnu.org/software/bison/manual/bison.html#https://www.gnu.org/software/bison/manual/bison.html

=========================

%token GRAM_EOF 0 "end of file"
%token STRING     "string"
%token PERCENT_TOKEN       "%token"
%token PERCENT_NTERM       "%nterm"
%token PERCENT_TYPE        "%type"
%token PERCENT_DESTRUCTOR  "%destructor"
%token PERCENT_PRINTER     "%printer"
%token PERCENT_LEFT        "%left"
%token PERCENT_RIGHT       "%right"
%token PERCENT_NONASSOC    "%nonassoc"
%token PERCENT_PRECEDENCE  "%precedence"
%token PERCENT_PREC          "%prec"
%token PERCENT_DPREC         "%dprec"
%token PERCENT_MERGE         "%merge"
%token
%token BRACED_CODE     "{...}"
%token BRACED_PREDICATE "%?{...}"
%token BRACKETED_ID    "[identifier]"
%token CHAR            "char"
%token COLON           ":"
%token EPILOGUE        "epilogue"
%token EQUAL           "="
%token ID              "identifier"
%token ID_COLON        "identifier:"
%token PERCENT_PERCENT "%%"
%token PIPE            "|"
%token PROLOGUE        "%{...%}"
%token SEMICOLON       ";"
%token TAG             "<tag>"
%token TAG_ANY         "<*>"
%token TAG_NONE        "<>"
%token <int> INT "integer"
%token <param_type> PERCENT_PARAM "%param";
%token PERCENT_UNION "%union";
%token PERCENT_EMPTY "%empty";

