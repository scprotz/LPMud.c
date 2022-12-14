%{
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include "lint.h"
#include "config.h"
#include <unistd.h>
#define FUNC_SPEC		"list_funcs.txt"
#define FUNC_TOKENS 	"efun_tokens.y"
#define PRE_LANG        "prelang.y"
#define POST_LANG       "postlang.y"
#define THE_LANG        "lang.y"
#define MAKE_FUNC_BUFSIZ 		1024
#define NELEMS(arr) 	(sizeof arr / sizeof arr[0])

#define MAX_FUNC  	2048  /* If we need more than this we're in trouble! */
int num_buff;
/* For quick sort purposes : */
char *key[MAX_FUNC], *buf[MAX_FUNC], has_token[MAX_FUNC];

int min_arg = -1, limit_max = 0;

/*
 * arg_types is the types of all arguments. A 0 is used as a delimiter,
 * marking next argument. An argument can have several types.
 */
int arg_types[200], last_current_type;
/*
 * Store the types of the current efun. They will be copied into the
 * arg_types list if they were not already there (to save memory).
 */
int curr_arg_types[MAX_LOCAL], curr_arg_type_size;

void yyerror(const char *);
int yylex();
int yyparse();
int ungetc(int c, FILE *f);
char* type_str(int);
char* etype(int);
char* etype1(int);
char* ctype(int);
#ifndef toupper
int toupper(int);
#endif

void fatal(char* str)
{
    fprintf(stderr, "%s", str);
    exit(1);
}

%}
%union {
    int number;
    char *string;
}

%token ID

%token VOID INT STRING OBJECT MIXED UNKNOWN

%token DEFAULT

%type <number> type VOID INT STRING OBJECT MIXED UNKNOWN arg_list basic typel
%type <number> arg_type typel2

%type <string> ID optional_ID optional_default

%%

funcs: /* empty */ | funcs func ;

optional_ID: ID | /* empty */ { $$ = ""; } ;

optional_default: DEFAULT ':' ID { $$ = $3; } | /* empty */ { $$="0"; } ;

func: type ID optional_ID '(' arg_list optional_default ')' ';'
    {
	char buff[525];
	char f_name[500];
	int i;
	if (min_arg == -1)
	    min_arg = $5;
	if ($3[0] == '\0') {
	    int len;
	    if (strlen($2) + 1 + 2 > sizeof f_name)
		fatal("A local buffer was too small!(1)\n");
	    sprintf(f_name, "F_%s", $2);
	    len = strlen(f_name);
	    for (i=0; i < len; i++) {
		if (islower(f_name[i]))
		    f_name[i] = toupper(f_name[i]);
	    }
	    has_token[num_buff]=1;
	} else {
	    if (strlen($3) + 1 > sizeof f_name)
		fatal("A local buffer was too small(2)!\n");
	    strcpy(f_name, $3);
	    has_token[num_buff]=0;
	}
	for(i=0; i < last_current_type; i++) {
	    int j;
	    for (j = 0; j+i<last_current_type && j < curr_arg_type_size; j++)
	    {
		if (curr_arg_types[j] != arg_types[i+j])
		    break;
	    }
	    if (j == curr_arg_type_size)
		break;
	}
	if (i == last_current_type) {
	    int j;
	    for (j=0; j < curr_arg_type_size; j++) {
		arg_types[last_current_type++] = curr_arg_types[j];
		if (last_current_type == NELEMS(arg_types))
		    yyerror("Array 'arg_types' is too small");
	    }
	}
	sprintf(buff, "{\"%s\",%s,%d,%d,%s,%s,%s,%d,%s},\n",
		$2, f_name, min_arg, limit_max ? -1 : $5, ctype($1),
		etype(0), etype(1), i, $6);
        if (strlen(buff) > sizeof buff)
     	    fatal("Local buffer overwritten !\n");
        key[num_buff] = (char *) malloc(strlen($2) + 1);
	strcpy(key[num_buff], $2);
	buf[num_buff] = (char *) malloc(strlen(buff) + 1);
	strcpy(buf[num_buff], buff);
        num_buff++;
	min_arg = -1;
	limit_max = 0;
	curr_arg_type_size = 0;
    } ;

type: basic | basic '*' { $$ = $1 | 0x10000; };

basic: VOID | INT | STRING | MIXED | UNKNOWN | OBJECT ;

arg_list: /* empty */		{ $$ = 0; }
	| typel2			{ $$ = 1; if ($1) min_arg = 0; }
	| arg_list ',' typel2 	{ $$ = $1 + 1; if ($3) min_arg = $$ - 1; } ;

typel2: typel
    {
	$$ = $1;
	curr_arg_types[curr_arg_type_size++] = 0;
	if (curr_arg_type_size == NELEMS(curr_arg_types))
	    yyerror("Too many arguments");
    } ;

arg_type: type
    {
	if ($1 != VOID) {
	    curr_arg_types[curr_arg_type_size++] = $1;
	    if (curr_arg_type_size == NELEMS(curr_arg_types))
		yyerror("Too many arguments");
	}
	$$ = $1;
    } ;

typel: arg_type			{ $$ = ($1 == VOID && min_arg == -1); }
     | typel '|' arg_type 	{ $$ = (min_arg == -1 && ($1 || $3 == VOID));}
     | '.' '.' '.'		{ $$ = min_arg == -1 ; limit_max = 1; } ;

%%

struct type {
    char *name;
    int num;
} types[] = {
{ "void", VOID },
{ "int", INT },
{ "string", STRING },
{ "object", OBJECT },
{ "mixed", MIXED },
{ "unknown", UNKNOWN }
};

FILE *f;
int current_line = 1;

int main(int argc, char **argv)
{
    int i, fdr, fdw;
    char buffer[MAKE_FUNC_BUFSIZ + 1];

    if ((f = fopen(FUNC_SPEC, "r")) == NULL) { 
	perror(FUNC_SPEC);
	exit(1);
    }
    yyparse();
    /* Now sort the main_list */
    for (i = 0; i < num_buff; i++) {
       int j;
       for (j = 0; j < i; j++)
	   if (strcmp(key[i], key[j]) < 0) {
	      char *tmp;
	      int tmpi;
	      tmp = key[i]; key[i] = key[j]; key[j] = tmp;
	      tmp = buf[i]; buf[i] = buf[j]; buf[j] = tmp;
	      tmpi = has_token[i];
	      has_token[i] = has_token[j]; has_token[j] = tmpi;
           }
    }
    /* Now display it... */
    printf("struct keyword predefs[] = ");
    printf("{\n");
    for (i = 0; i < num_buff; i++)
       printf("%s", buf[i]);
    printf("\n};\nint efun_arg_types[] = {\n");
    for (i=0; i < last_current_type; i++) {
	if (arg_types[i] == 0)
	    printf("0,\n");
	else
	    printf("%s,", ctype(arg_types[i]));
    }
    printf("};\n");
    pclose(f);
    /*
     * Write all the tokens out.  Do this by copying the
     * pre-include portion of lang.y to lang.y, appending
     * this information, then appending the post-include
     * portion of lang.y.  It's done this way because I don't
     * know how to get YACC to #include %token files.  *grin*
     */
    if ((fdr = open(PRE_LANG, O_RDONLY)) < 0) {
       perror(PRE_LANG);
       exit(1);
    }
    unlink(THE_LANG);
    if ((fdw = open(THE_LANG, O_CREAT | O_WRONLY, 0444)) < 0) {
       perror(THE_LANG);
       exit(1);
    }
    while ((i = read(fdr, buffer, MAKE_FUNC_BUFSIZ)))
       write(fdw, buffer, i);
    close(fdr);
    for (i = 0; i < num_buff; i++) {
       if (has_token[i]) {
          char *str;   /* It's okay to mung key[*] now */
          for (str = key[i]; *str; str++)
   	     if (islower(*str)) *str = toupper(*str);
          sprintf(buffer, "%%token F_%s\n", key[i]);
          write(fdw, buffer, strlen(buffer));
       }
    }
    if ((fdr = open(POST_LANG, O_RDONLY)) < 0) {
       perror(POST_LANG);
       exit(1);
    }
    while ((i = read(fdr, buffer, MAKE_FUNC_BUFSIZ)))
       write(fdw, buffer, i);
    close(fdr), close(fdw);
    return 0;
}

void yyerror(const char *str)
{
    fprintf(stderr, "%s:%d: %s\n", FUNC_SPEC, current_line, str);
    exit(1);
}

int ident(int c)
{
    char buff[100];
    int len, i;

    for (len=0; isalnum(c) || c == '_'; c = getc(f)) {
	buff[len++] = c;
	if (len + 1 >= sizeof buff)
	    fatal("Local buffer in ident() too small!\n");
	if (len == sizeof buff - 1) {
	    yyerror("Too long indentifier");
	    break;
	}
    }
    (void)ungetc(c, f);
    buff[len] = '\0';
    for (i=0; i < NELEMS(types); i++) {
	if (strcmp(buff, types[i].name) == 0) {
	    yylval.number = types[i].num;
	    return types[i].num;
	}
    }
    if (strcmp(buff, "default") == 0)
	return DEFAULT;
    yylval.string = (char*)malloc(strlen(buff)+1);
    strcpy(yylval.string, buff);
    return ID;
}

char *type_str(int n)
{
    int i, type = n & 0xffff;

    for (i=0; i < NELEMS(types); i++) {
	if (types[i].num == type) {
	    if (n & 0x10000) {
		static char buff[100];
		if (strlen(types[i].name) + 3 > sizeof buff)
		    fatal("Local buffer too small in type_str()!\n");
		sprintf(buff, "%s *", types[i].name);
		return buff;
	    }
	    return types[i].name;
	}
    }
    return "What ?";
}

int yylex1() {
    register int c;

    for(;;) {
		switch(c = getc(f)) {
	case ' ':
	case '\t':
		continue;
	case '#':
	{
		int line;
		char file[2048]; /* does any operating system support
				longer pathnames? */
		if ( fscanf(f,"%d \"%s\"",&line,file ) == 2 )
		current_line = line;
		while(c != '\n' && c != EOF)
		c = getc(f);
		current_line++;
		continue;
	}
	case '\n':
		current_line++;
		continue;
	case EOF:
		return -1;
	default:
		if (isalpha(c))
		return ident(c);
		return c;
	}
    }
}

int yylex() {
    return yylex1();
}

char *etype1(int n)
{
    if (n & 0x10000)
	return "T_POINTER";
    switch(n) {
    case INT:
	return "T_NUMBER";
    case OBJECT:
	return "T_OBJECT";
    case STRING:
	return "T_STRING";
    case MIXED:
	return "0";	/* 0 means any type */
    default:
	yyerror("Illegal type for argument");
    }
    return "What ?";
}

char *etype(int n)
{
    int i;
    int local_size = 100;
    char *buff = (char*)malloc(local_size);

    for (i=0; i < curr_arg_type_size; i++) {
	if (n == 0)
	    break;
	if (curr_arg_types[i] == 0)
	    n--;
    }
    if (i == curr_arg_type_size)
	return "0";
    buff[0] = '\0';
    for(; curr_arg_types[i] != 0; i++) {
	char *p;
	if (curr_arg_types[i] == VOID)
	    continue;
	if (buff[0] != '\0')
	    strcat(buff, "|");
	p = etype1(curr_arg_types[i]);
	/*
	 * The number 2 below is to include the zero-byte and the next
	 * '|' (which may not come).
	 */
	if (strlen(p) + strlen(buff) + 2 > local_size) {
	    fprintf(stderr, "Buffer overflow!\n");
	    exit(1);
	}
	strcat(buff, etype1(curr_arg_types[i]));
    }
    return buff;
}

char *ctype( int n)
{
    static char buff[100];	/* 100 is such a comfortable size :-) */
    char *p;

    if (n & 0x10000)
	strcpy(buff, "TYPE_MOD_POINTER|");
    else
	buff[0] = '\0';
    n &= ~0x10000;
    switch(n) {
    case VOID: p = "TYPE_VOID"; break;
    case STRING: p = "TYPE_STRING"; break;
    case INT: p = "TYPE_NUMBER"; break;
    case OBJECT: p = "TYPE_OBJECT"; break;
    case MIXED: p = "TYPE_ANY"; break;
    case UNKNOWN: p = "TYPE_UNKNOWN"; break;
    default: yyerror("Bad type!");
    }
    strcat(buff, p);
    if (strlen(buff) + 1 > sizeof buff)
	fatal("Local buffer overwritten in ctype()");
    return buff;
}
