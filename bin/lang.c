/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "lang.y"

# line 3 "prelang.y"
/* The above line is to give proper line number references. Please mail me
 * if your compiler complains about it.
 */
/*
 * This is the grammar definition of LPC. The token table is built
 * automatically by make_func. The lang.y is constructed from this file,
 * the generated token list and post_lang.y. The reason of this is that there
 * is no #include-statment that yacc recognizes.
 */
#include <string.h>
#include <stdio.h>
#include <memory.h>
#if defined(sun)
#include <alloca.h>
#endif

#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "exec.h"
#include "config.h"
#include "instrs.h"
#include "incralloc.h"
#include "switch.h"
#include <stdlib.h>

#if defined(__GNUC__) && !defined(lint) && !defined(DEBUG)
#define INLINE inline
#else
#define INLINE
#endif

#define YYMAXDEPTH	600

/* NUMPAREAS areas are saved with the program code after compilation.
 */
#define A_PROGRAM		0
#define A_FUNCTIONS		1
#define A_STRINGS		2
#define A_VARIABLES		3
#define A_LINENUMBERS		4
#define A_INHERITS		5
#define A_ARGUMENT_TYPES	6
#define A_ARGUMENT_INDEX	7
#define NUMPAREAS		8
#define A_CASE_NUMBERS		8
#define A_CASE_STRINGS		9
#define A_CASE_LABELS	       10
#define NUMAREAS	       11

#define BREAK_ON_STACK		0x40000
#define BREAK_FROM_CASE		0x80000

/* make shure that this struct has a size that is a power of two */
struct case_heap_entry { int key; short addr; short line; };
#define CASE_HEAP_ENTRY_ALIGN(offset) offset &= -sizeof(struct case_heap_entry)

static struct mem_block mem_block[NUMAREAS];

/*
 * Some good macros to have.
 */

#define BASIC_TYPE(e,t) ((e) == TYPE_ANY ||\
			 (e) == (t) ||\
			 (t) == TYPE_ANY)

#define TYPE(e,t) (BASIC_TYPE((e) & TYPE_MOD_MASK, (t) & TYPE_MOD_MASK) ||\
		   (((e) & TYPE_MOD_POINTER) && ((t) & TYPE_MOD_POINTER) &&\
		    BASIC_TYPE((e) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER),\
			       (t) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER))))

#define FUNCTION(n) ((struct function *)mem_block[A_FUNCTIONS].block + (n))
#define VARIABLE(n) ((struct variable *)mem_block[A_VARIABLES].block + (n))

#define align(x) (((x) + 3) & ~3)

/*
 * If the type of the function is given, then strict types are
 * checked and required.
 */
static int exact_types;
extern int pragma_strict_types;	/* Maintained by lex.c */
extern int pragma_save_types;	/* Also maintained by lex.c */
int approved_object;		/* How I hate all these global variables */

extern int total_num_prog_blocks, total_prog_block_size;

extern int num_parse_error;
extern int d_flag;
static int heart_beat;		/* Number of the heart beat function */

static int current_break_address;
static int current_continue_address;
static int current_case_number_heap;
static int current_case_string_heap;
#define SOME_NUMERIC_CASE_LABELS 0x40000
#define NO_STRING_CASE_LABELS    0x80000
static int zero_case_label;
static int current_type;

static int last_push_indexed;
static int last_push_local;
static int last_push_identifier;

/*
 * There is always function starting at address 0, which will execute
 * the initialization code. This code is spread all over the program,
 * with jumps to next initializer. The next variable keeps track of
 * the previous jump. After the last initializer, the jump will be changed
 * into a return(0) statement instead.
 *
 * A function named '__INIT' will be defined, which will contain the
 * initialization code. If there was no initialization code, then the
 * function will not be defined. That is the usage of the
 * first_last_initializer_end variable.
 *
 * When inheriting from another object, a call will automatically be made
 * to call __INIT in that code from the current __INIT.
 */
static int last_initializer_end;
static int first_last_initializer_end;

static struct program NULL_program; /* marion - clean neat empty struct */

void epilog();
static int check_declared PROT((char *str));
static void prolog();
static char *get_two_types PROT((int type1, int type2));
void free_all_local_names(),
    add_local_name PROT((char *, int)), smart_log PROT((char *, int, char *));
extern int yylex();
static int verify_declared PROT((char *));
static void copy_variables();
static int copy_functions PROT((struct program *, int type));
void type_error PROT((char *, int));

char *xalloc(), *string_copy();

extern int current_line;
/*
 * 'inherit_file' is used as a flag. If it is set to a string
 * after yyparse(), this string should be loaded as an object,
 * and the original object must be loaded again.
 */
extern char *current_file, *inherit_file;

/*
 * The names and types of arguments and auto variables.
 */
char *local_names[MAX_LOCAL];
unsigned short type_of_locals[MAX_LOCAL];
int current_number_of_locals = 0;
int current_break_stack_need = 0  ,max_break_stack_need = 0;

/*
 * The types of arguments when calling functions must be saved,
 * to be used afterwards for checking. And because function calls
 * can be done as an argument to a function calls,
 * a stack of argument types is needed. This stack does not need to
 * be freed between compilations, but will be reused.
 */
static struct mem_block type_of_arguments;

struct program *prog;	/* Is returned to the caller of yyparse */

/*
 * Compare two types, and return true if they are compatible.
 */
static int compatible_types(t1, t2)
    int t1, t2;
{
    if (t1 == TYPE_UNKNOWN || t2 == TYPE_UNKNOWN)
	return 0;
    if (t1 == t2)
	return 1;
    if (t1 == TYPE_ANY || t2 == TYPE_ANY)
	return 1;
    if ((t1 & TYPE_MOD_POINTER) && (t2 & TYPE_MOD_POINTER)) {
	if ((t1 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER) ||
	    (t2 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER))
	    return 1;
    }
    return 0;
}

/*
 * Add another argument type to the argument type stack
 */
INLINE
static void add_arg_type(type)
    unsigned short type;
{
    struct mem_block *mbp = &type_of_arguments;
    while (mbp->current_size + sizeof type > mbp->max_size) {
	mbp->max_size <<= 1;
	mbp->block = realloc((char *)mbp->block, mbp->max_size);
    }
    memcpy(mbp->block + mbp->current_size, &type, sizeof type);
    mbp->current_size += sizeof type;
}

/*
 * Pop the argument type stack 'n' elements.
 */
INLINE
static void pop_arg_stack(n)
    int n;
{
    type_of_arguments.current_size -= sizeof (unsigned short) * n;
}

/*
 * Get type of argument number 'arg', where there are
 * 'n' arguments in total in this function call. Argument
 * 0 is the first argument.
 */
INLINE
int get_argument_type(arg, n)
    int arg, n;
{
    return
	((unsigned short *)
	 (type_of_arguments.block + type_of_arguments.current_size))[arg - n];
}

INLINE
static void add_to_mem_block(n, data, size)
    int n, size;
    char *data;
{
    struct mem_block *mbp = &mem_block[n];
    while (mbp->current_size + size > mbp->max_size) {
	mbp->max_size <<= 1;
	mbp->block = realloc((char *)mbp->block, mbp->max_size);
    }
    memcpy(mbp->block + mbp->current_size, data, size);
    mbp->current_size += size;
}

static void ins_byte(b)
    char b;
{
    add_to_mem_block(A_PROGRAM, &b, 1);
}

/*
 * Store a 2 byte number. It is stored in such a way as to be sure
 * that correct byte order is used, regardless of machine architecture.
 * Also beware that some machines can't write a word to odd addresses.
 */
static void ins_short(l)
    short l;
{
    add_to_mem_block(A_PROGRAM, (char *)&l + 0, 1);
    add_to_mem_block(A_PROGRAM, (char *)&l + 1, 1);
}

static void upd_short(offset, l)
    int offset;
    short l;
{
    mem_block[A_PROGRAM].block[offset + 0] = ((char *)&l)[0];
    mem_block[A_PROGRAM].block[offset + 1] = ((char *)&l)[1];
}

static short read_short(offset)
    int offset;
{
    short l;

    ((char *)&l)[0] = mem_block[A_PROGRAM].block[offset + 0];
    ((char *)&l)[1] = mem_block[A_PROGRAM].block[offset + 1];
    return l;
}

/*
 * Store a 4 byte number. It is stored in such a way as to be sure
 * that correct byte order is used, regardless of machine architecture.
 */
static void ins_long(l)
    int l;
{
    add_to_mem_block(A_PROGRAM, (char *)&l+0, 1);
    add_to_mem_block(A_PROGRAM, (char *)&l+1, 1);
    add_to_mem_block(A_PROGRAM, (char *)&l+2, 1);
    add_to_mem_block(A_PROGRAM, (char *)&l+3, 1);
}

static void ins_f_byte(b)
    unsigned int b;
{
    ins_byte((char)(b - F_OFFSET));
}

/*
 * Return the index of the function found, otherwise -1.
 */
static int defined_function(s)
    char *s;
{
    int offset;
    struct function *funp;

    for (offset = 0; offset < mem_block[A_FUNCTIONS].current_size;
	 offset += sizeof (struct function)) {
	funp = (struct function *)&mem_block[A_FUNCTIONS].block[offset];
	if (funp->flags & NAME_HIDDEN)
	    continue;
        if (strcmp(funp->name, s) == 0)
	    return offset / sizeof (struct function);
    }
    return -1;
}

/*
 * A mechanism to remember addresses on a stack. The size of the stack is
 * defined in config.h.
 */
static int comp_stackp;
static int comp_stack[COMPILER_STACK_SIZE];

static void push_address() {
    if (comp_stackp >= COMPILER_STACK_SIZE) {
	yyerror("Compiler stack overflow");
	comp_stackp++;
	return;
    }
    comp_stack[comp_stackp++] = mem_block[A_PROGRAM].current_size;
}

static void push_explicit(address)
    int address;
{
    if (comp_stackp >= COMPILER_STACK_SIZE) {
	yyerror("Compiler stack overflow");
	comp_stackp++;
	return;
    }
    comp_stack[comp_stackp++] = address;
}

static int pop_address() {
    if (comp_stackp == 0)
	fatal("Compiler stack underflow.\n");
    if (comp_stackp > COMPILER_STACK_SIZE) {
	--comp_stackp;
	return 0;
    }
    return comp_stack[--comp_stackp];
}

/*
 * Patch a function definition of an inherited function, to what it really
 * should be.
 * The name of the function can be one of:
 *    object::name
 *    ::name
 *    name
 * Where 'object' is the name of the superclass.
 */
static void find_inherited(funp)
    struct function *funp;
{
    int i;
    struct inherit *ip;
    int num_inherits, super_length;
    char *real_name, *super_name = 0, *p;

    real_name = funp->name;
    if (real_name[0] == ':')
	real_name = real_name + 2;	/* There will be exactly two ':' */
    else if ((p = strchr(real_name, ':'))) {
	real_name = p+2;
	super_name = funp->name;
	super_length = real_name - super_name - 2;
    }
    num_inherits = mem_block[A_INHERITS].current_size /
	sizeof (struct inherit);
    ip = (struct inherit *)mem_block[A_INHERITS].block;
    for (; num_inherits > 0; ip++, num_inherits--) {
	if (super_name) {
	    int l = strlen(ip->prog->name);	/* Including .c */
	    if (l - 2 < super_length)
		continue;
	    if (strncmp(super_name, ip->prog->name + l - 2 - super_length,
			super_length) != 0)
		continue;
	}
	for (i=0; i < ip->prog->num_functions; i++) {
	    if (ip->prog->functions[i].flags & (NAME_UNDEFINED|NAME_HIDDEN))
		continue;
	    if (strcmp(ip->prog->functions[i].name, real_name) != 0)
		continue;
	    funp->offset = ip - (struct inherit *)mem_block[A_INHERITS].block;
	    funp->flags = ip->prog->functions[i].flags | NAME_INHERITED;
	    funp->num_local = ip->prog->functions[i].num_local;
	    funp->num_arg = ip->prog->functions[i].num_arg;
	    funp->type = ip->prog->functions[i].type;
	    funp->function_index_offset = i;
	    return;
	}
    }
    return;
}

/*
 * Define a new function. Note that this function is called at least twice
 * for alll function definitions. First as a prototype, then as the real
 * function. Thus, there are tests to avoid generating error messages more
 * than once by looking at (flags & NAME_PROTOTYPE).
 */
static int define_new_function(name, num_arg, num_local, offset, flags, type)
    char *name;
    int num_arg, num_local;
    int offset, flags, type;
{
    int num;
    struct function fun;
    unsigned short argument_start_index;

    num = defined_function(name);
    if (num >= 0) {
	struct function *funp;

	/*
	 * The function was already defined. It may be one of several reasons:
	 *
	 * 1.	There has been a prototype.
	 * 2.	There was the same function defined by inheritance.
	 * 3.	This function has been called, but not yet defined.
	 * 4.	The function is doubly defined.
	 * 5.	A "late" prototype has been encountered.
	 */
	funp = (struct function *)(mem_block[A_FUNCTIONS].block) + num;
	if (!(funp->flags & NAME_UNDEFINED) &&
	    !(flags & NAME_PROTOTYPE) &&
	    !(funp->flags & NAME_INHERITED))
	{
	    char buff[500];
	    sprintf(buff, "Redeclaration of function %s.", name);
	    yyerror(buff);
	    return num;
	}
	/*
	 * It was either an undefined but used funtion, or an inherited
	 * function. In both cases, we now consider this to be THE new
	 * definition. It might also have been a prototype to an already
	 * defined function.
	 *
	 * Check arguments only when types are supposed to be tested,
	 * and if this function really has been defined already.
	 *
	 * 'nomask' functions may not be redefined.
	 */
	if ((funp->type & TYPE_MOD_NO_MASK) &&
	    !(funp->flags & NAME_PROTOTYPE) &&
	    !(flags & NAME_PROTOTYPE))
	{
	    char *p = (char *)alloca(80 + strlen(name));
	    sprintf(p, "Illegal to redefine 'nomask' function \"%s\"",name);
	    yyerror(p);
	}
	if (exact_types && funp->type != TYPE_UNKNOWN) {
	    int i;
	    if (funp->num_arg != num_arg && !(funp->type & TYPE_MOD_VARARGS))
		yyerror("Incorrect number of arguments.");
	    else if (!(funp->flags & NAME_STRICT_TYPES))
		yyerror("Called function not compiled with type testing.");
	    else {
		/* Now check that argument types wasn't changed. */
		for (i=0; i < num_arg; i++) {
		}
	    }
	}
	/* If it was yet another prototype, then simply return. */
	if (flags & NAME_PROTOTYPE)
	    return num;
	funp->num_arg = num_arg;
	funp->num_local = num_local;
	funp->flags = flags;
	funp->offset = offset;
	funp->function_index_offset = 0;
	funp->type = type;
	if (exact_types)
	    funp->flags |= NAME_STRICT_TYPES;
	return num;
    }
    if (strcmp(name, "heart_beat") == 0)
	heart_beat = mem_block[A_FUNCTIONS].current_size /
	    sizeof (struct function);
    fun.name = make_shared_string(name);
    fun.offset = offset;
    fun.flags = flags;
    fun.num_arg = num_arg;
    fun.num_local = num_local;
    fun.function_index_offset = 0;
    fun.type = type;
    if (exact_types)
	fun.flags |= NAME_STRICT_TYPES;
    num = mem_block[A_FUNCTIONS].current_size / sizeof fun;
    /* Number of local variables will be updated later */
    add_to_mem_block(A_FUNCTIONS, (char *)&fun, sizeof fun);

    if (exact_types == 0 || num_arg == 0) {
	argument_start_index = INDEX_START_NONE;
    } else {
	int i;

	/*
	 * Save the start of argument types.
	 */
	argument_start_index =
	    mem_block[A_ARGUMENT_TYPES].current_size /
		sizeof (unsigned short);
	for (i=0; i < num_arg; i++) {
	    add_to_mem_block(A_ARGUMENT_TYPES, &type_of_locals[i],
			     sizeof type_of_locals[i]);
	}
    }
    add_to_mem_block(A_ARGUMENT_INDEX, &argument_start_index,
		     sizeof argument_start_index);
    return num;
}

static void define_variable(name, type, flags)
    char *name;
    int type;
    int flags;
{
    struct variable dummy;
    int n;

    n = check_declared(name);
    if (n != -1 && (VARIABLE(n)->type & TYPE_MOD_NO_MASK)) {
	char *p = (char *)alloca(80 + strlen(name));
	sprintf(p, "Illegal to redefine 'nomask' variable \"%s\"", name);
	yyerror(p);
    }
    dummy.name = make_shared_string(name);
    dummy.type = type;
    dummy.flags = flags;
    add_to_mem_block(A_VARIABLES, (char *)&dummy, sizeof dummy);
}

short store_prog_string(str)
    char *str;
{
    short i;
    char **p;

    p = (char **) mem_block[A_STRINGS].block;
    str = make_shared_string(str);
    for (i=mem_block[A_STRINGS].current_size / sizeof str -1; i>=0; --i)
	if (p[i] == str)  {
	    free_string(str); /* Needed as string is only free'ed once. */
	    return i;
	}

    add_to_mem_block(A_STRINGS, &str, sizeof str);
    return mem_block[A_STRINGS].current_size / sizeof str - 1;
}

void add_to_case_heap(block_index,entry)
    int block_index;
    struct case_heap_entry *entry;
{
    char *heap_start;
    int offset,parent;
    int current_heap;

    if ( block_index == A_CASE_NUMBERS )
        current_heap = current_case_number_heap;
    else
        current_heap = current_case_string_heap;
    offset = mem_block[block_index].current_size - current_heap;
    add_to_mem_block(block_index, (char*)entry, sizeof(*entry) );
    heap_start = mem_block[block_index].block + current_heap;
    for ( ; offset; offset = parent ) {
        parent = ( offset - sizeof(struct case_heap_entry) ) >> 1 ;
        CASE_HEAP_ENTRY_ALIGN(parent);
        if ( ((struct case_heap_entry*)(heap_start+offset))->key <
             ((struct case_heap_entry*)(heap_start+parent))->key )
        {
            *(struct case_heap_entry*)(heap_start+offset) =
            *(struct case_heap_entry*)(heap_start+parent);
            *(struct case_heap_entry*)(heap_start+parent) = *entry;
        }
    }
}

/*
 * Arrange a jump to the current position for the initialization code
 * to continue.
 */
static void transfer_init_control() {
    if (mem_block[A_PROGRAM].current_size - 2 == last_initializer_end)
	mem_block[A_PROGRAM].current_size -= 3;
    else {
	/*
	 * Change the address of the last jump after the last
	 * initializer to this point.
	 */
	upd_short(last_initializer_end,
		  mem_block[A_PROGRAM].current_size);
    }
}

void add_new_init_jump();

#line 683 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    F_JUMP = 258,
    F_JUMP_WHEN_ZERO = 259,
    F_JUMP_WHEN_NON_ZERO = 260,
    F_POP_VALUE = 261,
    F_DUP = 262,
    F_STORE = 263,
    F_CALL_FUNCTION_BY_ADDRESS = 264,
    F_PUSH_IDENTIFIER_LVALUE = 265,
    F_PUSH_LOCAL_VARIABLE_LVALUE = 266,
    F_PUSH_INDEXED_LVALUE = 267,
    F_INDIRECT = 268,
    F_INDEX = 269,
    F_CONST0 = 270,
    F_CONST1 = 271,
    F_IF = 272,
    F_IDENTIFIER = 273,
    F_LAND = 274,
    F_LOR = 275,
    F_STATUS = 276,
    F_RETURN = 277,
    F_STRING = 278,
    F_INC = 279,
    F_DEC = 280,
    F_POST_INC = 281,
    F_POST_DEC = 282,
    F_COMMA = 283,
    F_NUMBER = 284,
    F_ASSIGN = 285,
    F_INT = 286,
    F_ADD = 287,
    F_SUBTRACT = 288,
    F_MULTIPLY = 289,
    F_DIVIDE = 290,
    F_LT = 291,
    F_GT = 292,
    F_EQ = 293,
    F_GE = 294,
    F_LE = 295,
    F_NE = 296,
    F_ADD_EQ = 297,
    F_SUB_EQ = 298,
    F_DIV_EQ = 299,
    F_MULT_EQ = 300,
    F_NEGATE = 301,
    F_SUBSCRIPT = 302,
    F_WHILE = 303,
    F_BREAK = 304,
    F_DO = 305,
    F_FOR = 306,
    F_SWITCH = 307,
    F_SSCANF = 308,
    F_PARSE_COMMAND = 309,
    F_STRING_DECL = 310,
    F_LOCAL_NAME = 311,
    F_ELSE = 312,
    F_DESCRIBE = 313,
    F_CONTINUE = 314,
    F_MOD = 315,
    F_MOD_EQ = 316,
    F_INHERIT = 317,
    F_COLON_COLON = 318,
    F_STATIC = 319,
    F_ARROW = 320,
    F_AGGREGATE = 321,
    F_COMPL = 322,
    F_AND = 323,
    F_AND_EQ = 324,
    F_OR = 325,
    F_OR_EQ = 326,
    F_XOR = 327,
    F_XOR_EQ = 328,
    F_LSH = 329,
    F_LSH_EQ = 330,
    F_RSH = 331,
    F_RSH_EQ = 332,
    F_CATCH = 333,
    F_OBJECT = 334,
    F_VOID = 335,
    F_MIXED = 336,
    F_PRIVATE = 337,
    F_NO_MASK = 338,
    F_NOT = 339,
    F_PROTECTED = 340,
    F_PUBLIC = 341,
    F_VARARGS = 342,
    F_ADD_ACTION = 343,
    F_ADD_VERB = 344,
    F_ADD_WORTH = 345,
    F_ADD_XVERB = 346,
    F_ALL_INVENTORY = 347,
    F_ALLOCATE = 348,
    F_ASSOC = 349,
    F_BREAK_POINT = 350,
    F_CALL_OTHER = 351,
    F_CALL_OUT = 352,
    F_CALL_OUT_INFO = 353,
    F_CAPITALIZE = 354,
    F_CAT = 355,
    F_CINDENT = 356,
    F_CLEAR_BIT = 357,
    F_CLONE_OBJECT = 358,
    F_COMMAND = 359,
    F_CREATE_WIZARD = 360,
    F_CREATOR = 361,
    F_CRYPT = 362,
    F_CTIME = 363,
    F_DEBUG_INFO = 364,
    F_DEEP_INVENTORY = 365,
    F_DESTRUCT = 366,
    F_DISABLE_COMMANDS = 367,
    F_ED = 368,
    F_ENABLE_COMMANDS = 369,
    F_ENVIRONMENT = 370,
    F_EXEC = 371,
    F_EXPLODE = 372,
    F_EXTRACT = 373,
    F_FILE_NAME = 374,
    F_FILE_SIZE = 375,
    F_FILTER_ARRAY = 376,
    F_FIND_CALL_OUT = 377,
    F_FIND_LIVING = 378,
    F_FIND_OBJECT = 379,
    F_FIND_PLAYER = 380,
    F_FIRST_INVENTORY = 381,
    F_FUNCTION_EXISTS = 382,
    F_GET_DIR = 383,
    F_IMPLODE = 384,
    F_INHERIT_LIST = 385,
    F_INPUT_TO = 386,
    F_INSERT_ALIST = 387,
    F_INTERACTIVE = 388,
    F_INTERSECT_ALIST = 389,
    F_INTP = 390,
    F_LIVING = 391,
    F_LOCALCMD = 392,
    F_LOG_FILE = 393,
    F_LOWER_CASE = 394,
    F_MAP_ARRAY = 395,
    F_MEMBER_ARRAY = 396,
    F_MKDIR = 397,
    F_MOVE_OBJECT = 398,
    F_NEXT_INVENTORY = 399,
    F_NOTIFY_FAIL = 400,
    F_OBJECTP = 401,
    F_ORDER_ALIST = 402,
    F_POINTERP = 403,
    F_PRESENT = 404,
    F_PREVIOUS_OBJECT = 405,
    F_PROCESS_STRING = 406,
    F_QUERY_HOST_NAME = 407,
    F_QUERY_IDLE = 408,
    F_QUERY_IP_NAME = 409,
    F_QUERY_IP_NUMBER = 410,
    F_QUERY_LOAD_AVERAGE = 411,
    F_QUERY_SNOOP = 412,
    F_QUERY_VERB = 413,
    F_RANDOM = 414,
    F_READ_BYTES = 415,
    F_READ_FILE = 416,
    F_REGEXP = 417,
    F_REMOVE_CALL_OUT = 418,
    F_RESTORE_OBJECT = 419,
    F_RM = 420,
    F_RMDIR = 421,
    F_SAVE_OBJECT = 422,
    F_SAY = 423,
    F_SET_BIT = 424,
    F_SET_HEART_BEAT = 425,
    F_SET_LIGHT = 426,
    F_SET_LIVING_NAME = 427,
    F_SHADOW = 428,
    F_SHOUT = 429,
    F_SHUTDOWN = 430,
    F_SIZEOF = 431,
    F_SNOOP = 432,
    F_SORT_ARRAY = 433,
    F_STRINGP = 434,
    F_STRLEN = 435,
    F_SWAP = 436,
    F_TAIL = 437,
    F_TELL_OBJECT = 438,
    F_TELL_ROOM = 439,
    F_TEST_BIT = 440,
    F_THIS_OBJECT = 441,
    F_THIS_PLAYER = 442,
    F_THROW = 443,
    F_TIME = 444,
    F_TRACE = 445,
    F_TRACEPREFIX = 446,
    F_TRANSFER = 447,
    F_UNIQUE_ARRAY = 448,
    F_USERS = 449,
    F_VERSION = 450,
    F_WIZLIST = 451,
    F_WRITE = 452,
    F_WRITE_BYTES = 453,
    F_WRITE_FILE = 454,
    F_CASE = 455,
    F_DEFAULT = 456,
    F_RANGE = 457
  };
#endif
/* Tokens.  */
#define F_JUMP 258
#define F_JUMP_WHEN_ZERO 259
#define F_JUMP_WHEN_NON_ZERO 260
#define F_POP_VALUE 261
#define F_DUP 262
#define F_STORE 263
#define F_CALL_FUNCTION_BY_ADDRESS 264
#define F_PUSH_IDENTIFIER_LVALUE 265
#define F_PUSH_LOCAL_VARIABLE_LVALUE 266
#define F_PUSH_INDEXED_LVALUE 267
#define F_INDIRECT 268
#define F_INDEX 269
#define F_CONST0 270
#define F_CONST1 271
#define F_IF 272
#define F_IDENTIFIER 273
#define F_LAND 274
#define F_LOR 275
#define F_STATUS 276
#define F_RETURN 277
#define F_STRING 278
#define F_INC 279
#define F_DEC 280
#define F_POST_INC 281
#define F_POST_DEC 282
#define F_COMMA 283
#define F_NUMBER 284
#define F_ASSIGN 285
#define F_INT 286
#define F_ADD 287
#define F_SUBTRACT 288
#define F_MULTIPLY 289
#define F_DIVIDE 290
#define F_LT 291
#define F_GT 292
#define F_EQ 293
#define F_GE 294
#define F_LE 295
#define F_NE 296
#define F_ADD_EQ 297
#define F_SUB_EQ 298
#define F_DIV_EQ 299
#define F_MULT_EQ 300
#define F_NEGATE 301
#define F_SUBSCRIPT 302
#define F_WHILE 303
#define F_BREAK 304
#define F_DO 305
#define F_FOR 306
#define F_SWITCH 307
#define F_SSCANF 308
#define F_PARSE_COMMAND 309
#define F_STRING_DECL 310
#define F_LOCAL_NAME 311
#define F_ELSE 312
#define F_DESCRIBE 313
#define F_CONTINUE 314
#define F_MOD 315
#define F_MOD_EQ 316
#define F_INHERIT 317
#define F_COLON_COLON 318
#define F_STATIC 319
#define F_ARROW 320
#define F_AGGREGATE 321
#define F_COMPL 322
#define F_AND 323
#define F_AND_EQ 324
#define F_OR 325
#define F_OR_EQ 326
#define F_XOR 327
#define F_XOR_EQ 328
#define F_LSH 329
#define F_LSH_EQ 330
#define F_RSH 331
#define F_RSH_EQ 332
#define F_CATCH 333
#define F_OBJECT 334
#define F_VOID 335
#define F_MIXED 336
#define F_PRIVATE 337
#define F_NO_MASK 338
#define F_NOT 339
#define F_PROTECTED 340
#define F_PUBLIC 341
#define F_VARARGS 342
#define F_ADD_ACTION 343
#define F_ADD_VERB 344
#define F_ADD_WORTH 345
#define F_ADD_XVERB 346
#define F_ALL_INVENTORY 347
#define F_ALLOCATE 348
#define F_ASSOC 349
#define F_BREAK_POINT 350
#define F_CALL_OTHER 351
#define F_CALL_OUT 352
#define F_CALL_OUT_INFO 353
#define F_CAPITALIZE 354
#define F_CAT 355
#define F_CINDENT 356
#define F_CLEAR_BIT 357
#define F_CLONE_OBJECT 358
#define F_COMMAND 359
#define F_CREATE_WIZARD 360
#define F_CREATOR 361
#define F_CRYPT 362
#define F_CTIME 363
#define F_DEBUG_INFO 364
#define F_DEEP_INVENTORY 365
#define F_DESTRUCT 366
#define F_DISABLE_COMMANDS 367
#define F_ED 368
#define F_ENABLE_COMMANDS 369
#define F_ENVIRONMENT 370
#define F_EXEC 371
#define F_EXPLODE 372
#define F_EXTRACT 373
#define F_FILE_NAME 374
#define F_FILE_SIZE 375
#define F_FILTER_ARRAY 376
#define F_FIND_CALL_OUT 377
#define F_FIND_LIVING 378
#define F_FIND_OBJECT 379
#define F_FIND_PLAYER 380
#define F_FIRST_INVENTORY 381
#define F_FUNCTION_EXISTS 382
#define F_GET_DIR 383
#define F_IMPLODE 384
#define F_INHERIT_LIST 385
#define F_INPUT_TO 386
#define F_INSERT_ALIST 387
#define F_INTERACTIVE 388
#define F_INTERSECT_ALIST 389
#define F_INTP 390
#define F_LIVING 391
#define F_LOCALCMD 392
#define F_LOG_FILE 393
#define F_LOWER_CASE 394
#define F_MAP_ARRAY 395
#define F_MEMBER_ARRAY 396
#define F_MKDIR 397
#define F_MOVE_OBJECT 398
#define F_NEXT_INVENTORY 399
#define F_NOTIFY_FAIL 400
#define F_OBJECTP 401
#define F_ORDER_ALIST 402
#define F_POINTERP 403
#define F_PRESENT 404
#define F_PREVIOUS_OBJECT 405
#define F_PROCESS_STRING 406
#define F_QUERY_HOST_NAME 407
#define F_QUERY_IDLE 408
#define F_QUERY_IP_NAME 409
#define F_QUERY_IP_NUMBER 410
#define F_QUERY_LOAD_AVERAGE 411
#define F_QUERY_SNOOP 412
#define F_QUERY_VERB 413
#define F_RANDOM 414
#define F_READ_BYTES 415
#define F_READ_FILE 416
#define F_REGEXP 417
#define F_REMOVE_CALL_OUT 418
#define F_RESTORE_OBJECT 419
#define F_RM 420
#define F_RMDIR 421
#define F_SAVE_OBJECT 422
#define F_SAY 423
#define F_SET_BIT 424
#define F_SET_HEART_BEAT 425
#define F_SET_LIGHT 426
#define F_SET_LIVING_NAME 427
#define F_SHADOW 428
#define F_SHOUT 429
#define F_SHUTDOWN 430
#define F_SIZEOF 431
#define F_SNOOP 432
#define F_SORT_ARRAY 433
#define F_STRINGP 434
#define F_STRLEN 435
#define F_SWAP 436
#define F_TAIL 437
#define F_TELL_OBJECT 438
#define F_TELL_ROOM 439
#define F_TEST_BIT 440
#define F_THIS_OBJECT 441
#define F_THIS_PLAYER 442
#define F_THROW 443
#define F_TIME 444
#define F_TRACE 445
#define F_TRACEPREFIX 446
#define F_TRANSFER 447
#define F_UNIQUE_ARRAY 448
#define F_USERS 449
#define F_VERSION 450
#define F_WIZLIST 451
#define F_WRITE 452
#define F_WRITE_BYTES 453
#define F_WRITE_FILE 454
#define F_CASE 455
#define F_DEFAULT 456
#define F_RANGE 457

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 772 "lang.y"

	int number;
	unsigned int address;	/* Address of an instruction */
	char *string;
	short type;
	struct { int key; char block; } case_label;
	struct function *funp;

#line 1148 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   959

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  225
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  98
/* YYNRULES -- Number of rules.  */
#define YYNRULES  221
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  371

#define YYUNDEFTOK  2
#define YYMAXUTOK   457


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   219,   214,     2,
     205,   206,   204,   217,   207,   218,     2,   220,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   211,   203,
     216,   208,   215,   222,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   223,     2,   224,   213,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   209,   212,   210,   221,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   797,   797,   799,   800,   802,   803,   805,   850,   861,
     861,   863,   863,   866,   879,   865,   901,   902,   904,   913,
     916,   917,   919,   920,   922,   923,   924,   925,   926,   927,
     929,   930,   932,   934,   939,   939,   941,   942,   943,   944,
     945,   946,   948,   949,   951,   957,   956,   978,   981,   982,
     984,   989,   990,   992,   993,   994,   996,  1004,  1004,  1004,
    1004,  1004,  1004,  1004,  1004,  1005,  1006,  1007,  1017,  1025,
    1029,  1025,  1045,  1045,  1068,  1070,  1073,  1081,  1068,  1097,
    1098,  1101,  1100,  1290,  1303,  1320,  1330,  1341,  1342,  1344,
    1345,  1347,  1348,  1350,  1351,  1352,  1354,  1355,  1356,  1357,
    1358,  1360,  1361,  1362,  1364,  1365,  1366,  1368,  1369,  1370,
    1371,  1373,  1374,  1376,  1377,  1378,  1379,  1381,  1395,  1396,
    1396,  1400,  1401,  1411,  1413,  1415,  1421,  1414,  1441,  1442,
    1443,  1444,  1445,  1446,  1447,  1448,  1449,  1450,  1451,  1453,
    1461,  1468,  1469,  1470,  1472,  1473,  1475,  1477,  1476,  1492,
    1494,  1493,  1509,  1510,  1520,  1521,  1531,  1532,  1544,  1545,
    1555,  1566,  1567,  1569,  1571,  1573,  1576,  1577,  1586,  1596,
    1597,  1599,  1624,  1625,  1634,  1643,  1653,  1654,  1662,  1663,
    1670,  1677,  1682,  1689,  1697,  1698,  1705,  1713,  1714,  1731,
    1731,  1732,  1733,  1734,  1735,  1736,  1744,  1744,  1758,  1763,
    1768,  1769,  1771,  1783,  1790,  1811,  1829,  1837,  1844,  1845,
    1855,  1854,  2030,  2029,  2043,  2044,  2050,  2060,  2058,  2069,
    2076,  2077
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "F_JUMP", "F_JUMP_WHEN_ZERO",
  "F_JUMP_WHEN_NON_ZERO", "F_POP_VALUE", "F_DUP", "F_STORE",
  "F_CALL_FUNCTION_BY_ADDRESS", "F_PUSH_IDENTIFIER_LVALUE",
  "F_PUSH_LOCAL_VARIABLE_LVALUE", "F_PUSH_INDEXED_LVALUE", "F_INDIRECT",
  "F_INDEX", "F_CONST0", "F_CONST1", "F_IF", "F_IDENTIFIER", "F_LAND",
  "F_LOR", "F_STATUS", "F_RETURN", "F_STRING", "F_INC", "F_DEC",
  "F_POST_INC", "F_POST_DEC", "F_COMMA", "F_NUMBER", "F_ASSIGN", "F_INT",
  "F_ADD", "F_SUBTRACT", "F_MULTIPLY", "F_DIVIDE", "F_LT", "F_GT", "F_EQ",
  "F_GE", "F_LE", "F_NE", "F_ADD_EQ", "F_SUB_EQ", "F_DIV_EQ", "F_MULT_EQ",
  "F_NEGATE", "F_SUBSCRIPT", "F_WHILE", "F_BREAK", "F_DO", "F_FOR",
  "F_SWITCH", "F_SSCANF", "F_PARSE_COMMAND", "F_STRING_DECL",
  "F_LOCAL_NAME", "F_ELSE", "F_DESCRIBE", "F_CONTINUE", "F_MOD",
  "F_MOD_EQ", "F_INHERIT", "F_COLON_COLON", "F_STATIC", "F_ARROW",
  "F_AGGREGATE", "F_COMPL", "F_AND", "F_AND_EQ", "F_OR", "F_OR_EQ",
  "F_XOR", "F_XOR_EQ", "F_LSH", "F_LSH_EQ", "F_RSH", "F_RSH_EQ", "F_CATCH",
  "F_OBJECT", "F_VOID", "F_MIXED", "F_PRIVATE", "F_NO_MASK", "F_NOT",
  "F_PROTECTED", "F_PUBLIC", "F_VARARGS", "F_ADD_ACTION", "F_ADD_VERB",
  "F_ADD_WORTH", "F_ADD_XVERB", "F_ALL_INVENTORY", "F_ALLOCATE", "F_ASSOC",
  "F_BREAK_POINT", "F_CALL_OTHER", "F_CALL_OUT", "F_CALL_OUT_INFO",
  "F_CAPITALIZE", "F_CAT", "F_CINDENT", "F_CLEAR_BIT", "F_CLONE_OBJECT",
  "F_COMMAND", "F_CREATE_WIZARD", "F_CREATOR", "F_CRYPT", "F_CTIME",
  "F_DEBUG_INFO", "F_DEEP_INVENTORY", "F_DESTRUCT", "F_DISABLE_COMMANDS",
  "F_ED", "F_ENABLE_COMMANDS", "F_ENVIRONMENT", "F_EXEC", "F_EXPLODE",
  "F_EXTRACT", "F_FILE_NAME", "F_FILE_SIZE", "F_FILTER_ARRAY",
  "F_FIND_CALL_OUT", "F_FIND_LIVING", "F_FIND_OBJECT", "F_FIND_PLAYER",
  "F_FIRST_INVENTORY", "F_FUNCTION_EXISTS", "F_GET_DIR", "F_IMPLODE",
  "F_INHERIT_LIST", "F_INPUT_TO", "F_INSERT_ALIST", "F_INTERACTIVE",
  "F_INTERSECT_ALIST", "F_INTP", "F_LIVING", "F_LOCALCMD", "F_LOG_FILE",
  "F_LOWER_CASE", "F_MAP_ARRAY", "F_MEMBER_ARRAY", "F_MKDIR",
  "F_MOVE_OBJECT", "F_NEXT_INVENTORY", "F_NOTIFY_FAIL", "F_OBJECTP",
  "F_ORDER_ALIST", "F_POINTERP", "F_PRESENT", "F_PREVIOUS_OBJECT",
  "F_PROCESS_STRING", "F_QUERY_HOST_NAME", "F_QUERY_IDLE",
  "F_QUERY_IP_NAME", "F_QUERY_IP_NUMBER", "F_QUERY_LOAD_AVERAGE",
  "F_QUERY_SNOOP", "F_QUERY_VERB", "F_RANDOM", "F_READ_BYTES",
  "F_READ_FILE", "F_REGEXP", "F_REMOVE_CALL_OUT", "F_RESTORE_OBJECT",
  "F_RM", "F_RMDIR", "F_SAVE_OBJECT", "F_SAY", "F_SET_BIT",
  "F_SET_HEART_BEAT", "F_SET_LIGHT", "F_SET_LIVING_NAME", "F_SHADOW",
  "F_SHOUT", "F_SHUTDOWN", "F_SIZEOF", "F_SNOOP", "F_SORT_ARRAY",
  "F_STRINGP", "F_STRLEN", "F_SWAP", "F_TAIL", "F_TELL_OBJECT",
  "F_TELL_ROOM", "F_TEST_BIT", "F_THIS_OBJECT", "F_THIS_PLAYER", "F_THROW",
  "F_TIME", "F_TRACE", "F_TRACEPREFIX", "F_TRANSFER", "F_UNIQUE_ARRAY",
  "F_USERS", "F_VERSION", "F_WIZLIST", "F_WRITE", "F_WRITE_BYTES",
  "F_WRITE_FILE", "F_CASE", "F_DEFAULT", "F_RANGE", "';'", "'*'", "'('",
  "')'", "','", "'='", "'{'", "'}'", "':'", "'|'", "'^'", "'&'", "'>'",
  "'<'", "'+'", "'-'", "'%'", "'/'", "'~'", "'?'", "'['", "']'", "$accept",
  "all", "program", "possible_semi_colon", "inheritance", "number",
  "optional_star", "block_or_semi", "def", "$@1", "$@2", "new_arg_name",
  "argument", "argument_list", "type_modifier", "type_modifier_list",
  "type", "cast", "opt_basic_type", "basic_type", "name_list", "new_name",
  "$@3", "block", "local_declarations", "new_local_name",
  "local_name_list", "statements", "statement", "while", "$@4", "$@5",
  "do", "$@6", "for", "$@7", "$@8", "$@9", "$@10", "for_expr", "switch",
  "$@11", "case", "case_label", "constant", "const1", "const2", "const3",
  "const4", "const5", "const6", "const7", "const8", "const9", "default",
  "comma_expr", "$@12", "expr0", "expr01", "$@13", "$@14", "assign",
  "return", "expr_list", "expr_list2", "expr1", "$@15", "expr2", "$@16",
  "expr211", "expr212", "expr213", "expr22", "expr23", "expr24", "expr25",
  "expr27", "expr28", "expr3", "expr31", "expr4", "catch", "$@17",
  "sscanf", "parse_command", "lvalue_list", "lvalue", "string",
  "string_constant", "string_con1", "function_call", "@18", "$@19",
  "function_name", "cond", "$@20", "condStart", "optional_else_part", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,    59,    42,    40,    41,    44,    61,   123,
     125,    58,   124,    94,    38,    62,    60,    43,    45,    37,
      47,   126,    63,    91,    93
};
# endif

#define YYPACT_NINF (-300)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-215)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -300,    28,   288,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -156,   346,   414,  -133,  -300,  -300,  -300,  -300,  -300,
    -300,    39,  -300,  -300,  -300,  -300,  -300,  -300,    65,  -107,
     -81,   -75,  -165,  -300,  -133,  -300,   -66,   -57,   135,  -300,
      24,   532,   -32,  -300,   -51,   -40,   203,   -50,   751,   -56,
    -300,   196,   196,  -300,   -26,   -12,  -300,   181,  -300,   549,
     269,   549,   549,  -300,   549,  -300,  -300,   -20,   206,   -17,
      14,    17,  -300,  -300,   104,  -152,  -184,  -300,  -300,  -300,
     -54,  -300,  -300,  -300,   659,  -300,  -300,  -300,  -300,   346,
    -300,   210,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,   613,   214,   313,   -54,   -53,   -53,   532,
     532,  -300,    32,  -300,    76,   333,  -133,    33,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,   613,   613,   613,   613,   613,
     613,   613,   613,   613,   613,   613,   613,   613,   549,   549,
     549,    -2,   532,  -300,  -300,   532,    35,  -177,  -300,  -300,
    -300,  -300,    36,    40,   532,  -300,    43,    41,    48,  -300,
      49,   613,   613,   613,    14,    17,  -300,    13,    13,    13,
      13,  -152,  -152,    13,    13,  -184,  -184,  -300,  -300,  -300,
     182,  -300,  -183,  -300,   359,  -300,  -300,  -300,  -300,   532,
     532,    55,    58,   188,  -300,   532,  -300,  -300,  -300,    50,
     532,  -300,    61,     0,    69,    72,  -300,  -300,  -300,  -300,
      64,   359,    22,  -300,   644,    75,   401,    78,    80,    91,
      88,    15,    90,  -300,  -133,  -300,    89,    68,  -300,   262,
    -300,   266,  -300,  -300,  -300,  -300,   123,   124,  -300,   112,
     196,   129,   532,   613,   137,  -300,  -300,   532,   126,  -300,
    -300,   532,  -300,  -300,  -300,   315,   -24,   316,   317,  -172,
     142,   146,   141,    57,   -31,    38,  -113,  -147,  -300,  -300,
    -300,   144,  -300,   345,   157,   162,  -300,  -300,   163,   112,
    -300,  -300,  -300,   -74,  -300,    69,  -300,  -300,   166,   443,
     172,  -300,  -142,  -300,  -300,    15,  -300,   -24,   -24,   -24,
     -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,   -24,
     -24,   -24,   -24,   356,  -300,  -133,  -300,   532,   332,   324,
    -300,   179,  -300,   187,   185,  -300,  -300,   184,   146,   141,
      57,   -31,   -31,    38,    38,    38,    38,  -113,  -113,  -147,
    -147,  -300,  -300,  -300,  -300,  -300,   193,   195,   112,  -300,
    -300,  -300,   112,  -300,  -300,   532,  -300,   443,  -300,   112,
     197,   202,  -300,   204,  -300,  -300,   457,   198,  -300,   112,
    -300
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     0,    30,     1,    25,    26,    24,    29,    27,    28,
      17,     5,    30,    35,     9,     6,     3,    31,    36,    37,
      38,     0,    39,    40,    41,    32,    34,    10,     0,     0,
      42,     0,    44,    16,     9,     7,     0,     0,     0,    43,
      30,     0,    44,    22,     0,    21,    35,     9,     0,   202,
     206,     0,     0,     8,     0,     0,   203,     0,   196,     0,
       0,     0,     0,   190,     0,    46,   121,   124,   146,   149,
     152,   154,   156,   158,   161,   166,   169,   172,   176,   178,
     184,   192,   193,   194,   188,   189,   187,   210,    14,    30,
      19,     0,   134,   135,   138,   136,   137,   129,   130,   131,
     132,   133,   128,     0,     0,     0,     0,   179,   180,     0,
       0,   215,     0,   181,   188,     0,     9,   119,   118,   183,
     182,   177,   125,   147,   150,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   185,   186,     0,     0,     0,    23,    18,
     123,   216,     0,     0,     0,   144,     0,   142,     0,   191,
       0,     0,     0,     0,   153,   155,   157,   159,   163,   165,
     160,   167,   168,   162,   164,   170,   171,   173,   174,   175,
     214,   212,   119,   122,     0,    12,    48,    15,    11,     0,
       0,   119,     0,     0,    33,     0,   126,   148,   151,     0,
       0,   205,     0,     0,   200,     0,   197,   195,   145,   120,
       0,     0,   119,   211,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    66,     9,    65,     0,     0,    58,     0,
      59,     0,    60,    61,    62,    63,   119,     0,    57,     0,
       0,     0,     0,     0,     0,   204,    55,     0,   119,    67,
      74,     0,    68,   208,   113,     0,     0,     0,     0,     0,
      85,    87,    89,    91,    93,    96,   101,   104,   107,   111,
      86,   207,   117,     0,    51,     0,    47,    54,     0,     0,
      56,    64,   217,   188,   198,   200,   127,   213,   119,     0,
     119,   115,     0,   114,   116,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     9,    49,     0,     0,   220,
     201,     0,   219,     0,    80,    81,   112,     0,    88,    90,
      92,    94,    95,    98,   100,    97,    99,   102,   103,   105,
     106,   108,   109,   110,   209,    52,   119,     0,     0,   218,
     199,    75,     0,    84,    70,     0,   221,     0,    82,     0,
     119,     0,    71,     0,    76,    73,     0,     0,    77,     0,
      78
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -300,  -300,  -300,  -300,  -300,  -300,    -1,  -300,  -300,  -300,
    -300,   319,  -300,  -300,  -300,    25,   412,  -300,  -300,   -46,
     382,  -300,  -300,   271,  -300,  -300,   105,   194,   -41,  -300,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -299,
    -300,  -300,  -300,   128,   171,   139,   136,   140,  -153,  -228,
    -148,  -101,  -138,  -300,  -300,   -60,  -300,    -7,   -93,  -300,
    -300,   354,  -300,  -169,  -300,   278,  -300,   279,  -300,  -300,
     321,   322,   314,  -300,   174,    83,    86,  -300,   201,  -300,
     -48,  -300,  -300,  -300,  -300,  -160,   648,  -300,  -300,  -300,
    -300,  -300,  -300,   306,  -300,  -300,  -300,  -300
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    16,    10,    63,   273,   187,    11,    36,
     147,    43,    44,    45,    12,    46,    47,    64,    25,    26,
      29,    30,    37,   225,   203,   274,   275,   226,   227,   228,
     229,   359,   230,   231,   232,   289,   357,   366,   369,   323,
     233,   352,   234,   259,   260,   261,   262,   263,   264,   265,
     266,   267,   268,   269,   235,   236,   160,   118,    66,   161,
     210,   103,   237,   156,   157,    67,   162,    68,   163,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,   112,    82,    83,   241,    84,    85,   270,   271,
      86,   146,   199,    87,   238,   319,   239,   349
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     117,   214,   124,   106,   106,   254,    90,   104,   302,   303,
     150,   141,  -188,    28,   116,   202,   180,   215,    49,   200,
     138,    18,   216,    50,    51,    52,   185,    13,     3,    53,
     295,    19,   186,    38,    65,   139,   140,    17,   253,   296,
     -13,   201,   244,   -45,   254,   117,    91,    15,   -69,   217,
     -72,   218,   219,    54,    55,    20,    56,   310,   361,   220,
     255,    57,    31,    57,   326,   136,   137,   367,   196,   214,
     297,    27,   311,   312,   333,   334,   335,   336,    58,    22,
      23,    24,   182,    32,    59,   215,    49,   132,     4,   133,
     216,    50,    51,    52,   191,   300,    33,    53,   301,   255,
     143,   144,   152,   153,   308,   309,     5,     6,   155,     7,
       8,     9,   306,    48,   307,   158,   -69,   217,   -72,   218,
     219,    54,    55,   320,    56,   321,    34,   220,    35,   215,
      49,    57,  -200,   240,   216,    50,    51,    52,   183,    40,
     212,    53,   128,   129,   130,   131,    58,   331,   332,  -214,
     286,    41,    59,    42,    27,    88,   248,   224,   337,   338,
     -69,   217,   -72,   218,   219,    54,    55,    89,    56,   142,
    -188,   220,   341,   342,   343,    57,   -45,   155,   132,   109,
     133,   256,   204,   205,   304,   305,   208,   288,   209,    48,
      58,   290,   106,   110,   257,   125,    59,   258,   282,   111,
     221,   222,   122,   223,   155,    60,    49,   339,   340,   186,
     -53,    50,    51,    52,    49,   171,   172,    53,    61,    50,
     256,    62,   175,   176,    18,    53,   123,   126,   149,   324,
     -20,   127,   151,   257,    19,   285,   258,   154,   318,   159,
     184,    54,    55,   189,    56,   104,   245,   190,   193,    54,
      55,    57,    56,   192,   194,   211,   195,   346,    20,    57,
     113,   206,   119,   120,   207,   121,    58,   213,   221,   222,
      48,   223,    59,    60,    58,   243,   240,   186,   -53,   242,
     247,   249,    22,    23,    24,   250,    61,    49,    -2,    62,
      18,   252,    50,    51,    52,   360,   251,   324,    53,   276,
      19,   272,   167,   168,   169,   170,   324,   356,   173,   174,
     278,   358,   221,   222,    48,   223,   279,    60,   362,   134,
     135,   186,    54,    55,    20,    56,   280,   281,   370,  -140,
      61,    49,    57,    62,    48,   284,    50,    51,    52,   177,
     178,   179,    53,   287,   291,   293,   294,    58,    22,    23,
      24,    49,     4,    59,   297,   299,    50,    51,    52,   298,
      48,   313,    53,   314,   315,   316,    54,    55,   317,    56,
       5,     6,   322,     7,     8,     9,    57,    49,   325,   344,
     347,   348,    50,    51,    52,   350,    54,    55,    53,    56,
     351,    58,  -119,    60,  -143,   353,    57,    59,  -143,   354,
     355,   105,    48,   363,   368,   364,    61,   365,   148,    62,
       4,    58,    54,    55,    14,    56,    39,    59,   188,    49,
     345,   277,    57,   327,    50,    51,    52,   292,     5,     6,
      53,     7,     8,     9,   329,    18,   328,    58,   145,   330,
     197,   166,   198,    59,    48,    19,   164,   181,   165,     0,
       0,     0,     0,     0,    54,    55,     0,    56,    48,     0,
       0,    49,     0,     0,    57,     0,    50,    51,    52,    20,
       0,     0,    53,     0,    60,    49,    21,     0,   115,    58,
      50,    51,    52,     0,     0,    59,    53,    61,     0,     0,
      62,     0,     0,    22,    23,    24,    54,    55,     0,    56,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
      54,    55,     0,    56,     0,     0,     0,     0,    60,     0,
      57,    58,   115,     0,     0,     0,     0,    59,     0,     0,
       0,    61,     0,    48,    62,    58,     0,     0,    60,     0,
       0,    59,     0,  -141,     0,     0,     0,     0,     0,     0,
      49,    61,     0,     0,    62,    50,    51,    52,     0,     0,
       0,    53,     0,     0,    60,  -141,     0,    49,     0,     0,
       0,     0,    50,    51,    52,     0,     0,    61,    53,     0,
      62,     0,     0,     0,     0,    54,    55,     0,    56,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    54,    55,  -139,    56,    60,     0,     0,     0,
      58,     0,    57,     0,     0,     0,    59,     0,     0,    61,
       0,     0,    62,     0,     0,     0,     0,    58,     0,     0,
       0,    49,     0,    59,     0,     0,    50,    51,    52,     0,
       0,     0,    53,     0,     0,     0,   -79,     0,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,    60,   -79,    62,     0,    54,    55,     0,    56,
       0,     0,     0,     0,     0,    61,    57,     0,    62,     0,
       0,     0,     0,   143,   144,     0,    92,    93,    94,    95,
       0,    58,     0,     0,     0,     0,     0,    59,     0,   107,
     108,    92,    93,    94,    95,    96,     0,   114,     0,   114,
     114,     0,   114,    97,     0,    98,     0,    99,     0,   100,
      96,   101,     0,     0,     0,     0,     0,     0,    97,     0,
      98,     0,    99,     0,   100,     0,   101,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,   114,     0,    62,   105,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,     0,     0,   114,   114,   114,   114,   114,   114,   114,
     114,   114,   114,   114,   114,   114,   114,   114,   114,     0,
       0,     0,     0,    92,    93,    94,    95,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
     114,   114,    96,     0,     0,     0,     0,     0,    60,     0,
      97,     0,    98,     0,    99,     0,   100,     0,   101,     0,
       0,    61,     0,     0,    62,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   246,     0,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   283,     0,
       0,   114,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   102
};

static const yytype_int16 yycheck[] =
{
      60,     1,    19,    51,    52,    29,    56,    63,    39,    40,
     103,    65,    65,    14,    60,   184,    18,    17,    18,   202,
     204,    21,    22,    23,    24,    25,   203,     2,     0,    29,
     202,    31,   209,    34,    41,   219,   220,    12,    23,   211,
     205,   224,   211,   208,    29,   105,    47,   203,    48,    49,
      50,    51,    52,    53,    54,    55,    56,   204,   357,    59,
      84,    63,    23,    63,   206,   217,   218,   366,   161,     1,
     212,   204,   219,   220,   302,   303,   304,   305,    78,    79,
      80,    81,   142,    18,    84,    17,    18,    74,    64,    76,
      22,    23,    24,    25,   154,    38,   203,    29,    41,    84,
      24,    25,   109,   110,   217,   218,    82,    83,   115,    85,
      86,    87,    74,     1,    76,   116,    48,    49,    50,    51,
      52,    53,    54,   283,    56,   285,   207,    59,   203,    17,
      18,    63,   206,   207,    22,    23,    24,    25,   145,   205,
     200,    29,    38,    39,    40,    41,    78,   300,   301,   205,
     243,   208,    84,    18,   204,   206,   216,   203,   306,   307,
      48,    49,    50,    51,    52,    53,    54,   207,    56,   223,
     223,    59,   310,   311,   312,    63,   208,   184,    74,   205,
      76,   205,   189,   190,   215,   216,   193,   247,   195,     1,
      78,   251,   240,   205,   218,   212,    84,   221,   239,    18,
     200,   201,   222,   203,   211,   205,    18,   308,   309,   209,
     210,    23,    24,    25,    18,   132,   133,    29,   218,    23,
     205,   221,   136,   137,    21,    29,    20,   213,    18,   289,
     206,   214,    18,   218,    31,   242,   221,   205,   279,   206,
     205,    53,    54,   207,    56,    63,   224,   207,   207,    53,
      54,    63,    56,   210,   206,   205,   207,   317,    55,    63,
      59,   206,    61,    62,   206,    64,    78,   206,   200,   201,
       1,   203,    84,   205,    78,   211,   207,   209,   210,   207,
     205,   203,    79,    80,    81,   205,   218,    18,     0,   221,
      21,   203,    23,    24,    25,   355,   205,   357,    29,   210,
      31,   211,   128,   129,   130,   131,   366,   348,   134,   135,
      48,   352,   200,   201,     1,   203,    50,   205,   359,   215,
     216,   209,    53,    54,    55,    56,   203,   203,   369,   203,
     218,    18,    63,   221,     1,   206,    23,    24,    25,   138,
     139,   140,    29,   206,    29,    29,    29,    78,    79,    80,
      81,    18,    64,    84,   212,   214,    23,    24,    25,   213,
       1,   217,    29,    18,   207,   203,    53,    54,   205,    56,
      82,    83,   206,    85,    86,    87,    63,    18,   206,    23,
      48,    57,    23,    24,    25,   206,    53,    54,    29,    56,
     203,    78,   207,   205,   206,   211,    63,    84,   210,   206,
     205,   205,     1,   206,   206,   203,   218,   203,    89,   221,
      64,    78,    53,    54,     2,    56,    34,    84,   147,    18,
     315,   227,    63,   295,    23,    24,    25,   256,    82,    83,
      29,    85,    86,    87,   298,    21,   297,    78,    84,   299,
     162,   127,   163,    84,     1,    31,   125,   141,   126,    -1,
      -1,    -1,    -1,    -1,    53,    54,    -1,    56,     1,    -1,
      -1,    18,    -1,    -1,    63,    -1,    23,    24,    25,    55,
      -1,    -1,    29,    -1,   205,    18,    62,    -1,   209,    78,
      23,    24,    25,    -1,    -1,    84,    29,   218,    -1,    -1,
     221,    -1,    -1,    79,    80,    81,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      53,    54,    -1,    56,    -1,    -1,    -1,    -1,   205,    -1,
      63,    78,   209,    -1,    -1,    -1,    -1,    84,    -1,    -1,
      -1,   218,    -1,     1,   221,    78,    -1,    -1,   205,    -1,
      -1,    84,    -1,   210,    -1,    -1,    -1,    -1,    -1,    -1,
      18,   218,    -1,    -1,   221,    23,    24,    25,    -1,    -1,
      -1,    29,    -1,    -1,   205,   206,    -1,    18,    -1,    -1,
      -1,    -1,    23,    24,    25,    -1,    -1,   218,    29,    -1,
     221,    -1,    -1,    -1,    -1,    53,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    53,    54,   203,    56,   205,    -1,    -1,    -1,
      78,    -1,    63,    -1,    -1,    -1,    84,    -1,    -1,   218,
      -1,    -1,   221,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    18,    -1,    84,    -1,    -1,    23,    24,    25,    -1,
      -1,    -1,    29,    -1,    -1,    -1,   203,    -1,   205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   218,   205,   206,   221,    -1,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,   218,    63,    -1,   221,    -1,
      -1,    -1,    -1,    24,    25,    -1,    42,    43,    44,    45,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    84,    -1,    51,
      52,    42,    43,    44,    45,    61,    -1,    59,    -1,    61,
      62,    -1,    64,    69,    -1,    71,    -1,    73,    -1,    75,
      61,    77,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      71,    -1,    73,    -1,    75,    -1,    77,   205,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     218,   103,    -1,   221,   205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   218,    -1,    -1,
     221,    -1,    -1,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
      -1,    -1,    -1,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,    61,    -1,    -1,    -1,    -1,    -1,   205,    -1,
      69,    -1,    71,    -1,    73,    -1,    75,    -1,    77,    -1,
      -1,   218,    -1,    -1,   221,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,    -1,    -1,
      -1,    -1,   208,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   208,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,
      -1,   243,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   208
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   226,   227,     0,    64,    82,    83,    85,    86,    87,
     229,   233,   239,   240,   241,   203,   228,   240,    21,    31,
      55,    62,    79,    80,    81,   243,   244,   204,   231,   245,
     246,    23,    18,   203,   207,   203,   234,   247,   231,   245,
     205,   208,    18,   236,   237,   238,   240,   241,     1,    18,
      23,    24,    25,    29,    53,    54,    56,    63,    78,    84,
     205,   218,   221,   230,   242,   282,   283,   290,   292,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   308,   309,   311,   312,   315,   318,   206,   207,
      56,   231,    42,    43,    44,    45,    61,    69,    71,    73,
      75,    77,   208,   286,    63,   205,   305,   311,   311,   205,
     205,    18,   307,   303,   311,   209,   244,   280,   282,   303,
     303,   303,   222,    20,    19,   212,   213,   214,    38,    39,
      40,    41,    74,    76,   215,   216,   217,   218,   204,   219,
     220,    65,   223,    24,    25,   286,   316,   235,   236,    18,
     283,    18,   282,   282,   205,   282,   288,   289,   231,   206,
     281,   284,   291,   293,   295,   296,   297,   299,   299,   299,
     299,   300,   300,   299,   299,   301,   301,   303,   303,   303,
      18,   318,   280,   282,   205,   203,   209,   232,   248,   207,
     207,   280,   210,   207,   206,   207,   283,   290,   292,   317,
     202,   224,   288,   249,   282,   282,   206,   206,   282,   282,
     285,   205,   280,   206,     1,    17,    22,    49,    51,    52,
      59,   200,   201,   203,   244,   248,   252,   253,   254,   255,
     257,   258,   259,   265,   267,   279,   280,   287,   319,   321,
     207,   310,   207,   211,   288,   224,   203,   205,   280,   203,
     205,   205,   203,    23,    29,    84,   205,   218,   221,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,   278,
     313,   314,   211,   231,   250,   251,   210,   252,    48,    50,
     203,   203,   253,   311,   206,   282,   283,   206,   280,   260,
     280,    29,   269,    29,    29,   202,   211,   212,   213,   214,
      38,    41,    39,    40,   215,   216,    74,    76,   217,   218,
     204,   219,   220,   217,    18,   207,   203,   205,   253,   320,
     310,   310,   206,   264,   280,   206,   206,   268,   270,   271,
     272,   273,   273,   274,   274,   274,   274,   275,   275,   276,
     276,   277,   277,   277,    23,   251,   280,    48,    57,   322,
     206,   203,   266,   211,   206,   205,   253,   261,   253,   256,
     280,   264,   253,   206,   203,   203,   262,   264,   206,   263,
     253
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   225,   226,   227,   227,   228,   228,   229,   230,   231,
     231,   232,   232,   234,   235,   233,   233,   233,   236,   236,
     237,   237,   238,   238,   239,   239,   239,   239,   239,   239,
     240,   240,   241,   242,   243,   243,   244,   244,   244,   244,
     244,   244,   245,   245,   246,   247,   246,   248,   249,   249,
     250,   251,   251,   252,   252,   252,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   255,
     256,   254,   258,   257,   260,   261,   262,   263,   259,   264,
     264,   266,   265,   267,   267,   268,   268,   269,   269,   270,
     270,   271,   271,   272,   272,   272,   273,   273,   273,   273,
     273,   274,   274,   274,   275,   275,   275,   276,   276,   276,
     276,   277,   277,   278,   278,   278,   278,   279,   280,   281,
     280,   282,   282,   282,   283,   284,   285,   283,   286,   286,
     286,   286,   286,   286,   286,   286,   286,   286,   286,   287,
     287,   288,   288,   288,   289,   289,   290,   291,   290,   292,
     293,   292,   294,   294,   295,   295,   296,   296,   297,   297,
     297,   298,   298,   298,   298,   298,   299,   299,   299,   300,
     300,   300,   301,   301,   301,   301,   302,   302,   303,   303,
     303,   303,   303,   303,   304,   304,   304,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   307,   306,   308,   309,
     310,   310,   311,   311,   311,   311,   312,   313,   314,   314,
     316,   315,   317,   315,   318,   318,   318,   320,   319,   321,
     322,   322
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     0,     0,     1,     4,     1,     0,
       1,     1,     1,     0,     0,     9,     3,     1,     3,     2,
       0,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       0,     2,     2,     4,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     0,     5,     4,     0,     4,
       2,     1,     3,     0,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     2,     0,
       0,     7,     0,     8,     0,     0,     0,     0,    13,     0,
       1,     0,     6,     3,     5,     1,     1,     1,     3,     1,
       3,     1,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
       3,     1,     3,     1,     2,     2,     2,     2,     1,     0,
       4,     1,     3,     3,     1,     0,     0,     7,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     0,     1,     2,     1,     3,     1,     0,     4,     1,
       0,     4,     1,     3,     1,     3,     1,     3,     1,     3,
       3,     1,     3,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     1,     2,     1,     2,
       2,     2,     2,     2,     1,     2,     2,     1,     1,     1,
       1,     3,     1,     1,     1,     5,     0,     5,     7,     9,
       0,     3,     1,     1,     6,     4,     1,     1,     1,     3,
       0,     5,     0,     7,     1,     2,     3,     0,     4,     4,
       0,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 6:
#line 803 "lang.y"
                         { yyerror("Extra ';'. Ignored."); }
#line 2796 "y.tab.c"
    break;

  case 7:
#line 806 "lang.y"
                {
		    struct object *ob;
		    struct inherit inherit;
		    int initializer;

		    ob = find_object2((yyvsp[-1].string));
		    if (ob == 0) {
			inherit_file = (yyvsp[-1].string);
			/* Return back to load_object() */
			YYACCEPT;
		    }
		    free((yyvsp[-1].string));
		    if (ob->flags & O_APPROVED)
			approved_object = 1;
		    inherit.prog = ob->prog;
		    inherit.function_index_offset =
			mem_block[A_FUNCTIONS].current_size /
			    sizeof (struct function);
		    inherit.variable_index_offset =
			mem_block[A_VARIABLES].current_size /
			    sizeof (struct variable);
		    add_to_mem_block(A_INHERITS, &inherit, sizeof inherit);
		    copy_variables(ob->prog, (yyvsp[-3].number));
		    initializer = copy_functions(ob->prog, (yyvsp[-3].number));
		    if (initializer > 0) {
			struct function *funp;
			int f;
			f = define_new_function("::__INIT", 0, 0, 0, 0, 0);
			funp = FUNCTION(f);
			funp->offset = mem_block[A_INHERITS].current_size /
			    sizeof (struct inherit) - 1;
			funp->flags = NAME_STRICT_TYPES |
			    NAME_INHERITED | NAME_HIDDEN;
			funp->type = TYPE_VOID;
			funp->function_index_offset = initializer;
			transfer_init_control();
			ins_f_byte(F_CALL_FUNCTION_BY_ADDRESS);
			ins_short(f);
			ins_byte(0);	/* Actual number of arguments */
			ins_f_byte(F_POP_VALUE);
			add_new_init_jump();
		    }
		}
#line 2844 "y.tab.c"
    break;

  case 8:
#line 851 "lang.y"
        {
	    if ( (yyvsp[0].number) == 0 ) {
		ins_f_byte(F_CONST0); (yyval.type) = TYPE_ANY;
	    } else if ( (yyvsp[0].number) == 1 ) {
		ins_f_byte(F_CONST1); (yyval.type) = TYPE_NUMBER;
	    } else {
		ins_f_byte(F_NUMBER); ins_long((yyvsp[0].number)); (yyval.type) = TYPE_NUMBER;
	    }
	}
#line 2858 "y.tab.c"
    break;

  case 9:
#line 861 "lang.y"
                           { (yyval.number) = 0; }
#line 2864 "y.tab.c"
    break;

  case 10:
#line 861 "lang.y"
                                             { (yyval.number) = TYPE_MOD_POINTER; }
#line 2870 "y.tab.c"
    break;

  case 11:
#line 863 "lang.y"
                     { (yyval.number) = 0; }
#line 2876 "y.tab.c"
    break;

  case 12:
#line 863 "lang.y"
                                       { (yyval.number) = ';'; }
#line 2882 "y.tab.c"
    break;

  case 13:
#line 866 "lang.y"
        {
	    /* Save start of function. */
	    push_explicit(mem_block[A_PROGRAM].current_size);

	    if ((yyvsp[-2].number) & TYPE_MOD_MASK) {
		exact_types = (yyvsp[-2].number) | (yyvsp[-1].number);
	    } else {
		if (pragma_strict_types)
		    yyerror("\"#pragma strict_types\" requires type of function");
		exact_types = 0;
	    }
	}
#line 2899 "y.tab.c"
    break;

  case 14:
#line 879 "lang.y"
        {
	    /*
	     * Define a prototype. If it is a real function, then the
	     * prototype will be replaced below.
	     */
	    define_new_function((yyvsp[-4].string), (yyvsp[-1].number), 0, 0,
				NAME_UNDEFINED|NAME_PROTOTYPE, (yyvsp[-6].number) | (yyvsp[-5].number));
	}
#line 2912 "y.tab.c"
    break;

  case 15:
#line 888 "lang.y"
        {
	    /* Either a prototype or a block */
	    if ((yyvsp[0].number) == ';') {
		(void)pop_address(); /* Not used here */
	    } else {
		define_new_function((yyvsp[-6].string), (yyvsp[-3].number), current_number_of_locals - (yyvsp[-3].number)+
			( max_break_stack_need -1 ) / sizeof(struct svalue) +1,
			pop_address(), 0, (yyvsp[-8].number) | (yyvsp[-7].number));
		ins_f_byte(F_CONST0); ins_f_byte(F_RETURN);
	    }
	    free_all_local_names();
	    free((yyvsp[-6].string));		/* Value was copied above */
	}
#line 2930 "y.tab.c"
    break;

  case 16:
#line 901 "lang.y"
                        { if ((yyvsp[-2].number) == 0) yyerror("Missing type"); }
#line 2936 "y.tab.c"
    break;

  case 18:
#line 905 "lang.y"
        {
	    if (exact_types && (yyvsp[-2].number) == 0) {
		yyerror("Missing type for argument");
		add_local_name((yyvsp[0].string), TYPE_ANY);	/* Supress more errors */
	    } else {
		add_local_name((yyvsp[0].string), (yyvsp[-2].number) | (yyvsp[-1].number));
	    }
	}
#line 2949 "y.tab.c"
    break;

  case 19:
#line 914 "lang.y"
                {yyerror("Illegal to redeclare local name"); }
#line 2955 "y.tab.c"
    break;

  case 20:
#line 916 "lang.y"
                      { (yyval.number) = 0; }
#line 2961 "y.tab.c"
    break;

  case 22:
#line 919 "lang.y"
                            { (yyval.number) = 1; }
#line 2967 "y.tab.c"
    break;

  case 23:
#line 920 "lang.y"
                                              { (yyval.number) = (yyvsp[-2].number) + 1; }
#line 2973 "y.tab.c"
    break;

  case 24:
#line 922 "lang.y"
                         { (yyval.number) = TYPE_MOD_NO_MASK; }
#line 2979 "y.tab.c"
    break;

  case 25:
#line 923 "lang.y"
                        { (yyval.number) = TYPE_MOD_STATIC; }
#line 2985 "y.tab.c"
    break;

  case 26:
#line 924 "lang.y"
                         { (yyval.number) = TYPE_MOD_PRIVATE; }
#line 2991 "y.tab.c"
    break;

  case 27:
#line 925 "lang.y"
                        { (yyval.number) = TYPE_MOD_PUBLIC; }
#line 2997 "y.tab.c"
    break;

  case 28:
#line 926 "lang.y"
                         { (yyval.number) = TYPE_MOD_VARARGS; }
#line 3003 "y.tab.c"
    break;

  case 29:
#line 927 "lang.y"
                           { (yyval.number) = TYPE_MOD_PROTECTED; }
#line 3009 "y.tab.c"
    break;

  case 30:
#line 929 "lang.y"
                                { (yyval.number) = 0; }
#line 3015 "y.tab.c"
    break;

  case 31:
#line 930 "lang.y"
                                                     { (yyval.number) = (yyvsp[-1].number) | (yyvsp[0].number); }
#line 3021 "y.tab.c"
    break;

  case 32:
#line 932 "lang.y"
                                        { (yyval.number) = (yyvsp[-1].number) | (yyvsp[0].number); current_type = (yyval.number); }
#line 3027 "y.tab.c"
    break;

  case 33:
#line 935 "lang.y"
        {
	    (yyval.type) = (yyvsp[-2].number) | (yyvsp[-1].number);
	}
#line 3035 "y.tab.c"
    break;

  case 35:
#line 939 "lang.y"
                                         { (yyval.number) = TYPE_UNKNOWN; }
#line 3041 "y.tab.c"
    break;

  case 36:
#line 941 "lang.y"
                     { (yyval.number) = TYPE_NUMBER; current_type = (yyval.number); }
#line 3047 "y.tab.c"
    break;

  case 37:
#line 942 "lang.y"
                { (yyval.number) = TYPE_NUMBER; current_type = (yyval.number); }
#line 3053 "y.tab.c"
    break;

  case 38:
#line 943 "lang.y"
                        { (yyval.number) = TYPE_STRING; current_type = (yyval.number); }
#line 3059 "y.tab.c"
    break;

  case 39:
#line 944 "lang.y"
                   { (yyval.number) = TYPE_OBJECT; current_type = (yyval.number); }
#line 3065 "y.tab.c"
    break;

  case 40:
#line 945 "lang.y"
                 {(yyval.number) = TYPE_VOID; current_type = (yyval.number); }
#line 3071 "y.tab.c"
    break;

  case 41:
#line 946 "lang.y"
                  { (yyval.number) = TYPE_ANY; current_type = (yyval.number); }
#line 3077 "y.tab.c"
    break;

  case 44:
#line 952 "lang.y"
        {
	    define_variable((yyvsp[0].string), current_type | (yyvsp[-1].number), 0);
	    free((yyvsp[0].string));
	}
#line 3086 "y.tab.c"
    break;

  case 45:
#line 957 "lang.y"
        {
	    int var_num;
	    define_variable((yyvsp[0].string), current_type | (yyvsp[-1].number), 0);
	    var_num = verify_declared((yyvsp[0].string));
	    transfer_init_control();
	    ins_f_byte(F_PUSH_IDENTIFIER_LVALUE);
	    ins_byte(var_num);
	}
#line 3099 "y.tab.c"
    break;

  case 46:
#line 966 "lang.y"
        {
	    if (!compatible_types((current_type | (yyvsp[-4].number)) & TYPE_MOD_MASK, (yyvsp[0].type))){
		char buff[100];
		sprintf(buff, "Type mismatch %s when initializing %s",
			get_two_types(current_type | (yyvsp[-4].number), (yyvsp[0].type)), (yyvsp[-3].string));
		yyerror(buff);
	    }
	    ins_f_byte(F_ASSIGN);
	    ins_f_byte(F_POP_VALUE);
	    add_new_init_jump();
	    free((yyvsp[-3].string));
	}
#line 3116 "y.tab.c"
    break;

  case 47:
#line 979 "lang.y"
        { ; }
#line 3122 "y.tab.c"
    break;

  case 50:
#line 985 "lang.y"
        {
	    add_local_name((yyvsp[0].string), current_type | (yyvsp[-1].number));
	}
#line 3130 "y.tab.c"
    break;

  case 56:
#line 997 "lang.y"
        {
	    ins_f_byte(F_POP_VALUE);
	    if (d_flag)
		ins_f_byte(F_BREAK_POINT);
	    /* if (exact_types && !TYPE($1,TYPE_VOID))
		yyerror("Value thrown away"); */
	}
#line 3142 "y.tab.c"
    break;

  case 67:
#line 1008 "lang.y"
                {
		    if (current_break_address == 0)
			yyerror("break statement outside loop");
		    if (current_break_address & BREAK_ON_STACK) {
			ins_f_byte(F_BREAK);
		    } else {
		        ins_f_byte(F_JUMP); ins_short(current_break_address);
		    }
		}
#line 3156 "y.tab.c"
    break;

  case 68:
#line 1018 "lang.y"
                {
		    if (current_continue_address == 0)
			yyerror("continue statement outside loop");
		    ins_f_byte(F_JUMP); ins_short(current_continue_address);
		}
#line 3166 "y.tab.c"
    break;

  case 69:
#line 1025 "lang.y"
        {   push_explicit(current_continue_address);
	    push_explicit(current_break_address);
	    current_continue_address = mem_block[A_PROGRAM].current_size;
	}
#line 3175 "y.tab.c"
    break;

  case 70:
#line 1029 "lang.y"
        {
	    ins_f_byte(F_JUMP_WHEN_NON_ZERO); ins_short(0);	/* to block */
	    current_break_address = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_JUMP); ins_short(0);	/* Exit loop */
	    upd_short(current_break_address-2,
		      mem_block[A_PROGRAM].current_size);
	}
#line 3187 "y.tab.c"
    break;

  case 71:
#line 1037 "lang.y"
        {
	  ins_f_byte(F_JUMP); ins_short(current_continue_address);
	  upd_short(current_break_address+1,
		    mem_block[A_PROGRAM].current_size);
	  current_break_address = pop_address();
	  current_continue_address = pop_address();
        }
#line 3199 "y.tab.c"
    break;

  case 72:
#line 1045 "lang.y"
    {
        int tmp_save;
        push_explicit(current_continue_address);
	push_explicit(current_break_address);
	/* Jump to start of loop. */
	ins_f_byte(F_JUMP); tmp_save = mem_block[A_PROGRAM].current_size;
	ins_short(0);
	current_break_address = mem_block[A_PROGRAM].current_size;
	/* Jump to end of loop. All breaks go through this one. */
	ins_f_byte(F_JUMP); push_address(); ins_short(0);
	current_continue_address = mem_block[A_PROGRAM].current_size;
	upd_short(tmp_save, current_continue_address);
        push_address();
	
    }
#line 3219 "y.tab.c"
    break;

  case 73:
#line 1060 "lang.y"
    {
	ins_f_byte(F_JUMP_WHEN_NON_ZERO); ins_short(pop_address());
	/* Fill in the break jump address in the beginning of the loop. */
	upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	current_break_address = pop_address();
	current_continue_address = pop_address();
    }
#line 3231 "y.tab.c"
    break;

  case 74:
#line 1068 "lang.y"
                  { push_explicit(current_continue_address);
		    push_explicit(current_break_address); }
#line 3238 "y.tab.c"
    break;

  case 75:
#line 1070 "lang.y"
                  {   ins_f_byte(F_POP_VALUE);
		      push_address();
		  }
#line 3246 "y.tab.c"
    break;

  case 76:
#line 1073 "lang.y"
                  {
		    ins_f_byte(F_JUMP_WHEN_NON_ZERO);
		    ins_short(0);	/* Jump to block of block */
		    current_break_address = mem_block[A_PROGRAM].current_size;
		    ins_f_byte(F_JUMP); ins_short(0);	/* Out of loop */
	 	    current_continue_address =
			mem_block[A_PROGRAM].current_size;
		  }
#line 3259 "y.tab.c"
    break;

  case 77:
#line 1081 "lang.y"
                  {
	 	    ins_f_byte(F_POP_VALUE);
		    ins_f_byte(F_JUMP); ins_short(pop_address());
		    /* Here starts the block. */
		    upd_short(current_break_address-2,
			      mem_block[A_PROGRAM].current_size);
		  }
#line 3271 "y.tab.c"
    break;

  case 78:
#line 1089 "lang.y"
   {
       ins_f_byte(F_JUMP); ins_short(current_continue_address);
       /* Now, the address of the end of the block is known. */
       upd_short(current_break_address+1, mem_block[A_PROGRAM].current_size);
       current_break_address = pop_address();
       current_continue_address = pop_address();
   }
#line 3283 "y.tab.c"
    break;

  case 79:
#line 1097 "lang.y"
                      { ins_f_byte(F_CONST1); }
#line 3289 "y.tab.c"
    break;

  case 81:
#line 1101 "lang.y"
    {
        current_break_stack_need += sizeof(short);
        if ( current_break_stack_need > max_break_stack_need )
            max_break_stack_need = current_break_stack_need;
	push_explicit(current_case_number_heap);
	push_explicit(current_case_string_heap);
	push_explicit(zero_case_label);
	push_explicit(current_break_address);
	ins_f_byte(F_SWITCH);
	ins_byte(0xff); /* kind of table */
	current_case_number_heap = mem_block[A_CASE_NUMBERS].current_size;
	current_case_string_heap = mem_block[A_CASE_STRINGS].current_size;
	zero_case_label = NO_STRING_CASE_LABELS;
	ins_short(0); /* address of table */
	current_break_address = mem_block[A_PROGRAM].current_size |
				BREAK_ON_STACK | BREAK_FROM_CASE ;
	ins_short(0); /* break address to push, table is entered before */
	ins_short(0); /* default address */
    }
#line 3313 "y.tab.c"
    break;

  case 82:
#line 1121 "lang.y"
    {
	char *heap_start;
	int heap_end_offs;
	int i,o;
	int current_key,last_key;
	/* int size_without_table; */
	int block_index;
	int current_case_heap;
	int lookup_start;
	int lookup_start_key;

	current_break_address &= ~(BREAK_ON_STACK|BREAK_FROM_CASE);

	if ( !read_short(current_break_address+2 ) )
	    upd_short(current_break_address+2,     /* no default given ->  */
	      mem_block[A_PROGRAM].current_size);  /* create one           */

	/* it isn't unusual that the last case/default has no break */
	ins_f_byte(F_BREAK);
	if(zero_case_label & (NO_STRING_CASE_LABELS|SOME_NUMERIC_CASE_LABELS)){
	    block_index = A_CASE_NUMBERS;
	    current_case_heap = current_case_number_heap;
	} else {
	    block_index = A_CASE_STRINGS;
	    current_case_heap = current_case_string_heap;
	    if (zero_case_label&0xffff) {
		struct case_heap_entry temp;

		temp.key = ZERO_AS_STR_CASE_LABEL;
		temp.addr = zero_case_label;
		temp.line = 0; /* if this is accessed later, something is
				* really wrong				  */
		add_to_case_heap(A_CASE_STRINGS,&temp);
	    }
	}
	heap_start = mem_block[block_index].block + current_case_heap ;
	heap_end_offs = mem_block[block_index].current_size -current_case_heap;
	if (!heap_end_offs) yyerror("switch without case not supported");

        /* add a dummy entry so that we can always
        * assume we have no or two childs
        */
        add_to_mem_block(block_index, "\0\0\0\0\0\0\0\0",
            sizeof(struct case_heap_entry) );

        /* read out the heap and build a sorted table */
	/* the table could be optimized better, but let's first see
	* how much switch is used at all when it is full-featured...
	*/
	mem_block[A_CASE_LABELS].current_size = 0;
	lookup_start = 0;
	lookup_start_key = ((struct case_heap_entry*)heap_start)->key;
        for( ; ((struct case_heap_entry*)heap_start)->addr; )
        {
            int offset;
	    int curr_line,last_line;
	    unsigned short current_addr,last_addr = 0xffff;
	    int range_start;

            current_key = ((struct case_heap_entry*)heap_start)->key ;
            curr_line = ((struct case_heap_entry*)heap_start)->line ;
            current_addr = ((struct case_heap_entry*)heap_start)->addr ;
            if ( current_key == last_key &&
              mem_block[A_CASE_LABELS].current_size )
            {
                char buf[90];

                sprintf(buf,"Duplicate case in line %d and %d",
		    last_line, curr_line);
                yyerror(buf);
            }
	    if (curr_line) {
		if (last_addr == 1) {
                    char buf[120];
    
		    sprintf(buf,
"Discontinued case label list range, line %d by line %d",
		      last_line, curr_line);
                    yyerror(buf);
		}
		  else if (current_key == last_key + 1
		    && current_addr == last_addr) {
		    if (mem_block[A_CASE_LABELS].current_size
		      != range_start + 6) {
		      *(short*)(mem_block[A_CASE_LABELS].block+range_start+4)
			=1;
		      mem_block[A_CASE_LABELS].current_size = range_start + 6;
		    }
		} else {
		    range_start = mem_block[A_CASE_LABELS].current_size;
		}
	    }
            last_key = current_key;
	    last_line = curr_line;
	    last_addr = current_addr;
	    add_to_mem_block(A_CASE_LABELS,
                (char *)&current_key, sizeof(long) );
	    add_to_mem_block(A_CASE_LABELS,
		(char *)&current_addr, sizeof(short) );
            for ( offset = 0; ; )
            {

                int child1,child2;

                child1 = ( offset << 1 ) + sizeof(struct case_heap_entry);
                child2 = child1 + sizeof(struct case_heap_entry);
                if ( child1 >= heap_end_offs ) break;
                if ( ((struct case_heap_entry*)(heap_start+child1))->addr &&
                  ( !((struct case_heap_entry*)(heap_start+child2))->addr ||
                   ((struct case_heap_entry*)(heap_start+child1))->key <=
                   ((struct case_heap_entry*)(heap_start+child2))->key  ) )
                {
                    *(struct case_heap_entry*)(heap_start+offset) =
                    *(struct case_heap_entry*)(heap_start+child1);
                    offset = child1;
                } else
                    if (((struct case_heap_entry*)(heap_start+child2))->addr ) {
                        *(struct case_heap_entry*)(heap_start+offset) =
                        *(struct case_heap_entry*)(heap_start+child2);
                        offset = child2;
                    } else break;
            }
            ((struct case_heap_entry*)(heap_start+offset))->addr = 0;
        }

	/* write start of table */
        upd_short(current_break_address-2,
            mem_block[A_PROGRAM].current_size);

	add_to_mem_block(A_PROGRAM, mem_block[A_CASE_LABELS].block,
            mem_block[A_CASE_LABELS].current_size );
        /* calculate starting index for itarative search at execution time */
        for(i=0xf0,o=6; o<<1 <= mem_block[A_CASE_LABELS].current_size; )
            i++,o<<=1;
        if (block_index == A_CASE_STRINGS) i = ( i << 4 ) | 0xf;
        /* and store it */
        mem_block[A_PROGRAM].block[current_break_address-3] &= i;
#if 0  /* neither the code for ordinary switch is fully debugged now,
	* nor is the code for packed switch tables complete */
	d = ((struct case_heap_entry*)heap_start)->key;
	if ( (r-d)*sizeof(short) < heap_end_offs ) {
	    mem_block[A_PROGRAM].block[current_break_address-3] &= 0xfe;
            upd_short(current_break_address-2, mem_block[A_PROGRAM].current_size);
            size_without_table = mem_block[A_PROGRAM].current_size;
	    r = heap_end_offs / sizeof(struct case_heap_entry);
	    add_to_mem_block(A_PROGRAM,mem_block[A_PROGRAM]->block,
		r * sizeof(short) );
	    memset(mem_block[A_PROGRAM]->block+size_without_table,
		'\0',r * sizeof(short) );
	    ins_long( d );
	    for(; --r; heap_start += sizeof(struct case_heap_entry) )
	    {
		upd_short(size_without_table + sizeof(short)*
                    ( ((struct case_heap_entry*)heap_start)->key - d )
		  , ((struct case_heap_entry*)heap_start)->addr );
	    }
        }
#endif /* 0 */
	upd_short(current_break_address, mem_block[A_PROGRAM].current_size);
	
	mem_block[A_CASE_NUMBERS].current_size = current_case_number_heap;
	mem_block[A_CASE_STRINGS].current_size = current_case_string_heap;
    	current_break_address = pop_address();
	zero_case_label = pop_address();
    	current_case_string_heap = pop_address();
    	current_case_number_heap = pop_address();
        current_break_stack_need -= sizeof(short);
    }
#line 3486 "y.tab.c"
    break;

  case 83:
#line 1291 "lang.y"
    {
	struct case_heap_entry temp;

	if ( !( current_break_address & BREAK_FROM_CASE ) ) {
	    yyerror("Case outside switch");
	    break;
	}
	temp.key = (yyvsp[-1].case_label).key;
	temp.addr = mem_block[A_PROGRAM].current_size;
	temp.line = current_line;
	add_to_case_heap((yyvsp[-1].case_label).block,&temp);
    }
#line 3503 "y.tab.c"
    break;

  case 84:
#line 1304 "lang.y"
    {
	struct case_heap_entry temp;

	if ( (yyvsp[-3].case_label).block != A_CASE_NUMBERS || (yyvsp[-1].case_label).block != A_CASE_NUMBERS )
	    yyerror("String case labels not allowed as range bounds");
	if ((yyvsp[-3].case_label).key > (yyvsp[-1].case_label).key) break;
	temp.key = (yyvsp[-3].case_label).key;
	temp.addr = 1;
	temp.line = current_line;
	add_to_case_heap(A_CASE_NUMBERS,&temp);
	temp.key = (yyvsp[-1].case_label).key;
	temp.addr = mem_block[A_PROGRAM].current_size;
	temp.line = 0;
	add_to_case_heap(A_CASE_NUMBERS,&temp);
    }
#line 3523 "y.tab.c"
    break;

  case 85:
#line 1321 "lang.y"
        {
	    if ( !(zero_case_label & NO_STRING_CASE_LABELS) )
		yyerror("Mixed case label list not allowed");
	    if ( ( (yyval.case_label).key = (yyvsp[0].number)) )
	        zero_case_label |= SOME_NUMERIC_CASE_LABELS;
	    else
		zero_case_label |= mem_block[A_PROGRAM].current_size;
	    (yyval.case_label).block = A_CASE_NUMBERS;
	}
#line 3537 "y.tab.c"
    break;

  case 86:
#line 1331 "lang.y"
        {
	    if ( zero_case_label & SOME_NUMERIC_CASE_LABELS )
		yyerror("Mixed case label list not allowed");
	    zero_case_label &= ~NO_STRING_CASE_LABELS;
            store_prog_string((yyvsp[0].string));
            (yyval.case_label).key = (int)(yyvsp[0].string);
	    (yyval.case_label).block = A_CASE_STRINGS;
        }
#line 3550 "y.tab.c"
    break;

  case 88:
#line 1342 "lang.y"
                              { (yyval.number) = (yyvsp[-2].number) | (yyvsp[0].number); }
#line 3556 "y.tab.c"
    break;

  case 90:
#line 1345 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) ^ (yyvsp[0].number); }
#line 3562 "y.tab.c"
    break;

  case 92:
#line 1348 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) & (yyvsp[0].number); }
#line 3568 "y.tab.c"
    break;

  case 94:
#line 1351 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) == (yyvsp[0].number); }
#line 3574 "y.tab.c"
    break;

  case 95:
#line 1352 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) != (yyvsp[0].number); }
#line 3580 "y.tab.c"
    break;

  case 97:
#line 1355 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) >  (yyvsp[0].number); }
#line 3586 "y.tab.c"
    break;

  case 98:
#line 1356 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) >= (yyvsp[0].number); }
#line 3592 "y.tab.c"
    break;

  case 99:
#line 1357 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) <  (yyvsp[0].number); }
#line 3598 "y.tab.c"
    break;

  case 100:
#line 1358 "lang.y"
                           { (yyval.number) = (yyvsp[-2].number) <= (yyvsp[0].number); }
#line 3604 "y.tab.c"
    break;

  case 102:
#line 1361 "lang.y"
                            { (yyval.number) = (yyvsp[-2].number) << (yyvsp[0].number); }
#line 3610 "y.tab.c"
    break;

  case 103:
#line 1362 "lang.y"
                            { (yyval.number) = (yyvsp[-2].number) >> (yyvsp[0].number); }
#line 3616 "y.tab.c"
    break;

  case 105:
#line 1365 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) + (yyvsp[0].number); }
#line 3622 "y.tab.c"
    break;

  case 106:
#line 1366 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) - (yyvsp[0].number); }
#line 3628 "y.tab.c"
    break;

  case 108:
#line 1369 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) * (yyvsp[0].number); }
#line 3634 "y.tab.c"
    break;

  case 109:
#line 1370 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) % (yyvsp[0].number); }
#line 3640 "y.tab.c"
    break;

  case 110:
#line 1371 "lang.y"
                          { (yyval.number) = (yyvsp[-2].number) / (yyvsp[0].number); }
#line 3646 "y.tab.c"
    break;

  case 112:
#line 1374 "lang.y"
                         { (yyval.number) = (yyvsp[-1].number); }
#line 3652 "y.tab.c"
    break;

  case 114:
#line 1377 "lang.y"
                       { (yyval.number) = -(yyvsp[0].number); }
#line 3658 "y.tab.c"
    break;

  case 115:
#line 1378 "lang.y"
                       { (yyval.number) = !(yyvsp[0].number); }
#line 3664 "y.tab.c"
    break;

  case 116:
#line 1379 "lang.y"
                       { (yyval.number) = ~(yyvsp[0].number); }
#line 3670 "y.tab.c"
    break;

  case 117:
#line 1382 "lang.y"
    {
	if ( !( current_break_address & BREAK_FROM_CASE ) ) {
	    yyerror("Default outside switch");
	    break;
	}
	current_break_address &= ~(BREAK_ON_STACK|BREAK_FROM_CASE);
	if ( read_short(current_break_address+2 ) )
	    yyerror("Duplicate default");
	upd_short(current_break_address+2, mem_block[A_PROGRAM].current_size);
	current_break_address |= (BREAK_ON_STACK|BREAK_FROM_CASE);
    }
#line 3686 "y.tab.c"
    break;

  case 118:
#line 1395 "lang.y"
                  { (yyval.type) = (yyvsp[0].type); }
#line 3692 "y.tab.c"
    break;

  case 119:
#line 1396 "lang.y"
                       { ins_f_byte(F_POP_VALUE); }
#line 3698 "y.tab.c"
    break;

  case 120:
#line 1398 "lang.y"
        { (yyval.type) = (yyvsp[0].type); }
#line 3704 "y.tab.c"
    break;

  case 122:
#line 1402 "lang.y"
        {
	    if (exact_types && !compatible_types((yyvsp[-2].type), (yyvsp[0].type)) &&
		!((yyvsp[-2].type) == TYPE_STRING && (yyvsp[0].type) == TYPE_NUMBER && (yyvsp[-1].number) == F_ADD_EQ))
	    {
		type_error("Bad assignment. Rhs", (yyvsp[0].type));
	    }
	    ins_f_byte((yyvsp[-1].number));
	    (yyval.type) = (yyvsp[0].type);
	}
#line 3718 "y.tab.c"
    break;

  case 123:
#line 1411 "lang.y"
                           { yyerror("Illegal LHS");  (yyval.type) = TYPE_ANY; }
#line 3724 "y.tab.c"
    break;

  case 124:
#line 1413 "lang.y"
              { (yyval.type) = (yyvsp[0].type); }
#line 3730 "y.tab.c"
    break;

  case 125:
#line 1415 "lang.y"
        {
	    ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	}
#line 3740 "y.tab.c"
    break;

  case 126:
#line 1421 "lang.y"
        {
	    int i;
	    i = pop_address();
	    ins_f_byte(F_JUMP); push_address(); ins_short(0);
	    upd_short(i, mem_block[A_PROGRAM].current_size);
	}
#line 3751 "y.tab.c"
    break;

  case 127:
#line 1428 "lang.y"
        {
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if (exact_types && !compatible_types((yyvsp[-3].type), (yyvsp[0].type))) {
		type_error("Different types in ?: expr", (yyvsp[-3].type));
		type_error("                      and ", (yyvsp[0].type));
	    }
	    if ((yyvsp[-3].type) == TYPE_ANY) (yyval.type) = (yyvsp[0].type);
	    else if ((yyvsp[0].type) == TYPE_ANY) (yyval.type) = (yyvsp[-3].type);
	    else if (TYPE((yyvsp[-3].type), TYPE_MOD_POINTER|TYPE_ANY)) (yyval.type) = (yyvsp[0].type);
	    else if (TYPE((yyvsp[0].type), TYPE_MOD_POINTER|TYPE_ANY)) (yyval.type) = (yyvsp[-3].type);
	    else (yyval.type) = (yyvsp[-3].type);
	}
#line 3768 "y.tab.c"
    break;

  case 128:
#line 1441 "lang.y"
            { (yyval.number) = F_ASSIGN; }
#line 3774 "y.tab.c"
    break;

  case 129:
#line 1442 "lang.y"
                 { (yyval.number) = F_AND_EQ; }
#line 3780 "y.tab.c"
    break;

  case 130:
#line 1443 "lang.y"
                { (yyval.number) = F_OR_EQ; }
#line 3786 "y.tab.c"
    break;

  case 131:
#line 1444 "lang.y"
                 { (yyval.number) = F_XOR_EQ; }
#line 3792 "y.tab.c"
    break;

  case 132:
#line 1445 "lang.y"
                 { (yyval.number) = F_LSH_EQ; }
#line 3798 "y.tab.c"
    break;

  case 133:
#line 1446 "lang.y"
                 { (yyval.number) = F_RSH_EQ; }
#line 3804 "y.tab.c"
    break;

  case 134:
#line 1447 "lang.y"
                 { (yyval.number) = F_ADD_EQ; }
#line 3810 "y.tab.c"
    break;

  case 135:
#line 1448 "lang.y"
                 { (yyval.number) = F_SUB_EQ; }
#line 3816 "y.tab.c"
    break;

  case 136:
#line 1449 "lang.y"
                  { (yyval.number) = F_MULT_EQ; }
#line 3822 "y.tab.c"
    break;

  case 137:
#line 1450 "lang.y"
                 { (yyval.number) = F_MOD_EQ; }
#line 3828 "y.tab.c"
    break;

  case 138:
#line 1451 "lang.y"
                 { (yyval.number) = F_DIV_EQ; }
#line 3834 "y.tab.c"
    break;

  case 139:
#line 1454 "lang.y"
        {
	    if (exact_types && !TYPE(exact_types, TYPE_VOID))
		type_error("Must return a value for a function declared",
			   exact_types);
	    ins_f_byte(F_CONST0);
	    ins_f_byte(F_RETURN);
	}
#line 3846 "y.tab.c"
    break;

  case 140:
#line 1462 "lang.y"
        {
	    if (exact_types && !TYPE((yyvsp[0].type), exact_types & TYPE_MOD_MASK))
		type_error("Return type not matching", exact_types);
	    ins_f_byte(F_RETURN);
	}
#line 3856 "y.tab.c"
    break;

  case 141:
#line 1468 "lang.y"
                                { (yyval.number) = 0; }
#line 3862 "y.tab.c"
    break;

  case 142:
#line 1469 "lang.y"
                                { (yyval.number) = (yyvsp[0].number); }
#line 3868 "y.tab.c"
    break;

  case 143:
#line 1470 "lang.y"
                                { (yyval.number) = (yyvsp[-1].number); }
#line 3874 "y.tab.c"
    break;

  case 144:
#line 1472 "lang.y"
                                { (yyval.number) = 1; add_arg_type((yyvsp[0].type)); }
#line 3880 "y.tab.c"
    break;

  case 145:
#line 1473 "lang.y"
                                { (yyval.number) = (yyvsp[-2].number) + 1; add_arg_type((yyvsp[0].type)); }
#line 3886 "y.tab.c"
    break;

  case 146:
#line 1475 "lang.y"
             { (yyval.type) = (yyvsp[0].type); }
#line 3892 "y.tab.c"
    break;

  case 147:
#line 1477 "lang.y"
        {
	    ins_f_byte(F_DUP); ins_f_byte(F_JUMP_WHEN_NON_ZERO);
	    push_address();
	    ins_short(0);
	    ins_f_byte(F_POP_VALUE);
	}
#line 3903 "y.tab.c"
    break;

  case 148:
#line 1484 "lang.y"
        {
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if ((yyvsp[-3].type) == (yyvsp[0].type))
		(yyval.type) = (yyvsp[-3].type);
	    else
		(yyval.type) = TYPE_ANY;	/* Return type can't be known */
	}
#line 3915 "y.tab.c"
    break;

  case 149:
#line 1492 "lang.y"
               { (yyval.type) = (yyvsp[0].type); }
#line 3921 "y.tab.c"
    break;

  case 150:
#line 1494 "lang.y"
        {
	    ins_f_byte(F_DUP); ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	    ins_f_byte(F_POP_VALUE);
	}
#line 3932 "y.tab.c"
    break;

  case 151:
#line 1501 "lang.y"
        {
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if ((yyvsp[-3].type) == (yyvsp[0].type))
		(yyval.type) = (yyvsp[-3].type);
	    else
		(yyval.type) = TYPE_ANY;	/* Return type can't be known */
	}
#line 3944 "y.tab.c"
    break;

  case 153:
#line 1511 "lang.y"
          {
	      if (exact_types && !TYPE((yyvsp[-2].type),TYPE_NUMBER))
		  type_error("Bad argument 1 to |", (yyvsp[-2].type));
	      if (exact_types && !TYPE((yyvsp[0].type),TYPE_NUMBER))
		  type_error("Bad argument 2 to |", (yyvsp[0].type));
	      (yyval.type) = TYPE_NUMBER;
	      ins_f_byte(F_OR);
	  }
#line 3957 "y.tab.c"
    break;

  case 155:
#line 1522 "lang.y"
          {
	      if (exact_types && !TYPE((yyvsp[-2].type),TYPE_NUMBER))
		  type_error("Bad argument 1 to ^", (yyvsp[-2].type));
	      if (exact_types && !TYPE((yyvsp[0].type),TYPE_NUMBER))
		  type_error("Bad argument 2 to ^", (yyvsp[0].type));
	      (yyval.type) = TYPE_NUMBER;
	      ins_f_byte(F_XOR);
	  }
#line 3970 "y.tab.c"
    break;

  case 157:
#line 1533 "lang.y"
          {
	      ins_f_byte(F_AND);
	      if ( !TYPE((yyvsp[-2].type),TYPE_MOD_POINTER) || !TYPE((yyvsp[0].type),TYPE_MOD_POINTER) ) {
	          if (exact_types && !TYPE((yyvsp[-2].type),TYPE_NUMBER))
		      type_error("Bad argument 1 to &", (yyvsp[-2].type));
	          if (exact_types && !TYPE((yyvsp[0].type),TYPE_NUMBER))
		      type_error("Bad argument 2 to &", (yyvsp[0].type));
	      }
	      (yyval.type) = TYPE_NUMBER;
	  }
#line 3985 "y.tab.c"
    break;

  case 159:
#line 1546 "lang.y"
        {
	    int t1 = (yyvsp[-2].type) & TYPE_MOD_MASK, t2 = (yyvsp[0].type) & TYPE_MOD_MASK;
	    if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
		type_error("== always false because of different types", (yyvsp[-2].type));
		type_error("                               compared to", (yyvsp[0].type));
	    }
	    ins_f_byte(F_EQ);
	    (yyval.type) = TYPE_NUMBER;
	}
#line 3999 "y.tab.c"
    break;

  case 160:
#line 1556 "lang.y"
        {
	    int t1 = (yyvsp[-2].type) & TYPE_MOD_MASK, t2 = (yyvsp[0].type) & TYPE_MOD_MASK;
	    if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
		type_error("!= always true because of different types", (yyvsp[-2].type));
		type_error("                               compared to", (yyvsp[0].type));
	    }
	    ins_f_byte(F_NE);
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4013 "y.tab.c"
    break;

  case 162:
#line 1568 "lang.y"
        { (yyval.type) = TYPE_NUMBER; ins_f_byte(F_GT); }
#line 4019 "y.tab.c"
    break;

  case 163:
#line 1570 "lang.y"
        { (yyval.type) = TYPE_NUMBER; ins_f_byte(F_GE); }
#line 4025 "y.tab.c"
    break;

  case 164:
#line 1572 "lang.y"
        { (yyval.type) = TYPE_NUMBER; ins_f_byte(F_LT); }
#line 4031 "y.tab.c"
    break;

  case 165:
#line 1574 "lang.y"
        { (yyval.type) = TYPE_NUMBER; ins_f_byte(F_LE); }
#line 4037 "y.tab.c"
    break;

  case 167:
#line 1578 "lang.y"
        {
	    ins_f_byte(F_LSH);
	    (yyval.type) = TYPE_NUMBER;
	    if (exact_types && !TYPE((yyvsp[-2].type), TYPE_NUMBER))
		type_error("Bad argument number 1 to '<<'", (yyvsp[-2].type));
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument number 2 to '<<'", (yyvsp[0].type));
	}
#line 4050 "y.tab.c"
    break;

  case 168:
#line 1587 "lang.y"
        {
	    ins_f_byte(F_RSH);
	    (yyval.type) = TYPE_NUMBER;
	    if (exact_types && !TYPE((yyvsp[-2].type), TYPE_NUMBER))
		type_error("Bad argument number 1 to '>>'", (yyvsp[-2].type));
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument number 2 to '>>'", (yyvsp[0].type));
	}
#line 4063 "y.tab.c"
    break;

  case 170:
#line 1598 "lang.y"
        { ins_f_byte(F_ADD); (yyval.type) = TYPE_ANY; }
#line 4069 "y.tab.c"
    break;

  case 171:
#line 1600 "lang.y"
        {
	    int bad_arg = 0;

	    if (exact_types) {
		if (!TYPE((yyvsp[-2].type), TYPE_NUMBER) && !((yyvsp[-2].type) & TYPE_MOD_POINTER) ) {
                    type_error("Bad argument number 1 to '-'", (yyvsp[-2].type));
		    bad_arg++;
		}
		if (!TYPE((yyvsp[0].type), TYPE_NUMBER) && !((yyvsp[0].type) & TYPE_MOD_POINTER) ) {
                    type_error("Bad argument number 2 to '-'", (yyvsp[0].type));
		    bad_arg++;
		}
	    }
	    (yyval.type) = TYPE_ANY;
	    if (((yyvsp[-2].type) & TYPE_MOD_POINTER) || ((yyvsp[0].type) & TYPE_MOD_POINTER))
		(yyval.type) = TYPE_MOD_POINTER | TYPE_ANY;
	    if (!((yyvsp[-2].type) & TYPE_MOD_POINTER) || !((yyvsp[0].type) & TYPE_MOD_POINTER)) {
		if (exact_types && (yyval.type) != TYPE_ANY && !bad_arg)
		    yyerror("Arguments to '-' don't match");
		(yyval.type) = TYPE_NUMBER;
	    }
	    ins_f_byte(F_SUBTRACT);
	}
#line 4097 "y.tab.c"
    break;

  case 173:
#line 1626 "lang.y"
        {
	    if (exact_types && !TYPE((yyvsp[-2].type), TYPE_NUMBER))
		type_error("Bad argument number 1 to '*'", (yyvsp[-2].type));
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument number 2 to '*'", (yyvsp[0].type));
	    ins_f_byte(F_MULTIPLY);
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4110 "y.tab.c"
    break;

  case 174:
#line 1635 "lang.y"
        {
	    if (exact_types && !TYPE((yyvsp[-2].type), TYPE_NUMBER))
		type_error("Bad argument number 1 to '%'", (yyvsp[-2].type));
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument number 2 to '%'", (yyvsp[0].type));
	    ins_f_byte(F_MOD);
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4123 "y.tab.c"
    break;

  case 175:
#line 1644 "lang.y"
        {
	    if (exact_types && !TYPE((yyvsp[-2].type), TYPE_NUMBER))
		type_error("Bad argument number 1 to '/'", (yyvsp[-2].type));
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument number 2 to '/'", (yyvsp[0].type));
	    ins_f_byte(F_DIVIDE);
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4136 "y.tab.c"
    break;

  case 177:
#line 1655 "lang.y"
              {
		  (yyval.type) = (yyvsp[-1].type);
		  if (exact_types && (yyvsp[0].type) != TYPE_ANY && (yyvsp[0].type) != TYPE_UNKNOWN &&
		      (yyvsp[-1].type) != TYPE_VOID)
		      type_error("Casts are only legal for type mixed, or when unknown", (yyvsp[0].type));
	      }
#line 4147 "y.tab.c"
    break;

  case 179:
#line 1664 "lang.y"
        {
	    ins_f_byte(F_INC);
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument to ++", (yyvsp[0].type));
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4158 "y.tab.c"
    break;

  case 180:
#line 1671 "lang.y"
        {
	    ins_f_byte(F_DEC);
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument to --", (yyvsp[0].type));
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4169 "y.tab.c"
    break;

  case 181:
#line 1678 "lang.y"
        {
	    ins_f_byte(F_NOT);	/* Any type is valid here. */
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4178 "y.tab.c"
    break;

  case 182:
#line 1683 "lang.y"
        {
	    ins_f_byte(F_COMPL);
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument to ~", (yyvsp[0].type));
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4189 "y.tab.c"
    break;

  case 183:
#line 1690 "lang.y"
        {
	    ins_f_byte(F_NEGATE);
	    if (exact_types && !TYPE((yyvsp[0].type), TYPE_NUMBER))
		type_error("Bad argument to unary '-'", (yyvsp[0].type));
	    (yyval.type) = TYPE_NUMBER;
	}
#line 4200 "y.tab.c"
    break;

  case 185:
#line 1699 "lang.y"
         {
	     ins_f_byte(F_POST_INC);
	     if (exact_types && !TYPE((yyvsp[-1].type), TYPE_NUMBER))
		 type_error("Bad argument to ++", (yyvsp[-1].type));
	     (yyval.type) = TYPE_NUMBER;
	 }
#line 4211 "y.tab.c"
    break;

  case 186:
#line 1706 "lang.y"
         {
	     ins_f_byte(F_POST_DEC);
	     if (exact_types && !TYPE((yyvsp[-1].type), TYPE_NUMBER))
		 type_error("Bad argument to --", (yyvsp[-1].type));
	     (yyval.type) = TYPE_NUMBER;
	 }
#line 4222 "y.tab.c"
    break;

  case 188:
#line 1715 "lang.y"
        {
	    int pos = mem_block[A_PROGRAM].current_size;
	    /* Some optimization. Replace the push-lvalue with push-value */
	    if (last_push_identifier == pos-2)
		mem_block[A_PROGRAM].block[last_push_identifier] =
		    F_IDENTIFIER - F_OFFSET;
	    else if (last_push_local == pos-2)
		mem_block[A_PROGRAM].block[last_push_local] =
		    F_LOCAL_NAME - F_OFFSET;
	    else if (last_push_indexed == pos-1)
		mem_block[A_PROGRAM].block[last_push_indexed] =
		    F_INDEX - F_OFFSET;
	    else if (last_push_indexed != 0)
		fatal("Should be a push at this point !\n");
	    (yyval.type) = (yyvsp[0].type);
	}
#line 4243 "y.tab.c"
    break;

  case 191:
#line 1732 "lang.y"
                          { (yyval.type) = (yyvsp[-1].type); }
#line 4249 "y.tab.c"
    break;

  case 192:
#line 1733 "lang.y"
             { (yyval.type) = TYPE_ANY; }
#line 4255 "y.tab.c"
    break;

  case 193:
#line 1734 "lang.y"
              { (yyval.type) = TYPE_NUMBER; }
#line 4261 "y.tab.c"
    break;

  case 194:
#line 1735 "lang.y"
                     { (yyval.type) = TYPE_NUMBER; }
#line 4267 "y.tab.c"
    break;

  case 195:
#line 1737 "lang.y"
       {
	   pop_arg_stack((yyvsp[-2].number));		/* We don't care about these types */
	   ins_f_byte(F_AGGREGATE);
	   ins_short((yyvsp[-2].number));
	   (yyval.type) = TYPE_MOD_POINTER | TYPE_ANY;
       }
#line 4278 "y.tab.c"
    break;

  case 196:
#line 1744 "lang.y"
               { ins_f_byte(F_CATCH); push_address(); ins_short(0);}
#line 4284 "y.tab.c"
    break;

  case 197:
#line 1746 "lang.y"
               {
		   ins_f_byte(F_POP_VALUE);
#if 1
		   ins_f_byte(F_CONST0);
		   ins_f_byte(F_THROW);
#else
		   ins_f_byte(F_RETURN);
#endif
		   upd_short(pop_address(),
			     mem_block[A_PROGRAM].current_size);
	       }
#line 4300 "y.tab.c"
    break;

  case 198:
#line 1759 "lang.y"
        {
	    ins_f_byte(F_SSCANF); ins_byte((yyvsp[-1].number) + 2);
	}
#line 4308 "y.tab.c"
    break;

  case 199:
#line 1764 "lang.y"
        {
	    ins_f_byte(F_PARSE_COMMAND); ins_byte((yyvsp[-1].number) + 3);
	}
#line 4316 "y.tab.c"
    break;

  case 200:
#line 1768 "lang.y"
                         { (yyval.number) = 0; }
#line 4322 "y.tab.c"
    break;

  case 201:
#line 1769 "lang.y"
                                    { (yyval.number) = 1 + (yyvsp[0].number); }
#line 4328 "y.tab.c"
    break;

  case 202:
#line 1772 "lang.y"
        {
	    int i = verify_declared((yyvsp[0].string));
	    last_push_identifier = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_PUSH_IDENTIFIER_LVALUE);
	    ins_byte(i);
	    free((yyvsp[0].string));
	    if (i == -1)
		(yyval.type) = TYPE_ANY;
	    else
		(yyval.type) = VARIABLE(i)->type & TYPE_MOD_MASK;
	}
#line 4344 "y.tab.c"
    break;

  case 203:
#line 1784 "lang.y"
        {
	    last_push_local = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
	    ins_byte((yyvsp[0].number));
	    (yyval.type) = type_of_locals[(yyvsp[0].number)];
	}
#line 4355 "y.tab.c"
    break;

  case 204:
#line 1791 "lang.y"
          {
	      ins_f_byte(F_RANGE);
	      last_push_indexed = 0;
	      if (exact_types) {
		  if (((yyvsp[-5].type) & TYPE_MOD_POINTER) == 0 && !TYPE((yyvsp[-5].type), TYPE_STRING))
		      type_error("Bad type to indexed value", (yyvsp[-5].type));
		  if (!TYPE((yyvsp[-3].type), TYPE_NUMBER))
		      type_error("Bad type of index", (yyvsp[-3].type));
		  if (!TYPE((yyvsp[-1].type), TYPE_NUMBER))
		      type_error("Bad type of index", (yyvsp[-1].type));
	      }
	      if ((yyvsp[-5].type) == TYPE_ANY)
		  (yyval.type) = TYPE_ANY;
	      else if (TYPE((yyvsp[-5].type), TYPE_STRING))
		  (yyval.type) = TYPE_STRING;
	      else if ((yyvsp[-5].type) & TYPE_MOD_POINTER)
		  (yyval.type) = (yyvsp[-5].type);
	      else if (exact_types)
		  type_error("Bad type of argument used for range", (yyvsp[-5].type));
	  }
#line 4380 "y.tab.c"
    break;

  case 205:
#line 1812 "lang.y"
          {
	      last_push_indexed = mem_block[A_PROGRAM].current_size;
	      ins_f_byte(F_PUSH_INDEXED_LVALUE);
	      if (exact_types) {
		  if (((yyvsp[-3].type) & TYPE_MOD_POINTER) == 0 && !TYPE((yyvsp[-3].type), TYPE_STRING))
		      type_error("Bad type to indexed value", (yyvsp[-3].type));
		  if (!TYPE((yyvsp[-1].type), TYPE_NUMBER))
		      type_error("Bad type of index", (yyvsp[-1].type));
	      }
	      if ((yyvsp[-3].type) == TYPE_ANY)
		  (yyval.type) = TYPE_ANY;
	      else if (TYPE((yyvsp[-3].type), TYPE_STRING))
		  (yyval.type) = TYPE_NUMBER;
	      else
		  (yyval.type) = (yyvsp[-3].type) & TYPE_MOD_MASK & ~TYPE_MOD_POINTER;
	  }
#line 4401 "y.tab.c"
    break;

  case 206:
#line 1830 "lang.y"
        {
	    ins_f_byte(F_STRING);
	    ins_short(store_prog_string((yyvsp[0].string)));
	    free((yyvsp[0].string));
	    (yyval.type) = TYPE_STRING;
	}
#line 4412 "y.tab.c"
    break;

  case 207:
#line 1838 "lang.y"
        {
            char *p = make_shared_string((yyvsp[0].string));
            free((yyvsp[0].string));
            (yyval.string) = p;
        }
#line 4422 "y.tab.c"
    break;

  case 209:
#line 1846 "lang.y"
        {
	    (yyval.string) = xalloc( strlen((yyvsp[-2].string)) + strlen((yyvsp[0].string)) + 1 );
	    strcpy((yyval.string), (yyvsp[-2].string));
	    strcat((yyval.string), (yyvsp[0].string));
	    free((yyvsp[-2].string));
	    free((yyvsp[0].string));
	}
#line 4434 "y.tab.c"
    break;

  case 210:
#line 1855 "lang.y"
    {
	/* This seems to be an ordinary function call. But, if the function
	 * is not defined, then it might be a call to a simul_efun.
	 * If it is, then we make it a call_other(), which requires the
	 * function name as argument.
	 * We have to remember until after parsing the arguments if it was
	 * a simulated efun or not, which means that the pointer has to be
	 * pushed on a stack. Use the internal yacc stack for this purpose.
	 */
	(yyval.funp) = 0;
	if (defined_function((yyvsp[0].string)) == -1) {
	    char *p = make_shared_string((yyvsp[0].string));
	    (yyval.funp) = find_simul_efun(p);
	    if ((yyval.funp) && !((yyval.funp)->type & TYPE_MOD_STATIC)) {
		ins_f_byte(F_STRING);
		ins_short(store_prog_string(
	              query_simul_efun_file_name()));
		ins_f_byte(F_STRING);
		ins_short(store_prog_string(p));
	    } else {
		(yyval.funp) = 0;
	    }
	    free_string(p);
	}
    }
#line 4464 "y.tab.c"
    break;

  case 211:
#line 1881 "lang.y"
    { 
	int f;
	int efun_override = strncmp((yyvsp[-4].string), "efun::", 6) == 0;

	if ((yyvsp[-3].funp)) {
	    ins_f_byte(F_CALL_OTHER);
	    ins_byte((yyvsp[-1].number) + 2);
	    (yyval.type) = (yyvsp[-3].funp)->type;
	} else if (!efun_override && (f = defined_function((yyvsp[-4].string))) >= 0) {
	    struct function *funp;
	    ins_f_byte(F_CALL_FUNCTION_BY_ADDRESS); ins_short(f);
	    ins_byte((yyvsp[-1].number));	/* Actual number of arguments */
	    funp = FUNCTION(f);
	    if (funp->flags & NAME_UNDEFINED)
		find_inherited(funp);
	    /*
	     * Verify that the function has been defined already.
	     */
	    if ((funp->flags & NAME_UNDEFINED) &&
		!(funp->flags & NAME_PROTOTYPE) && exact_types)
	    {
		char buff[100];
		sprintf(buff, "Function %.50s undefined", funp->name);
		yyerror(buff);
	    }
	    (yyval.type) = funp->type & TYPE_MOD_MASK;
	    /*
	     * Check number of arguments.
	     */
	    if (funp->num_arg != (yyvsp[-1].number) && !(funp->type & TYPE_MOD_VARARGS) &&
		(funp->flags & NAME_STRICT_TYPES) && exact_types)
	    {
		char buff[100];
		sprintf(buff, "Wrong number of arguments to %.60s", (yyvsp[-4].string));
		yyerror(buff);
	    }
	    /*
	     * Check the argument types.
	     */
	    if (exact_types && *(unsigned short *)&mem_block[A_ARGUMENT_INDEX].block[f * sizeof (unsigned short)] != INDEX_START_NONE)
	    {
		int i, first;
		unsigned short *arg_types;
		
		arg_types = (unsigned short *)
		    mem_block[A_ARGUMENT_TYPES].block;
		first = *(unsigned short *)&mem_block[A_ARGUMENT_INDEX].block[f * sizeof (unsigned short)];
		for (i=0; i < funp->num_arg && i < (yyvsp[-1].number); i++) {
		    int tmp = get_argument_type(i, (yyvsp[-1].number));
		    if (!TYPE(tmp, arg_types[first + i])) {
			char buff[100];
			sprintf(buff, "Bad type for argument %d %s", i+1,
				get_two_types(arg_types[first+i], tmp));
			yyerror(buff);
		    }
		}
	    }
	} else if (efun_override || (f = lookup_predef((yyvsp[-4].string))) != -1) {
	    int min, max, def, *argp;
	    extern int efun_arg_types[];

	    if (efun_override) {
		f = lookup_predef((yyvsp[-4].string)+6);
	    }
	    if (f == -1) {	/* Only possible for efun_override */
		char buff[100];
		sprintf(buff, "Unknown efun: %s", (yyvsp[-4].string)+6);
		yyerror(buff);
	    } else {
		min = instrs[f-F_OFFSET].min_arg;
		max = instrs[f-F_OFFSET].max_arg;
		def = instrs[f-F_OFFSET].Default;
		(yyval.type) = instrs[f-F_OFFSET].ret_type;
		argp = &efun_arg_types[instrs[f-F_OFFSET].arg_index];
		if (def && (yyvsp[-1].number) == min-1) {
		    ins_f_byte(def);
		    max--;
		    min--;
		} else if ((yyvsp[-1].number) < min) {
		    char bff[100];
		    sprintf(bff, "Too few arguments to %s",
			    instrs[f-F_OFFSET].name);
		    yyerror(bff);
		} else if ((yyvsp[-1].number) > max && max != -1) {
		    char bff[100];
		    sprintf(bff, "Too many arguments to %s",
			    instrs[f-F_OFFSET].name);
		    yyerror(bff);
		} else if (max != -1 && exact_types) {
		    /*
		     * Now check all types of the arguments to efuns.
		     */
		    int i, argn;
		    char buff[100];
		    for (argn=0; argn < (yyvsp[-1].number); argn++) {
			int tmp = get_argument_type(argn, (yyvsp[-1].number));
			for(i=0; !TYPE(argp[i], tmp) && argp[i] != 0; i++)
			    ;
			if (argp[i] == 0) {
			    sprintf(buff, "Bad argument %d type to efun %s()",
				    argn+1, instrs[f-F_OFFSET].name);
			    yyerror(buff);
			}
			while(argp[i] != 0)
			    i++;
			argp += i + 1;
		    }
		}
		ins_f_byte(f);
		/* Only store number of arguments for instructions
		 * that allowed a variable number.
		 */
		if (max != min)
		    ins_byte((yyvsp[-1].number));/* Number of actual arguments */
	    }
	} else {
	    struct function *funp;

	    f = define_new_function((yyvsp[-4].string), 0, 0, 0, NAME_UNDEFINED, 0);
	    ins_f_byte(F_CALL_FUNCTION_BY_ADDRESS);
	    ins_short(f);
	    ins_byte((yyvsp[-1].number));	/* Number of actual arguments */
	    funp = FUNCTION(f);
	    if (strchr((yyvsp[-4].string), ':')) {
		/*
		 * A function defined by inheritance. Find
		 * real definition immediately.
		 */
		find_inherited(funp);
	    }
	    /*
	     * Check if this function has been defined.
	     * But, don't complain yet about functions defined
	     * by inheritance.
	     */
	    if (exact_types && (funp->flags & NAME_UNDEFINED)) {
		char buff[100];
		sprintf(buff, "Undefined function %.50s", (yyvsp[-4].string));
		yyerror(buff);
	    }
	    if (!(funp->flags & NAME_UNDEFINED))
		(yyval.type) = funp->type;
	    else
		(yyval.type) = TYPE_ANY;	/* Just a guess */
	}
	free((yyvsp[-4].string));
	pop_arg_stack((yyvsp[-1].number));	/* Argument types not needed more */
    }
#line 4617 "y.tab.c"
    break;

  case 212:
#line 2030 "lang.y"
    {
	ins_f_byte(F_STRING);
	ins_short(store_prog_string((yyvsp[0].string)));
	free((yyvsp[0].string));
    }
#line 4627 "y.tab.c"
    break;

  case 213:
#line 2036 "lang.y"
    {
	ins_f_byte(F_CALL_OTHER);
	ins_byte((yyvsp[-1].number) + 2);
	(yyval.type) = TYPE_UNKNOWN;
	pop_arg_stack((yyvsp[-1].number));	/* No good need of these arguments */
    }
#line 4638 "y.tab.c"
    break;

  case 215:
#line 2045 "lang.y"
                {
		    char *p = xalloc(strlen((yyvsp[0].string)) + 3);
		    strcpy(p, "::"); strcat(p, (yyvsp[0].string)); free((yyvsp[0].string));
		    (yyval.string) = p;
		}
#line 4648 "y.tab.c"
    break;

  case 216:
#line 2051 "lang.y"
                {
		    char *p = xalloc(strlen((yyvsp[-2].string)) + strlen((yyvsp[0].string)) + 3);
		    strcpy(p, (yyvsp[-2].string)); strcat(p, "::"); strcat(p, (yyvsp[0].string));
		    free((yyvsp[-2].string)); free((yyvsp[0].string));
		    (yyval.string) = p;
		}
#line 4659 "y.tab.c"
    break;

  case 217:
#line 2060 "lang.y"
        {
	    int i;
	    i = pop_address();
	    ins_f_byte(F_JUMP); push_address(); ins_short(0);
	    upd_short(i, mem_block[A_PROGRAM].current_size);
	}
#line 4670 "y.tab.c"
    break;

  case 218:
#line 2067 "lang.y"
        { upd_short(pop_address(), mem_block[A_PROGRAM].current_size); }
#line 4676 "y.tab.c"
    break;

  case 219:
#line 2070 "lang.y"
        {
	    ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	}
#line 4686 "y.tab.c"
    break;


#line 4690 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 2078 "lang.y"


void yyerror(const char* str)
{
    extern int num_parse_error;

    if (num_parse_error > 5)
	return;
    (void)fprintf(stderr, "%s: %s line %d\n", current_file, str,
		  current_line);
    fflush(stderr);
    smart_log(current_file, current_line, str);
    if (num_parse_error == 0)
	save_error(str, current_file, current_line);
    num_parse_error++;
}

static int check_declared(str)
    char *str;
{
    struct variable *vp;
    int offset;

    for (offset=0;
	 offset < mem_block[A_VARIABLES].current_size;
	 offset += sizeof (struct variable)) {
	vp = (struct variable *)&mem_block[A_VARIABLES].block[offset];
	if (vp->flags & NAME_HIDDEN)
	    continue;
	if (strcmp(vp->name, str) == 0)
	    return offset / sizeof (struct variable);
    }
    return -1;
}

static int verify_declared(str)
    char *str;
{
    int r;

    r = check_declared(str);
    if (r < 0) {
	char buff[100];
        (void)sprintf(buff, "Variable %s not declared !", str);
        yyerror(buff);
	return -1;
    }
    return r;
}

void free_all_local_names()
{
    int i;

    for (i=0; i<current_number_of_locals; i++) {
	free(local_names[i]);
	local_names[i] = 0;
    }
    current_number_of_locals = 0;
    current_break_stack_need = 0;
    max_break_stack_need = 0;
}

void add_local_name(str, type)
    char *str;
    int type;
{
    if (current_number_of_locals == MAX_LOCAL)
	yyerror("Too many local variables");
    else {
	type_of_locals[current_number_of_locals] = type;
	local_names[current_number_of_locals++] = str;
    }
}

/*
 * Copy all function definitions from an inherited object. They are added
 * as undefined, so that they can be redefined by a local definition.
 * If they are not redefined, then they will be updated, so that they
 * point to the inherited definition. See epilog(). Types will be copied
 * at that moment (if available).
 *
 * A call to an inherited function will not be
 * done through this entry (because this entry can be replaced by a new
 * definition). If an function defined by inheritance is called, then one
 * special definition will be made at first call.
 */
static int copy_functions(from, type)
    struct program *from;
    int type;
{
    int i, initializer = -1;
    unsigned short tmp_short;

    for (i=0; i < from->num_functions; i++) {
	/* Do not call define_new_function() from here, as duplicates would
	 * be removed.
	 */
	struct function fun;
	int new_type;

	fun = from->functions[i];	/* Make a copy */
	/* Prepare some data to be used if this function will not be
	 * redefined.
	 */
	if (strchr(fun.name, ':'))
	    fun.flags |= NAME_HIDDEN;	/* Not to be used again ! */
	fun.name = make_shared_string(fun.name);	/* Incr ref count */
	fun.offset = mem_block[A_INHERITS].current_size /
	    sizeof (struct inherit) - 1;
	fun.function_index_offset = i;
	if (fun.type & TYPE_MOD_NO_MASK) {
	    int n;
	    if ((n = defined_function(fun.name)) != -1 &&
		!(((struct function *)mem_block[A_FUNCTIONS].block)[n].flags &
		  NAME_UNDEFINED))
	    {
		char *p = (char *)alloca(80 + strlen(fun.name));
		sprintf(p, "Illegal to redefine 'nomask' function \"%s\"",
			fun.name);
		yyerror(p);
	    }
	    fun.flags |= NAME_INHERITED;
	} else if (!(fun.flags & NAME_HIDDEN)) {
	    fun.flags |= NAME_UNDEFINED;
	}
	/*
	 * public functions should not become private when inherited
	 * 'private'
	 */
	new_type = type;
	if (fun.type & TYPE_MOD_PUBLIC)
	    new_type &= ~TYPE_MOD_PRIVATE;
	fun.type |= new_type;
	/* marion
	 * this should make possible to inherit a heart beat function, and
	 * thus to mask it if wanted.
	 */
	if (heart_beat == -1 && fun.name[0] == 'h' &&
	    strcmp(fun.name, "heart_beat") == 0)
	{
	    heart_beat = mem_block[A_FUNCTIONS].current_size /
		sizeof (struct function);
	} else if (fun.name[0] == '_' && strcmp(fun.name, "__INIT") == 0) {
	    initializer = i;
	    fun.flags |= NAME_INHERITED;
	}
	add_to_mem_block(A_FUNCTIONS, (char *)&fun, sizeof fun);
	/*
	 * Copy information about the types of the arguments, if it is
	 * available.
	 */
	tmp_short = INDEX_START_NONE;	/* Presume not available. */
	if (from->type_start != 0 && from->type_start[i] != INDEX_START_NONE)
	{
	    int arg;
	    /*
	     * They are available for function number 'i'. Copy types of
	     * all arguments, and remember where they started.
	     */
	    tmp_short = mem_block[A_ARGUMENT_TYPES].current_size /
		sizeof from->argument_types[0];
	    for (arg = 0; arg < fun.num_arg; arg++) {
		add_to_mem_block(A_ARGUMENT_TYPES,
				 &from->argument_types[from->type_start[i]],
				 sizeof (unsigned short));
	    }
	}
	/*
	 * Save the index where they started. Every function will have an
	 * index where the type info of arguments starts.
	 */
	add_to_mem_block(A_ARGUMENT_INDEX, &tmp_short, sizeof tmp_short);
    }
    return initializer;
}

/*
 * Copy all variabel names from the object that is inherited from.
 * It is very important that they are stored in the same order with the
 * same index.
 */
static void copy_variables(from, type)
    struct program *from;
    int type;
{
    int i;

    for (i=0; i<from->num_variables; i++) {
	int new_type = type;
	int n = check_declared(from->variable_names[i].name);

	if (n != -1 && (VARIABLE(n)->type & TYPE_MOD_NO_MASK)) {
	    char *p = (char *)alloca(80 +
				     strlen(from->variable_names[i].name));
	    sprintf(p, "Illegal to redefine 'nomask' variable \"%s\"",
		    VARIABLE(n)->name);
	    yyerror(p);
	}
	/*
	 * 'public' variables should not become private when inherited
	 * 'private'.
	 */
	if (from->variable_names[i].type & TYPE_MOD_PUBLIC)
	    new_type &= ~TYPE_MOD_PRIVATE;
	define_variable(from->variable_names[i].name,
			from->variable_names[i].type | new_type,
			from->variable_names[i].type & TYPE_MOD_PRIVATE ?
			    NAME_HIDDEN : 0);
    }
}

/*
 * This function is called from lex.c for every new line read from the
 * "top" file (means not included files). Some new lines are missed,
 * as with #include statements, so it is compensated for.
 */
void store_line_number_info()
{
    unsigned short offset = mem_block[A_PROGRAM].current_size;

    while(mem_block[A_LINENUMBERS].current_size / sizeof (short) <
	  current_line)
    {
	add_to_mem_block(A_LINENUMBERS, (char *)&offset, sizeof offset);
    }
}

static char *get_type_name(type)
    int type;
{
    static char buff[100];
    static char *type_name[] = { "unknown", "int", "string",
				     "void", "object", "mixed", };
    int pointer = 0;

    buff[0] = 0;
    if (type & TYPE_MOD_STATIC)
	strcat(buff, "static ");
    if (type & TYPE_MOD_NO_MASK)
	strcat(buff, "nomask ");
    if (type & TYPE_MOD_PRIVATE)
	strcat(buff, "private ");
    if (type & TYPE_MOD_PROTECTED)
	strcat(buff, "protected ");
    if (type & TYPE_MOD_PUBLIC)
	strcat(buff, "public ");
    if (type & TYPE_MOD_VARARGS)
	strcat(buff, "varargs ");
    type &= TYPE_MOD_MASK;
    if (type & TYPE_MOD_POINTER) {
	pointer = 1;
	type &= ~TYPE_MOD_POINTER;
    }
    if (type >= sizeof type_name / sizeof type_name[0])
	fatal("Bad type\n");
    strcat(buff, type_name[type]);
    strcat(buff," ");
    if (pointer)
	strcat(buff, "* ");
    return buff;
}

void type_error(str, type)
    char *str;
    int type;
{
    static char buff[100];
    char *p;
    p = get_type_name(type);
    if (strlen(str) + strlen(p) + 5 >= sizeof buff) {
	yyerror(str);
    } else {
	strcpy(buff, str);
	strcat(buff, ": \"");
	strcat(buff, p);
	strcat(buff, "\"");
	yyerror(buff);
    }
}

/*
 * Compile an LPC file.
 */
void compile_file() {
    int yyparse();

    prolog();
    yyparse();
    epilog();
}

static char *get_two_types(type1, type2)
    int type1, type2;
{
    static char buff[100];

    strcpy(buff, "( ");
    strcat(buff, get_type_name(type1));
    strcat(buff, "vs ");
    strcat(buff, get_type_name(type2));
    strcat(buff, ")");
    return buff;
}

/*
 * The program has been compiled. Prepare a 'struct program' to be returned.
 */
void epilog() {
    int size, i;
    char *p;
    struct function *funp;
    static int current_id_number = 1;

#ifdef DEBUG
    if (num_parse_error == 0 && type_of_arguments.current_size != 0)
	fatal("Failed to deallocate argument type stack\n");
#endif
    /*
     * Define the __INIT function, but only if there was any code
     * to initialize.
     */
    if (first_last_initializer_end != last_initializer_end) {
	define_new_function("__INIT", 0, 0, 0, 0, 0);
	/*
	 * Change the last jump after the last initializer into a
	 * return(1) statement.
	 */
	mem_block[A_PROGRAM].block[last_initializer_end-1] =
	    F_CONST1 - F_OFFSET;
	mem_block[A_PROGRAM].block[last_initializer_end-0] =
	    F_RETURN - F_OFFSET;
    }

    /*
     * If functions are undefined, replace them by definitions done
     * by inheritance. All explicit "name::func" are already resolved.
     */
    for (i = 0; i < mem_block[A_FUNCTIONS].current_size; i += sizeof *funp) {
	funp = (struct function *)(mem_block[A_FUNCTIONS].block + i);
	if (!(funp->flags & NAME_UNDEFINED))
	    continue;
	find_inherited(funp);
    }
    if (num_parse_error > 0) {
	prog = 0;
	for (i=0; i<NUMAREAS; i++)
	    free(mem_block[i].block);
	return;
    }
    size = align(sizeof (struct program));
    for (i=0; i<NUMPAREAS; i++)
	size += align(mem_block[i].current_size);
    p = (char *)xalloc(size);
    prog = (struct program *)p;
    *prog = NULL_program;
    prog->total_size = size;
    prog->ref = 0;
    prog->heart_beat = heart_beat;
    prog->name = string_copy(current_file);
    prog->id_number = current_id_number++;
    total_prog_block_size += prog->total_size;
    total_num_prog_blocks += 1;

    p += align(sizeof (struct program));
    prog->program = p;
    if (mem_block[A_PROGRAM].current_size)
	memcpy(p, mem_block[A_PROGRAM].block,
	       mem_block[A_PROGRAM].current_size);
    prog->program_size = mem_block[A_PROGRAM].current_size;

    p += align(mem_block[A_PROGRAM].current_size);
    prog->line_numbers = (unsigned short *)p;
    if (mem_block[A_LINENUMBERS].current_size)
	memcpy(p, mem_block[A_LINENUMBERS].block,
	       mem_block[A_LINENUMBERS].current_size);

    p += align(mem_block[A_LINENUMBERS].current_size);
    prog->functions = (struct function *)p;
    prog->num_functions = mem_block[A_FUNCTIONS].current_size /
	sizeof (struct function);
    if (mem_block[A_FUNCTIONS].current_size)
	memcpy(p, mem_block[A_FUNCTIONS].block,
	       mem_block[A_FUNCTIONS].current_size);

    p += align(mem_block[A_FUNCTIONS].current_size);
    prog->strings = (char **)p;
    prog->num_strings = mem_block[A_STRINGS].current_size /
	sizeof (char *);
    if (mem_block[A_STRINGS].current_size)
	memcpy(p, mem_block[A_STRINGS].block,
	       mem_block[A_STRINGS].current_size);

    p += align(mem_block[A_STRINGS].current_size);
    prog->variable_names = (struct variable *)p;
    prog->num_variables = mem_block[A_VARIABLES].current_size /
	sizeof (struct variable);
    if (mem_block[A_VARIABLES].current_size)
	memcpy(p, mem_block[A_VARIABLES].block,
	       mem_block[A_VARIABLES].current_size);

    p += align(mem_block[A_VARIABLES].current_size);
    prog->num_inherited = mem_block[A_INHERITS].current_size /
	sizeof (struct inherit);
    if (prog->num_inherited) {
	memcpy(p, mem_block[A_INHERITS].block,
	       mem_block[A_INHERITS].current_size);
	prog->inherit = (struct inherit *)p;
    } else
	prog->inherit = 0;
    
    prog->argument_types = 0;	/* For now. Will be fixed someday */

    prog->type_start = 0;
    for (i=0; i<NUMAREAS; i++)
        free((char *)mem_block[i].block);

    /*  marion
	Do referencing here - avoid multiple referencing when an object
	inherits more than one object and one of the inherited is already
	loaded and not the last inherited
    */
    reference_prog (prog, "epilog");
    for (i = 0; i < prog->num_inherited; i++) {
	reference_prog (prog->inherit[i].prog, "inheritance");
    }
}

/*
 * Initialize the environment that the compiler needs.
 */
static void prolog() {
    int i;

    if (type_of_arguments.block == 0) {
	type_of_arguments.max_size = 100;
	type_of_arguments.block = xalloc(type_of_arguments.max_size);
    }
    type_of_arguments.current_size = 0;
    approved_object = 0;
    last_push_indexed = -1;
    last_push_local = -1;
    last_push_identifier = -1;
    prog = 0;		/* 0 means fail to load. */
    heart_beat = -1;
    comp_stackp = 0;	/* Local temp stack used by compiler */
    current_continue_address = 0;
    current_break_address = 0;
    num_parse_error = 0;
    free_all_local_names();	/* In case of earlier error */
    /* Initialize memory blocks where the result of the compilation
     * will be stored.
     */
    for (i=0; i < NUMAREAS; i++) {
	mem_block[i].block = xalloc(START_BLOCK_SIZE);
	mem_block[i].current_size = 0;
	mem_block[i].max_size = START_BLOCK_SIZE;
    }
    add_new_init_jump();
    first_last_initializer_end = last_initializer_end;
}

/*
 * Add a trailing jump after the last initialization code.
 */
void add_new_init_jump() {
    /*
     * Add a new jump.
     */
    ins_f_byte(F_JUMP);
    last_initializer_end = mem_block[A_PROGRAM].current_size;
    ins_short(0);
}
