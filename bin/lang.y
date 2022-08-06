%{
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
%}

/*
 * These values are used by the stack machine, and can not be directly
 * called from LPC.
 */
%token F_JUMP F_JUMP_WHEN_ZERO F_JUMP_WHEN_NON_ZERO
%token F_POP_VALUE F_DUP
%token F_STORE F_CALL_FUNCTION_BY_ADDRESS
%token F_PUSH_IDENTIFIER_LVALUE F_PUSH_LOCAL_VARIABLE_LVALUE
%token F_PUSH_INDEXED_LVALUE F_INDIRECT F_INDEX
%token F_CONST0 F_CONST1

/*
 * These are the predefined functions that can be accessed from LPC.
 */

%token F_IF F_IDENTIFIER F_LAND F_LOR F_STATUS
%token F_RETURN F_STRING
%token F_INC F_DEC
%token F_POST_INC F_POST_DEC F_COMMA
%token F_NUMBER F_ASSIGN F_INT F_ADD F_SUBTRACT F_MULTIPLY
%token F_DIVIDE F_LT F_GT F_EQ F_GE F_LE
%token F_NE
%token F_ADD_EQ F_SUB_EQ F_DIV_EQ F_MULT_EQ
%token F_NEGATE
%token F_SUBSCRIPT F_WHILE F_BREAK
%token F_DO F_FOR F_SWITCH
%token F_SSCANF F_PARSE_COMMAND F_STRING_DECL F_LOCAL_NAME
%token F_ELSE F_DESCRIBE
%token F_CONTINUE
%token F_MOD F_MOD_EQ F_INHERIT F_COLON_COLON
%token F_STATIC
%token F_ARROW F_AGGREGATE
%token F_COMPL F_AND F_AND_EQ F_OR F_OR_EQ F_XOR F_XOR_EQ
%token F_LSH F_LSH_EQ F_RSH F_RSH_EQ
%token F_CATCH
%token F_OBJECT F_VOID F_MIXED F_PRIVATE F_NO_MASK F_NOT
%token F_PROTECTED F_PUBLIC
%token F_VARARGS
%token F_ADD_ACTION
%token F_ADD_VERB
%token F_ADD_WORTH
%token F_ADD_XVERB
%token F_ALL_INVENTORY
%token F_ALLOCATE
%token F_ASSOC
%token F_BREAK_POINT
%token F_CALL_OTHER
%token F_CALL_OUT
%token F_CALL_OUT_INFO
%token F_CAPITALIZE
%token F_CAT
%token F_CINDENT
%token F_CLEAR_BIT
%token F_CLONE_OBJECT
%token F_COMMAND
%token F_CREATE_WIZARD
%token F_CREATOR
%token F_CRYPT
%token F_CTIME
%token F_DEBUG_INFO
%token F_DEEP_INVENTORY
%token F_DESTRUCT
%token F_DISABLE_COMMANDS
%token F_ED
%token F_ENABLE_COMMANDS
%token F_ENVIRONMENT
%token F_EXEC
%token F_EXPLODE
%token F_EXTRACT
%token F_FILE_NAME
%token F_FILE_SIZE
%token F_FILTER_ARRAY
%token F_FIND_CALL_OUT
%token F_FIND_LIVING
%token F_FIND_OBJECT
%token F_FIND_PLAYER
%token F_FIRST_INVENTORY
%token F_FUNCTION_EXISTS
%token F_GET_DIR
%token F_IMPLODE
%token F_INHERIT_LIST
%token F_INPUT_TO
%token F_INSERT_ALIST
%token F_INTERACTIVE
%token F_INTERSECT_ALIST
%token F_INTP
%token F_LIVING
%token F_LOCALCMD
%token F_LOG_FILE
%token F_LOWER_CASE
%token F_MAP_ARRAY
%token F_MEMBER_ARRAY
%token F_MKDIR
%token F_MOVE_OBJECT
%token F_NEXT_INVENTORY
%token F_NOTIFY_FAIL
%token F_OBJECTP
%token F_ORDER_ALIST
%token F_POINTERP
%token F_PRESENT
%token F_PREVIOUS_OBJECT
%token F_PROCESS_STRING
%token F_QUERY_HOST_NAME
%token F_QUERY_IDLE
%token F_QUERY_IP_NAME
%token F_QUERY_IP_NUMBER
%token F_QUERY_LOAD_AVERAGE
%token F_QUERY_SNOOP
%token F_QUERY_VERB
%token F_RANDOM
%token F_READ_BYTES
%token F_READ_FILE
%token F_REGEXP
%token F_REMOVE_CALL_OUT
%token F_RESTORE_OBJECT
%token F_RM
%token F_RMDIR
%token F_SAVE_OBJECT
%token F_SAY
%token F_SET_BIT
%token F_SET_HEART_BEAT
%token F_SET_LIGHT
%token F_SET_LIVING_NAME
%token F_SHADOW
%token F_SHOUT
%token F_SHUTDOWN
%token F_SIZEOF
%token F_SNOOP
%token F_SORT_ARRAY
%token F_STRINGP
%token F_STRLEN
%token F_SWAP
%token F_TAIL
%token F_TELL_OBJECT
%token F_TELL_ROOM
%token F_TEST_BIT
%token F_THIS_OBJECT
%token F_THIS_PLAYER
%token F_THROW
%token F_TIME
%token F_TRACE
%token F_TRACEPREFIX
%token F_TRANSFER
%token F_UNIQUE_ARRAY
%token F_USERS
%token F_VERSION
%token F_WIZLIST
%token F_WRITE
%token F_WRITE_BYTES
%token F_WRITE_FILE
/*
 * These are token values that needn't have an associated code for the
 * compiled file
 */

%token F_CASE F_DEFAULT F_RANGE

%union
{
	int number;
	unsigned int address;	/* Address of an instruction */
	char *string;
	short type;
	struct { int key; char block; } case_label;
	struct function *funp;
}

%type <number> assign F_NUMBER constant F_LOCAL_NAME expr_list
%type <number> const1 const2 const3 const4 const5 const6 const7 const8 const9
%type <number> lvalue_list argument type basic_type optional_star expr_list2
%type <number> type_modifier type_modifier_list opt_basic_type block_or_semi
%type <number> argument_list
%type <string> F_IDENTIFIER F_STRING string_con1 string_constant function_name

%type <case_label> case_label

/* The following symbos return type information */

%type <type> function_call lvalue string cast expr28 expr01 comma_expr
%type <type> expr2 expr211 expr1 expr212 expr213 expr24 expr22 expr23 expr25
%type <type> expr27 expr3 expr31 expr4 number expr0
%%

all: program;

program: program def possible_semi_colon
       |	 /* empty */ ;

possible_semi_colon: /* empty */
                   | ';' { yyerror("Extra ';'. Ignored."); };

inheritance: type_modifier_list F_INHERIT F_STRING ';'
		{
		    struct object *ob;
		    struct inherit inherit;
		    int initializer;

		    ob = find_object2($3);
		    if (ob == 0) {
			inherit_file = $3;
			/* Return back to load_object() */
			YYACCEPT;
		    }
		    free($3);
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
		    copy_variables(ob->prog, $1);
		    initializer = copy_functions(ob->prog, $1);
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

number: F_NUMBER
	{
	    if ( $1 == 0 ) {
		ins_f_byte(F_CONST0); $$ = TYPE_ANY;
	    } else if ( $1 == 1 ) {
		ins_f_byte(F_CONST1); $$ = TYPE_NUMBER;
	    } else {
		ins_f_byte(F_NUMBER); ins_long($1); $$ = TYPE_NUMBER;
	    }
	} ;

optional_star: /* empty */ { $$ = 0; } | '*' { $$ = TYPE_MOD_POINTER; } ;

block_or_semi: block { $$ = 0; } | ';' { $$ = ';'; } ;

def: type optional_star F_IDENTIFIER
	{
	    /* Save start of function. */
	    push_explicit(mem_block[A_PROGRAM].current_size);

	    if ($1 & TYPE_MOD_MASK) {
		exact_types = $1 | $2;
	    } else {
		if (pragma_strict_types)
		    yyerror("\"#pragma strict_types\" requires type of function");
		exact_types = 0;
	    }
	}
	'(' argument ')'
	{
	    /*
	     * Define a prototype. If it is a real function, then the
	     * prototype will be replaced below.
	     */
	    define_new_function($3, $6, 0, 0,
				NAME_UNDEFINED|NAME_PROTOTYPE, $1 | $2);
	}
        block_or_semi
	{
	    /* Either a prototype or a block */
	    if ($9 == ';') {
		(void)pop_address(); /* Not used here */
	    } else {
		define_new_function($3, $6, current_number_of_locals - $6+
			( max_break_stack_need -1 ) / sizeof(struct svalue) +1,
			pop_address(), 0, $1 | $2);
		ins_f_byte(F_CONST0); ins_f_byte(F_RETURN);
	    }
	    free_all_local_names();
	    free($3);		/* Value was copied above */
	}
   | type name_list ';' { if ($1 == 0) yyerror("Missing type"); }
   | inheritance ;

new_arg_name: type optional_star F_IDENTIFIER
	{
	    if (exact_types && $1 == 0) {
		yyerror("Missing type for argument");
		add_local_name($3, TYPE_ANY);	/* Supress more errors */
	    } else {
		add_local_name($3, $1 | $2);
	    }
	}
	  | type F_LOCAL_NAME
		{yyerror("Illegal to redeclare local name"); } ;

argument: /* empty */ { $$ = 0; }
	  | argument_list ;

argument_list: new_arg_name { $$ = 1; }
	     | argument_list ',' new_arg_name { $$ = $1 + 1; } ;

type_modifier: F_NO_MASK { $$ = TYPE_MOD_NO_MASK; }
	     | F_STATIC { $$ = TYPE_MOD_STATIC; }
	     | F_PRIVATE { $$ = TYPE_MOD_PRIVATE; }
	     | F_PUBLIC { $$ = TYPE_MOD_PUBLIC; }
	     | F_VARARGS { $$ = TYPE_MOD_VARARGS; }
	     | F_PROTECTED { $$ = TYPE_MOD_PROTECTED; } ;

type_modifier_list: /* empty */ { $$ = 0; }
		  | type_modifier type_modifier_list { $$ = $1 | $2; } ;

type: type_modifier_list opt_basic_type { $$ = $1 | $2; current_type = $$; } ;

cast: '(' basic_type optional_star ')'
	{
	    $$ = $2 | $3;
	} ;

opt_basic_type: basic_type | /* empty */ { $$ = TYPE_UNKNOWN; } ;

basic_type: F_STATUS { $$ = TYPE_NUMBER; current_type = $$; }
	| F_INT { $$ = TYPE_NUMBER; current_type = $$; }
	| F_STRING_DECL { $$ = TYPE_STRING; current_type = $$; }
	| F_OBJECT { $$ = TYPE_OBJECT; current_type = $$; }
	| F_VOID {$$ = TYPE_VOID; current_type = $$; }
	| F_MIXED { $$ = TYPE_ANY; current_type = $$; } ;

name_list: new_name
	 | new_name ',' name_list;

new_name: optional_star F_IDENTIFIER
	{
	    define_variable($2, current_type | $1, 0);
	    free($2);
	}
| optional_star F_IDENTIFIER
	{
	    int var_num;
	    define_variable($2, current_type | $1, 0);
	    var_num = verify_declared($2);
	    transfer_init_control();
	    ins_f_byte(F_PUSH_IDENTIFIER_LVALUE);
	    ins_byte(var_num);
	}
	'=' expr0
	{
	    if (!compatible_types((current_type | $1) & TYPE_MOD_MASK, $5)){
		char buff[100];
		sprintf(buff, "Type mismatch %s when initializing %s",
			get_two_types(current_type | $1, $5), $2);
		yyerror(buff);
	    }
	    ins_f_byte(F_ASSIGN);
	    ins_f_byte(F_POP_VALUE);
	    add_new_init_jump();
	    free($2);
	} ;
block: '{' local_declarations statements '}'
	{ ; };

local_declarations: /* empty */
		  | local_declarations basic_type local_name_list ';' ;

new_local_name: optional_star F_IDENTIFIER
	{
	    add_local_name($2, current_type | $1);
	} ;

local_name_list: new_local_name
	| new_local_name ',' local_name_list ;

statements: /* empty */
	  | statement statements
	  | error ';' ;

statement: comma_expr ';'
	{
	    ins_f_byte(F_POP_VALUE);
	    if (d_flag)
		ins_f_byte(F_BREAK_POINT);
	    /* if (exact_types && !TYPE($1,TYPE_VOID))
		yyerror("Value thrown away"); */
	}
	 | cond | while | do | for | switch | case | default | return ';'
	 | block
  	 | /* empty */ ';'
	 | F_BREAK ';'	/* This code is a jump to a jump */
		{
		    if (current_break_address == 0)
			yyerror("break statement outside loop");
		    if (current_break_address & BREAK_ON_STACK) {
			ins_f_byte(F_BREAK);
		    } else {
		        ins_f_byte(F_JUMP); ins_short(current_break_address);
		    }
		}
	 | F_CONTINUE ';'	/* This code is a jump to a jump */
		{
		    if (current_continue_address == 0)
			yyerror("continue statement outside loop");
		    ins_f_byte(F_JUMP); ins_short(current_continue_address);
		}
         ;

while:  {   push_explicit(current_continue_address);
	    push_explicit(current_break_address);
	    current_continue_address = mem_block[A_PROGRAM].current_size;
	} F_WHILE '(' comma_expr ')'
	{
	    ins_f_byte(F_JUMP_WHEN_NON_ZERO); ins_short(0);	/* to block */
	    current_break_address = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_JUMP); ins_short(0);	/* Exit loop */
	    upd_short(current_break_address-2,
		      mem_block[A_PROGRAM].current_size);
	}
       statement
	{
	  ins_f_byte(F_JUMP); ins_short(current_continue_address);
	  upd_short(current_break_address+1,
		    mem_block[A_PROGRAM].current_size);
	  current_break_address = pop_address();
	  current_continue_address = pop_address();
        }

do: {
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
	
    } F_DO statement F_WHILE '(' comma_expr ')' ';'
    {
	ins_f_byte(F_JUMP_WHEN_NON_ZERO); ins_short(pop_address());
	/* Fill in the break jump address in the beginning of the loop. */
	upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	current_break_address = pop_address();
	current_continue_address = pop_address();
    }

for: F_FOR '('	  { push_explicit(current_continue_address);
		    push_explicit(current_break_address); }
     for_expr ';' {   ins_f_byte(F_POP_VALUE);
		      push_address();
		  }
     for_expr ';' {
		    ins_f_byte(F_JUMP_WHEN_NON_ZERO);
		    ins_short(0);	/* Jump to block of block */
		    current_break_address = mem_block[A_PROGRAM].current_size;
		    ins_f_byte(F_JUMP); ins_short(0);	/* Out of loop */
	 	    current_continue_address =
			mem_block[A_PROGRAM].current_size;
		  }
     for_expr ')' {
	 	    ins_f_byte(F_POP_VALUE);
		    ins_f_byte(F_JUMP); ins_short(pop_address());
		    /* Here starts the block. */
		    upd_short(current_break_address-2,
			      mem_block[A_PROGRAM].current_size);
		  }
     statement
   {
       ins_f_byte(F_JUMP); ins_short(current_continue_address);
       /* Now, the address of the end of the block is known. */
       upd_short(current_break_address+1, mem_block[A_PROGRAM].current_size);
       current_break_address = pop_address();
       current_continue_address = pop_address();
   }

for_expr: /* EMPTY */ { ins_f_byte(F_CONST1); }
        | comma_expr;

switch: F_SWITCH '(' comma_expr ')'
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
      statement
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
    } ;

case: F_CASE case_label ':'
    {
	struct case_heap_entry temp;

	if ( !( current_break_address & BREAK_FROM_CASE ) ) {
	    yyerror("Case outside switch");
	    break;
	}
	temp.key = $2.key;
	temp.addr = mem_block[A_PROGRAM].current_size;
	temp.line = current_line;
	add_to_case_heap($2.block,&temp);
    }
    | F_CASE case_label F_RANGE case_label ':'
    {
	struct case_heap_entry temp;

	if ( $2.block != A_CASE_NUMBERS || $4.block != A_CASE_NUMBERS )
	    yyerror("String case labels not allowed as range bounds");
	if ($2.key > $4.key) break;
	temp.key = $2.key;
	temp.addr = 1;
	temp.line = current_line;
	add_to_case_heap(A_CASE_NUMBERS,&temp);
	temp.key = $4.key;
	temp.addr = mem_block[A_PROGRAM].current_size;
	temp.line = 0;
	add_to_case_heap(A_CASE_NUMBERS,&temp);
    } ;
	
case_label: constant
        {
	    if ( !(zero_case_label & NO_STRING_CASE_LABELS) )
		yyerror("Mixed case label list not allowed");
	    if ( ( $$.key = $1) )
	        zero_case_label |= SOME_NUMERIC_CASE_LABELS;
	    else
		zero_case_label |= mem_block[A_PROGRAM].current_size;
	    $$.block = A_CASE_NUMBERS;
	}
	  | string_constant
        {
	    if ( zero_case_label & SOME_NUMERIC_CASE_LABELS )
		yyerror("Mixed case label list not allowed");
	    zero_case_label &= ~NO_STRING_CASE_LABELS;
            store_prog_string($1);
            $$.key = (int)$1;
	    $$.block = A_CASE_STRINGS;
        }
	  ;

constant: const1
	| constant '|' const1 { $$ = $1 | $3; } ;

const1: const2
      | const1 '^' const2 { $$ = $1 ^ $3; } ;

const2: const3
      | const2 '&' const3 { $$ = $1 & $3; } ;

const3: const4
      | const3 F_EQ const4 { $$ = $1 == $3; }
      | const3 F_NE const4 { $$ = $1 != $3; } ;

const4: const5
      | const4 '>'  const5 { $$ = $1 >  $3; }
      | const4 F_GE const5 { $$ = $1 >= $3; }
      | const4 '<'  const5 { $$ = $1 <  $3; }
      | const4 F_LE const5 { $$ = $1 <= $3; } ;

const5: const6
      | const5 F_LSH const6 { $$ = $1 << $3; }
      | const5 F_RSH const6 { $$ = $1 >> $3; } ;

const6: const7
      | const6 '+' const7 { $$ = $1 + $3; }
      | const6 '-' const7 { $$ = $1 - $3; } ;

const7: const8
      | const7 '*' const8 { $$ = $1 * $3; }
      | const7 '%' const8 { $$ = $1 % $3; }
      | const7 '/' const8 { $$ = $1 / $3; } ;

const8: const9
      | '(' constant ')' { $$ = $2; } ;

const9: F_NUMBER
      | '-'   F_NUMBER { $$ = -$2; }
      | F_NOT F_NUMBER { $$ = !$2; }
      | '~'   F_NUMBER { $$ = ~$2; } ;

default: F_DEFAULT ':'
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
    } ;


comma_expr: expr0 { $$ = $1; }
          | comma_expr { ins_f_byte(F_POP_VALUE); }
	',' expr0
	{ $$ = $4; } ;

expr0:  expr01
     | lvalue assign expr0
	{
	    if (exact_types && !compatible_types($1, $3) &&
		!($1 == TYPE_STRING && $3 == TYPE_NUMBER && $2 == F_ADD_EQ))
	    {
		type_error("Bad assignment. Rhs", $3);
	    }
	    ins_f_byte($2);
	    $$ = $3;
	}
     | error assign expr01 { yyerror("Illegal LHS");  $$ = TYPE_ANY; };

expr01: expr1 { $$ = $1; }
     | expr1 '?'
	{
	    ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	}
      expr01
	{
	    int i;
	    i = pop_address();
	    ins_f_byte(F_JUMP); push_address(); ins_short(0);
	    upd_short(i, mem_block[A_PROGRAM].current_size);
	}
      ':' expr01
	{
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if (exact_types && !compatible_types($4, $7)) {
		type_error("Different types in ?: expr", $4);
		type_error("                      and ", $7);
	    }
	    if ($4 == TYPE_ANY) $$ = $7;
	    else if ($7 == TYPE_ANY) $$ = $4;
	    else if (TYPE($4, TYPE_MOD_POINTER|TYPE_ANY)) $$ = $7;
	    else if (TYPE($7, TYPE_MOD_POINTER|TYPE_ANY)) $$ = $4;
	    else $$ = $4;
	};

assign: '=' { $$ = F_ASSIGN; }
      | F_AND_EQ { $$ = F_AND_EQ; }
      | F_OR_EQ { $$ = F_OR_EQ; }
      | F_XOR_EQ { $$ = F_XOR_EQ; }
      | F_LSH_EQ { $$ = F_LSH_EQ; }
      | F_RSH_EQ { $$ = F_RSH_EQ; }
      | F_ADD_EQ { $$ = F_ADD_EQ; }
      | F_SUB_EQ { $$ = F_SUB_EQ; }
      | F_MULT_EQ { $$ = F_MULT_EQ; }
      | F_MOD_EQ { $$ = F_MOD_EQ; }
      | F_DIV_EQ { $$ = F_DIV_EQ; };

return: F_RETURN
	{
	    if (exact_types && !TYPE(exact_types, TYPE_VOID))
		type_error("Must return a value for a function declared",
			   exact_types);
	    ins_f_byte(F_CONST0);
	    ins_f_byte(F_RETURN);
	}
      | F_RETURN comma_expr
	{
	    if (exact_types && !TYPE($2, exact_types & TYPE_MOD_MASK))
		type_error("Return type not matching", exact_types);
	    ins_f_byte(F_RETURN);
	};

expr_list: /* empty */		{ $$ = 0; }
	 | expr_list2		{ $$ = $1; }
	 | expr_list2 ','	{ $$ = $1; } ; /* Allow a terminating comma */

expr_list2: expr0		{ $$ = 1; add_arg_type($1); }
         | expr_list2 ',' expr0	{ $$ = $1 + 1; add_arg_type($3); } ;

expr1: expr2 { $$ = $1; }
     | expr2 F_LOR
	{
	    ins_f_byte(F_DUP); ins_f_byte(F_JUMP_WHEN_NON_ZERO);
	    push_address();
	    ins_short(0);
	    ins_f_byte(F_POP_VALUE);
	}
       expr1
	{
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if ($1 == $4)
		$$ = $1;
	    else
		$$ = TYPE_ANY;	/* Return type can't be known */
	};

expr2: expr211 { $$ = $1; }
     | expr211 F_LAND
	{
	    ins_f_byte(F_DUP); ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	    ins_f_byte(F_POP_VALUE);
	}
       expr2
	{
	    upd_short(pop_address(), mem_block[A_PROGRAM].current_size);
	    if ($1 == $4)
		$$ = $1;
	    else
		$$ = TYPE_ANY;	/* Return type can't be known */
	} ;

expr211: expr212
       | expr211 '|' expr212
          {
	      if (exact_types && !TYPE($1,TYPE_NUMBER))
		  type_error("Bad argument 1 to |", $1);
	      if (exact_types && !TYPE($3,TYPE_NUMBER))
		  type_error("Bad argument 2 to |", $3);
	      $$ = TYPE_NUMBER;
	      ins_f_byte(F_OR);
	  };

expr212: expr213
       | expr212 '^' expr213
	  {
	      if (exact_types && !TYPE($1,TYPE_NUMBER))
		  type_error("Bad argument 1 to ^", $1);
	      if (exact_types && !TYPE($3,TYPE_NUMBER))
		  type_error("Bad argument 2 to ^", $3);
	      $$ = TYPE_NUMBER;
	      ins_f_byte(F_XOR);
	  };

expr213: expr22
       | expr213 '&' expr22
	  {
	      ins_f_byte(F_AND);
	      if ( !TYPE($1,TYPE_MOD_POINTER) || !TYPE($3,TYPE_MOD_POINTER) ) {
	          if (exact_types && !TYPE($1,TYPE_NUMBER))
		      type_error("Bad argument 1 to &", $1);
	          if (exact_types && !TYPE($3,TYPE_NUMBER))
		      type_error("Bad argument 2 to &", $3);
	      }
	      $$ = TYPE_NUMBER;
	  };

expr22: expr23
      | expr24 F_EQ expr24
	{
	    int t1 = $1 & TYPE_MOD_MASK, t2 = $3 & TYPE_MOD_MASK;
	    if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
		type_error("== always false because of different types", $1);
		type_error("                               compared to", $3);
	    }
	    ins_f_byte(F_EQ);
	    $$ = TYPE_NUMBER;
	};
      | expr24 F_NE expr24
	{
	    int t1 = $1 & TYPE_MOD_MASK, t2 = $3 & TYPE_MOD_MASK;
	    if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
		type_error("!= always true because of different types", $1);
		type_error("                               compared to", $3);
	    }
	    ins_f_byte(F_NE);
	    $$ = TYPE_NUMBER;
	};

expr23: expr24
      | expr24 '>' expr24
	{ $$ = TYPE_NUMBER; ins_f_byte(F_GT); };
      | expr24 F_GE expr24
	{ $$ = TYPE_NUMBER; ins_f_byte(F_GE); };
      | expr24 '<' expr24
	{ $$ = TYPE_NUMBER; ins_f_byte(F_LT); };
      | expr24 F_LE expr24
	{ $$ = TYPE_NUMBER; ins_f_byte(F_LE); };

expr24: expr25
      | expr24 F_LSH expr25
	{
	    ins_f_byte(F_LSH);
	    $$ = TYPE_NUMBER;
	    if (exact_types && !TYPE($1, TYPE_NUMBER))
		type_error("Bad argument number 1 to '<<'", $1);
	    if (exact_types && !TYPE($3, TYPE_NUMBER))
		type_error("Bad argument number 2 to '<<'", $3);
	};
      | expr24 F_RSH expr25
	{
	    ins_f_byte(F_RSH);
	    $$ = TYPE_NUMBER;
	    if (exact_types && !TYPE($1, TYPE_NUMBER))
		type_error("Bad argument number 1 to '>>'", $1);
	    if (exact_types && !TYPE($3, TYPE_NUMBER))
		type_error("Bad argument number 2 to '>>'", $3);
	};

expr25: expr27
      | expr25 '+' expr27	/* Type checks of this case is complicated */
	{ ins_f_byte(F_ADD); $$ = TYPE_ANY; };
      | expr25 '-' expr27
	{
	    int bad_arg = 0;

	    if (exact_types) {
		if (!TYPE($1, TYPE_NUMBER) && !($1 & TYPE_MOD_POINTER) ) {
                    type_error("Bad argument number 1 to '-'", $1);
		    bad_arg++;
		}
		if (!TYPE($3, TYPE_NUMBER) && !($3 & TYPE_MOD_POINTER) ) {
                    type_error("Bad argument number 2 to '-'", $3);
		    bad_arg++;
		}
	    }
	    $$ = TYPE_ANY;
	    if (($1 & TYPE_MOD_POINTER) || ($3 & TYPE_MOD_POINTER))
		$$ = TYPE_MOD_POINTER | TYPE_ANY;
	    if (!($1 & TYPE_MOD_POINTER) || !($3 & TYPE_MOD_POINTER)) {
		if (exact_types && $$ != TYPE_ANY && !bad_arg)
		    yyerror("Arguments to '-' don't match");
		$$ = TYPE_NUMBER;
	    }
	    ins_f_byte(F_SUBTRACT);
	};

expr27: expr28
      | expr27 '*' expr3
	{
	    if (exact_types && !TYPE($1, TYPE_NUMBER))
		type_error("Bad argument number 1 to '*'", $1);
	    if (exact_types && !TYPE($3, TYPE_NUMBER))
		type_error("Bad argument number 2 to '*'", $3);
	    ins_f_byte(F_MULTIPLY);
	    $$ = TYPE_NUMBER;
	};
      | expr27 '%' expr3
	{
	    if (exact_types && !TYPE($1, TYPE_NUMBER))
		type_error("Bad argument number 1 to '%'", $1);
	    if (exact_types && !TYPE($3, TYPE_NUMBER))
		type_error("Bad argument number 2 to '%'", $3);
	    ins_f_byte(F_MOD);
	    $$ = TYPE_NUMBER;
	};
      | expr27 '/' expr3
	{
	    if (exact_types && !TYPE($1, TYPE_NUMBER))
		type_error("Bad argument number 1 to '/'", $1);
	    if (exact_types && !TYPE($3, TYPE_NUMBER))
		type_error("Bad argument number 2 to '/'", $3);
	    ins_f_byte(F_DIVIDE);
	    $$ = TYPE_NUMBER;
	};

expr28: expr3
	| cast expr3
	      {
		  $$ = $1;
		  if (exact_types && $2 != TYPE_ANY && $2 != TYPE_UNKNOWN &&
		      $1 != TYPE_VOID)
		      type_error("Casts are only legal for type mixed, or when unknown", $2);
	      } ;

expr3: expr31
     | F_INC lvalue
        {
	    ins_f_byte(F_INC);
	    if (exact_types && !TYPE($2, TYPE_NUMBER))
		type_error("Bad argument to ++", $2);
	    $$ = TYPE_NUMBER;
	};
     | F_DEC lvalue
        {
	    ins_f_byte(F_DEC);
	    if (exact_types && !TYPE($2, TYPE_NUMBER))
		type_error("Bad argument to --", $2);
	    $$ = TYPE_NUMBER;
	};
     | F_NOT expr3
	{
	    ins_f_byte(F_NOT);	/* Any type is valid here. */
	    $$ = TYPE_NUMBER;
	};
     | '~' expr3
	{
	    ins_f_byte(F_COMPL);
	    if (exact_types && !TYPE($2, TYPE_NUMBER))
		type_error("Bad argument to ~", $2);
	    $$ = TYPE_NUMBER;
	};
     | '-' expr3
	{
	    ins_f_byte(F_NEGATE);
	    if (exact_types && !TYPE($2, TYPE_NUMBER))
		type_error("Bad argument to unary '-'", $2);
	    $$ = TYPE_NUMBER;
	};

expr31: expr4
      | lvalue F_INC
         {
	     ins_f_byte(F_POST_INC);
	     if (exact_types && !TYPE($1, TYPE_NUMBER))
		 type_error("Bad argument to ++", $1);
	     $$ = TYPE_NUMBER;
	 };
      | lvalue F_DEC
         {
	     ins_f_byte(F_POST_DEC);
	     if (exact_types && !TYPE($1, TYPE_NUMBER))
		 type_error("Bad argument to --", $1);
	     $$ = TYPE_NUMBER;
	 };

expr4: function_call
     | lvalue
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
	    $$ = $1;
	}
     | string | number
     | '(' comma_expr ')' { $$ = $2; }
     | catch { $$ = TYPE_ANY; }
     | sscanf { $$ = TYPE_NUMBER; }
     | parse_command { $$ = TYPE_NUMBER; }
     | '(' '{' expr_list '}' ')'
       {
	   pop_arg_stack($3);		/* We don't care about these types */
	   ins_f_byte(F_AGGREGATE);
	   ins_short($3);
	   $$ = TYPE_MOD_POINTER | TYPE_ANY;
       };

catch: F_CATCH { ins_f_byte(F_CATCH); push_address(); ins_short(0);}
       '(' comma_expr ')'
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
	       };

sscanf: F_SSCANF '(' expr0 ',' expr0 lvalue_list ')'
	{
	    ins_f_byte(F_SSCANF); ins_byte($6 + 2);
	}

parse_command: F_PARSE_COMMAND '(' expr0 ',' expr0 ',' expr0 lvalue_list ')'
	{
	    ins_f_byte(F_PARSE_COMMAND); ins_byte($8 + 3);
	}

lvalue_list: /* empty */ { $$ = 0; }
	   | ',' lvalue lvalue_list { $$ = 1 + $3; } ;

lvalue: F_IDENTIFIER
	{
	    int i = verify_declared($1);
	    last_push_identifier = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_PUSH_IDENTIFIER_LVALUE);
	    ins_byte(i);
	    free($1);
	    if (i == -1)
		$$ = TYPE_ANY;
	    else
		$$ = VARIABLE(i)->type & TYPE_MOD_MASK;
	}
        | F_LOCAL_NAME
	{
	    last_push_local = mem_block[A_PROGRAM].current_size;
	    ins_f_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
	    ins_byte($1);
	    $$ = type_of_locals[$1];
	}
	| expr4 '[' comma_expr F_RANGE comma_expr ']'
	  {
	      ins_f_byte(F_RANGE);
	      last_push_indexed = 0;
	      if (exact_types) {
		  if (($1 & TYPE_MOD_POINTER) == 0 && !TYPE($1, TYPE_STRING))
		      type_error("Bad type to indexed value", $1);
		  if (!TYPE($3, TYPE_NUMBER))
		      type_error("Bad type of index", $3);
		  if (!TYPE($5, TYPE_NUMBER))
		      type_error("Bad type of index", $5);
	      }
	      if ($1 == TYPE_ANY)
		  $$ = TYPE_ANY;
	      else if (TYPE($1, TYPE_STRING))
		  $$ = TYPE_STRING;
	      else if ($1 & TYPE_MOD_POINTER)
		  $$ = $1;
	      else if (exact_types)
		  type_error("Bad type of argument used for range", $1);
	  };
	| expr4 '[' comma_expr ']'
	  {
	      last_push_indexed = mem_block[A_PROGRAM].current_size;
	      ins_f_byte(F_PUSH_INDEXED_LVALUE);
	      if (exact_types) {
		  if (($1 & TYPE_MOD_POINTER) == 0 && !TYPE($1, TYPE_STRING))
		      type_error("Bad type to indexed value", $1);
		  if (!TYPE($3, TYPE_NUMBER))
		      type_error("Bad type of index", $3);
	      }
	      if ($1 == TYPE_ANY)
		  $$ = TYPE_ANY;
	      else if (TYPE($1, TYPE_STRING))
		  $$ = TYPE_NUMBER;
	      else
		  $$ = $1 & TYPE_MOD_MASK & ~TYPE_MOD_POINTER;
	  };

string: F_STRING
	{
	    ins_f_byte(F_STRING);
	    ins_short(store_prog_string($1));
	    free($1);
	    $$ = TYPE_STRING;
	};

string_constant: string_con1
        {
            char *p = make_shared_string($1);
            free($1);
            $$ = p;
        };

string_con1: F_STRING
	   | string_con1 '+' F_STRING
	{
	    $$ = xalloc( strlen($1) + strlen($3) + 1 );
	    strcpy($$, $1);
	    strcat($$, $3);
	    free($1);
	    free($3);
	};

function_call: function_name
    {
	/* This seems to be an ordinary function call. But, if the function
	 * is not defined, then it might be a call to a simul_efun.
	 * If it is, then we make it a call_other(), which requires the
	 * function name as argument.
	 * We have to remember until after parsing the arguments if it was
	 * a simulated efun or not, which means that the pointer has to be
	 * pushed on a stack. Use the internal yacc stack for this purpose.
	 */
	$<funp>$ = 0;
	if (defined_function($1) == -1) {
	    char *p = make_shared_string($1);
	    $<funp>$ = find_simul_efun(p);
	    if ($<funp>$ && !($<funp>$->type & TYPE_MOD_STATIC)) {
		ins_f_byte(F_STRING);
		ins_short(store_prog_string(
	              query_simul_efun_file_name()));
		ins_f_byte(F_STRING);
		ins_short(store_prog_string(p));
	    } else {
		$<funp>$ = 0;
	    }
	    free_string(p);
	}
    }
	'(' expr_list ')'
    { 
	int f;
	int efun_override = strncmp($1, "efun::", 6) == 0;

	if ($<funp>2) {
	    ins_f_byte(F_CALL_OTHER);
	    ins_byte($4 + 2);
	    $$ = $<funp>2->type;
	} else if (!efun_override && (f = defined_function($1)) >= 0) {
	    struct function *funp;
	    ins_f_byte(F_CALL_FUNCTION_BY_ADDRESS); ins_short(f);
	    ins_byte($4);	/* Actual number of arguments */
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
	    $$ = funp->type & TYPE_MOD_MASK;
	    /*
	     * Check number of arguments.
	     */
	    if (funp->num_arg != $4 && !(funp->type & TYPE_MOD_VARARGS) &&
		(funp->flags & NAME_STRICT_TYPES) && exact_types)
	    {
		char buff[100];
		sprintf(buff, "Wrong number of arguments to %.60s", $1);
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
		for (i=0; i < funp->num_arg && i < $4; i++) {
		    int tmp = get_argument_type(i, $4);
		    if (!TYPE(tmp, arg_types[first + i])) {
			char buff[100];
			sprintf(buff, "Bad type for argument %d %s", i+1,
				get_two_types(arg_types[first+i], tmp));
			yyerror(buff);
		    }
		}
	    }
	} else if (efun_override || (f = lookup_predef($1)) != -1) {
	    int min, max, def, *argp;
	    extern int efun_arg_types[];

	    if (efun_override) {
		f = lookup_predef($1+6);
	    }
	    if (f == -1) {	/* Only possible for efun_override */
		char buff[100];
		sprintf(buff, "Unknown efun: %s", $1+6);
		yyerror(buff);
	    } else {
		min = instrs[f-F_OFFSET].min_arg;
		max = instrs[f-F_OFFSET].max_arg;
		def = instrs[f-F_OFFSET].Default;
		$$ = instrs[f-F_OFFSET].ret_type;
		argp = &efun_arg_types[instrs[f-F_OFFSET].arg_index];
		if (def && $4 == min-1) {
		    ins_f_byte(def);
		    max--;
		    min--;
		} else if ($4 < min) {
		    char bff[100];
		    sprintf(bff, "Too few arguments to %s",
			    instrs[f-F_OFFSET].name);
		    yyerror(bff);
		} else if ($4 > max && max != -1) {
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
		    for (argn=0; argn < $4; argn++) {
			int tmp = get_argument_type(argn, $4);
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
		    ins_byte($4);/* Number of actual arguments */
	    }
	} else {
	    struct function *funp;

	    f = define_new_function($1, 0, 0, 0, NAME_UNDEFINED, 0);
	    ins_f_byte(F_CALL_FUNCTION_BY_ADDRESS);
	    ins_short(f);
	    ins_byte($4);	/* Number of actual arguments */
	    funp = FUNCTION(f);
	    if (strchr($1, ':')) {
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
		sprintf(buff, "Undefined function %.50s", $1);
		yyerror(buff);
	    }
	    if (!(funp->flags & NAME_UNDEFINED))
		$$ = funp->type;
	    else
		$$ = TYPE_ANY;	/* Just a guess */
	}
	free($1);
	pop_arg_stack($4);	/* Argument types not needed more */
    }
| expr4 F_ARROW function_name
    {
	ins_f_byte(F_STRING);
	ins_short(store_prog_string($3));
	free($3);
    }
'(' expr_list ')'
    {
	ins_f_byte(F_CALL_OTHER);
	ins_byte($6 + 2);
	$$ = TYPE_UNKNOWN;
	pop_arg_stack($6);	/* No good need of these arguments */
    };

function_name: F_IDENTIFIER
	     | F_COLON_COLON F_IDENTIFIER
		{
		    char *p = xalloc(strlen($2) + 3);
		    strcpy(p, "::"); strcat(p, $2); free($2);
		    $$ = p;
		}
	      | F_IDENTIFIER F_COLON_COLON F_IDENTIFIER
		{
		    char *p = xalloc(strlen($1) + strlen($3) + 3);
		    strcpy(p, $1); strcat(p, "::"); strcat(p, $3);
		    free($1); free($3);
		    $$ = p;
		};

cond: condStart
      statement
	{
	    int i;
	    i = pop_address();
	    ins_f_byte(F_JUMP); push_address(); ins_short(0);
	    upd_short(i, mem_block[A_PROGRAM].current_size);
	}
      optional_else_part
	{ upd_short(pop_address(), mem_block[A_PROGRAM].current_size); } ;

condStart: F_IF '(' comma_expr ')'
	{
	    ins_f_byte(F_JUMP_WHEN_ZERO);
	    push_address();
	    ins_short(0);
	} ;

optional_else_part: /* empty */
       | F_ELSE statement ;
%%

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
