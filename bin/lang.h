/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

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

#line 470 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
