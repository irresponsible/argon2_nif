# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

ARGON2_SRC_DIR = $(BASEDIR)/vendor/argon2/src
ARGON2_INC_DIR = $(BASEDIR)/vendor/argon2/include

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
CFLAGS += -std=c89 -pthread -O3 -Wall -g -I$(ARGON2_INC_DIR) -I$(ARGON2_SRC_DIR) -I. -I$(ERTS_INCLUDE_DIR)

OPTTARGET ?= native
OPTTEST := $(shell $(CC) -I$(ARGON2_INC_DIR) -I$(ARGON2_SRC_DIR) -march=$(OPTTARGET) $(SRC_DIR)/opt.c -c\
	-o /dev/null 2>/dev/null; echo $$?)

SOURCES := \
	$(ARGON2_SRC_DIR)/argon2.c \
	$(ARGON2_SRC_DIR)/core.c \
	$(ARGON2_SRC_DIR)/blake2/blake2b.c \
	$(ARGON2_SRC_DIR)/thread.c \
	$(ARGON2_SRC_DIR)/encoding.c \
	$(CURDIR)/argon2_nif.c

ifneq ($(OPTTEST), 0)
	SOURCES += $(ARGON2_SRC_DIR)/ref.c
else
	CFLAGS += -march=$(OPTTARGET)
	SOURCES += $(ARGON2_SRC_DIR)/opt.c
endif

OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

KERNEL_NAME := $(shell uname -s)

LIB_NAME = $(BASEDIR)/priv/argon2_nif.so

ifneq ($(CROSSCOMPILE),)
	LIB_CFLAGS := -shared -fPIC -fvisibility=hidden -DA2_VISCTL=1
	SO_LDFLAGS := -Wl,-soname,libargon2.so.0
else
	ifeq ($(KERNEL_NAME), Linux)
		LIB_CFLAGS := -shared -fPIC -fvisibility=hidden -DA2_VISCTL=1
		SO_LDFLAGS := -Wl,-soname,libargon2.so.0
	endif
	ifeq ($(KERNEL_NAME), Darwin)
		LIB_CFLAGS := -dynamiclib -undefined dynamic_lookup
	endif
	ifeq ($(KERNEL_NAME), $(filter $(KERNEL_NAME),OpenBSD FreeBSD NetBSD))
		LIB_CFLAGS := -shared -fPIC
	endif
endif

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c

$(LIB_NAME): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(LIB_NAME)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(LIB_NAME) $(OBJECTS)
