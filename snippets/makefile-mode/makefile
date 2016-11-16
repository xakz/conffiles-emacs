#name : Simple Program Makefile
# expand-env: ((yas/indent-line 'fixed))
# --
NAME	= ${1:fatsoft}

SUBLIBS	= ${2:libmy/libmy.a}

CC	= gcc
RM	= rm -f

CFLAGS	+= -g -O0
CFLAGS	+= -Wextra -Wall -Werror=implicit
CFLAGS	+= -Wno-unused-parameter
CFLAGS	+= -ansi -pedantic

CFLAGS	+= $(patsubst %,-I%,$(dir $(SUBLIBS)))
LDFLAGS	+= 
LDLIBS	+= $(SUBLIBS) $0

SRCS	= $(wildcard *.c)
HDRS	= $(wildcard *.h)
OBJS	= $(SRCS:.c=.o)

all:	$(NAME)

$(NAME):	$(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LDLIBS) -o $(NAME)
$(OBJS):	$(SUBLIBS) $(HDRS)

clean:
	$(RM) $(OBJS)
distclean:	clean
	for i in $(dir $(SUBLIBS)); do $(MAKE) -C $$i $@; done
	$(RM) $(NAME)
fclean:	distclean
re:	distclean all


$(SUBLIBS):
	$(MAKE) -C $(dir $@) $(notdir $@)

.PHONY:	all clean distclean fclean re

