JBUILDER ?= jbuilder

all:
	@$(JBUILDER) build

clean:
	@$(JBUILDER) clean

.PHONY: all clean
