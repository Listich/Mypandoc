##
## EPITECH PROJECT, 2025
## mypandoc
## File description:
## Makefile
##

NAME        := mypandoc
BUILD_DIR   := $(shell stack path --local-install-root)

all:
	stack build
	cp $(BUILD_DIR)/bin/$(NAME) .

clean:
	stack clean
	rm -f $(NAME)

fclean: clean

re: fclean all

.PHONY: all clean fclean re
