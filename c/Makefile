DIR_INCLUDE=include
DIR_SRC=src
DIR_BIN=bin

CC=gcc
CFLAGS=-Wall

TARGET=trif
SRC=main sexp


%.o: $(DIR_SRC)/%.c $(DIR_INCLUDE)/%.h
	$(CC) -c -I$(DIR_INCLUDE) $(CFLAGS) $< -o $@

$(DIR_BIN)/$(TARGET): $(SRC:%=%.o)
	mkdir -p $(DIR_BIN)
	$(CC) $(CFLAGS) $^ -o $@

all: $(DIR_BIN)/$(TARGET)

