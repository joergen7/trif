CC=g++
TARGET=trif
CFLAGS=-Wall

SRC=main

all: $(TARGET)

# delete default rule
%.o: %.cc

.PRECIOUS: %.ii
%.ii: %.cc
	$(CC) -E $(CFLAGS) $< -o $@

.PRECIOUS: %.s
%.s: %.ii
	$(CC) -S $(CFLAGS) $< -o $@

%.o: %.s
	$(CC) -c $(CFLAGS) $< -o $@

$(TARGET): $(SRC:%=%.o)
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.ii
	rm -f *.s
	rm -f *.o
	rm -f $(TARGET)

