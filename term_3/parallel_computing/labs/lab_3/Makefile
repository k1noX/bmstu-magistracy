SRC ?= src/task1/example1.cpp

NAME = $(basename $(notdir $(SRC)))
TARGET = bin/$(NAME)

CXX = g++
CXXFLAGS = -fopenmp -Wall

.PHONY: all run clean

$(shell mkdir -p bin)

all: build run

build:
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRC)

run: $(TARGET)
	OMP_NUM_THREADS=2 ./$(TARGET)

clean:
	rm -f $(TARGET)
