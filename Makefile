CXX = clang++

COMMONFLAGS := -I aoclib++ -std=c++23 -pipe -march=native
DEBUGFLAGS  := -O0 -g
RELFLAGS    := -O2

CXX_DIRS := 2025 mits2025

CXX_SOURCES := $(wildcard $(addsuffix /*.cpp,$(CXX_DIRS)))
CXX_OBJECTS := $(patsubst %.cpp,build/%.o,$(CXX_SOURCES))
CXX_DEPENDS := $(patsubst %.cpp,build/%.d,$(CXX_SOURCES))
EXEC	    := $(patsubst %.cpp,%,$(CXX_SOURCES))

Z3_SRC	:= 2025/day10.cpp mits2025/day19.cpp
Z3_OBJ 	:= $(patsubst %.cpp,build/%.o,$(Z3_SRC))

WARNING := -Wall -Wextra -Wmost -Wno-missing-designated-field-initializers

-include $(CXX_DEPENDS)

build/%.o: %.cpp Makefile
	@mkdir -p $(@D)
	$(CXX) $(WARNING) $(COMMONFLAGS) -static $(DEBUGFLAGS) -MD -MP $< -o $@

$(Z3_OBJ): build/%.o: %.cpp Makefile
	@mkdir -p $(@D)
	$(CXX) $(WARNING) $(COMMONFLAGS) -lz3 $(DEBUGFLAGS) -MD -MP $< -o $@

$(EXEC): %: build/%.o
	$< $(part) < $(dir $@)txt/$(basename $(notdir $@)).txt

clean:
	$(RM) $(CXX_OBJECTS) $(CXX_DEPENDS)


