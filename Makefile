CXX = clang++

COMMONFLAGS := -std=c++23 -pipe -march=native
DEBUGFLAGS  := -O0 -g
RELFLAGS    := -O2

CXX_DIRS := 2025 mits2025

CXX_SOURCES := $(wildcard $(addsuffix /*.cpp,$(CXX_DIRS)))
CXX_OBJECTS := $(patsubst %.cpp,build/%.o,$(CXX_SOURCES))
CXX_DEPENDS := $(patsubst %.cpp,build/%.d,$(CXX_SOURCES))
PART1	    := $(patsubst %.cpp,%.a,$(CXX_SOURCES))
PART2       := $(patsubst %.cpp,%.b,$(CXX_SOURCES))

WARNING := -Wall -Wextra -Wmost -Wno-missing-designated-field-initializers

-include $(CXX_DEPENDS)

build/%.o: %.cpp Makefile
	@mkdir -p $(@D)
	$(CXX) $(WARNING) $(COMMONFLAGS) -static $(DEBUGFLAGS) -MD -MP $< -o $@

build/2025/day10.o: 2025/day10.cpp Makefile
	@mkdir -p $(@D)
	$(CXX) $(WARNING) $(COMMONFLAGS) $(DEBUGFLAGS) -lz3 -MD -MP $< -o $@

$(PART1): %.a: build/%.o
	$< a < $(dir $@)txt/$(basename $(notdir $@)).txt
$(PART2): %.b: build/%.o
	$< b < $(dir $@)txt/$(basename $(notdir $@)).txt

clean:
	$(RM) $(CXX_OBJECTS) $(CXX_DEPENDS)


