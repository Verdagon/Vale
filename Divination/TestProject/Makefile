CC ?= clang++
# Resolve paths relative to this Makefile so the TestProject works regardless of CWD
# and regardless of where Divination itself was cloned/vendored.
MAKEFILE_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
DIVINATION_REPO_ROOT := $(abspath $(MAKEFILE_DIR)/..)
# Override DIVINATION on the command line to point at a different binary (release build, etc.).
DIVINATION ?= $(DIVINATION_REPO_ROOT)/target/debug/Divination
# cargo cbuild writes its output under rust/target/<triple>/release. The triple is detected
# at runtime by Divination via `rustc -vV`; we have to match that here for pkg-config to
# find the staticlib. Override TARGET_TRIPLE on the command line if your host differs.
TARGET_TRIPLE ?= $(shell rustc -vV | sed -n 's/^host: //p')
EXAMPLE_PROJECT_DIR := $(MAKEFILE_DIR)rust/target/$(TARGET_TRIPLE)/release
export PKG_CONFIG_PATH := $(EXAMPLE_PROJECT_DIR):$(PKG_CONFIG_PATH)

CFLAGS = -Wall -Wextra -Wl,-dead_strip -ffunction-sections -fdata-sections
INCLUDES = `pkg-config --cflags --static rust_deps`
LIBS = `pkg-config --libs --static rust_deps`

all:	build ccatter	tests

build:
	mkdir -p build

main: build/main.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/main $(LIBS)
# 	build/main || (echo "catter test failed!"; exit 1)

build/main.o:	src/main.c build	rust
	@echo "Doing main..."
	grep '^#pragma rs' src/main.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@



strlen: build/strlen.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/strlen $(LIBS)
	build/strlen || (echo "strlen test failed!"; exit 1)
	@echo strlen test succeeded!

build/strlen.o:	src/strlen.c build	rust
	@echo "Doing strlen test..."
	@grep '^#pragma rs' src/strlen.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@

mkdir: build/mkdir.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/mkdir $(LIBS)
	- rmdir x
	build/mkdir || (echo "mkdir test failed!"; exit 1)
	@echo mkdir test succeeded!

build/mkdir.o:	src/mkdir.c build	rust
	@echo "Doing mkdir test..."
	@grep '^#pragma rs' src/mkdir.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@

reverse: build/reverse.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/reverse $(LIBS)
	build/reverse || (echo "reverse test failed!"; exit 1)
	@echo reverse test succeeded!

build/reverse.o:	src/reverse.c build	rust
	@echo "Doing reverse test..."
	@grep '^#pragma rs' src/reverse.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@

catter: build/catter.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/catter $(LIBS)
	build/catter || (echo "catter test failed!"; exit 1)
	@echo catter test succeeded!

build/catter.o:	src/catter.c build	rust
	@echo "Doing catter test..."
	@grep '^#pragma rs' src/catter.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@

regex: build/regex.o
	echo $(CC) $(CFLAGS) $(INCLUDES) $^ -o $@ $(LIBS)
	$(CC) $(CFLAGS) $(INCLUDES) $^ -o build/regex $(LIBS)
	build/regex || (echo "regex test failed!"; exit 1)
	@echo regex test succeeded!

build/regex.o:	src/regex.c build	rust
	@echo "Doing regex test..."
	@grep '^#pragma rs' src/regex.c | $(DIVINATION) --crate std --cargo_toml Dependencies.toml --output_dir rust instantiate
	echo $(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@
	$(CC) $(CFLAGS) $(INCLUDES) -I . -c $< -o $@

# tests:	catter reverse mkdir strlen regex
# 	@echo "All tests succeeded!"

rust:
	mkdir rust

.PHONY: clean
clean:
	rm -rf *.o *.d *.h rust build clibthing
