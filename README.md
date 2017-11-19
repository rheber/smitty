# README

Smitty Werben Man Jensen is a procedural language currently implemented with
GHC 7.10 and Happy 1.19.

## Table of Contents
- [Usage](#usage)
  * [Building](#building)
  * [Execution](#execution)
  * [Cleaning](#cleaning)
- [Syntax](#syntax)
  * [Assignment](#assignment)
  * [Rational Arithmetic](#rational-arithmetic)
  * [Booleans](#booleans)
  * [Selection](#selection)
  * [Iteration](#iteration)
  * [Abstraction](#abstraction)
  * [Builtins](#builtins)

## Usage

### Building

Run `happy Grammar.y && ghc smitty`.

### Execution

To start the REPL, run `smitty`.

To execute the file foo.1, run `smitty -f foo.1`.

### Cleaning

Run `git clean -fx`.

## Syntax

### Assignment

```
x := "Initialise x";
x ::= "Reassign x";
x ::= "Reassign x again";
x := "Whoops!"; # Variable already initialised #

y ::= "Whoops!"; # Variable not yet initialised #
y := "Initialise y";

z := "Initialise z";
(~z); # Delete z #
z := "Initialise z again";
```

### Rational Arithmetic

```
4 + 8 # 12 / 1 #
4 - 8 # -4 / 1 #
4 * 8 # 32 / 1 #
4 / 8 # 1 / 2 #
```

### Booleans

```
:) # True #
:( # False#

(!:))    # :( #
:) || :( # :) #
:) && :( # :( #

4 == 8 # :( #
4 != 8 # :) #
4 >= 8 # :( #
4 <= 8 # :) #
```

### Selection

```
x := 42;
? (x > 42) {
  print("If case\n");
} :: (x < 42) {
  print("Else if case\n");
} :: {
  print("Else case\n");
}
```

### Iteration

```
i := 0;
@ {
  print("Executes before condition is tested.\n");
} (i < 3) {
  i ::= i + 1;
  print("Executes after condition is tested.\n");
}
```

### Abstraction

```
double := \(x) {
  $ 2 * x;
}
double(4) # 8 / 1 #
```

### Builtins

```
approx(4 / 8)     # "0.5" #
exp(4, 8)         # 65536 / 1 #
floor(4 / 8)      # 0 / 1 #
input()           # Reads string from stdin #
print("Ahoy!\n"); # Prints argument to stdout #
quit(0)           # Quits with the given exit code #
```
