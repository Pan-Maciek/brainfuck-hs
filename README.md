# Brainfuck-hs
This program compiles `.bf` files into `.c` programs.

## Usage
`./BF fileIn [-o fileOut]`

### Compilation
* Make sure you have access to `ghc` on you system.
* Compile using `ghc Program.hs -o BF`

### Running examples
* Compile `.bf` to `.c` using `./BF examples/hello-world.bf`
* Compile `.c` to executable using `gcc examples/hello-world.c -o hello`
* Run example `./hello`