# tiny-lc-compiler-anvenkat09

Notes:

Lambda Calculus to Javascript Parser

This project was written using Standard-ML or SML. It segments the input
file data into tokens, builds an abstract syntax tree, and outputs the desired
result as Javascript Code. 

Steps:

- To run this project, place all the files provided in the same directory. 
- Compile the files using the sml compiler in the unix command line
- When compiling "OutputJS.sml", call the function printAST ("filename"); upon standard
      input. This triggers the program and passes in the specified file. 
*****filename has to be in quotes*****

- The resulting output to javascript code is written to the file
  "testOutput.js" (It will be created if it doesn't exist already).
- This code can then be compiled using a Javascript Compiler.

***Important Note
I could not get the lambda symbol to work in vim (it's supposed to work with ctrl+k+|+*),
so I ended up just converting all the symbols to the word "lambda".
