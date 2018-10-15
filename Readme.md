# Implementation of Syntax analyzer.
Done by Alex Gospodchikov and Landysh Galieva

Group: SE

Language: Swift 4
We implemented swift 4 because this version has the most complete documentation.

## Requirement.
To run our program, a Python 3.7 must be installed and added [Python Lex-Yacc](https://github.com/dabeaz/ply/).
The code used for testing should be compiled by [default compilator](http://online.swiftplayground.run/) and should not contain what is written in the "What we didn't implemented" part. Otherwise, we can't guarantee the correct operation of our analyzer.


## How to run.
You can run our code from parser.py

## Testing.
Tests are entered only from a file. Your tests should be in the root folder and named `in.txt`.
The output of the program occurs in `out.txt`.
In the root folder is the folder `Test` which contains tests that you can use to verify the correct work of our code.
We recommend you to read bnf.docx (You can this file in the root folder) or sections "What we implemented" and "What we don't implement". 
# What we implemented.
* imports 
* functions (with params, function calls) 
* cycles 
* if's 
* simple expressions (with +-*/() ) 
* string and number literals 
* variables, assignments
 
Note for json tree:
* Optional function IDs are not displayed to improve readability. 
* If the variable is optional (can be of type nil), it will be displayed in the tree sub-item

# What we didn't implemented.
* tuple
* generic 
* array's literal
* creating own operator
* presence group

## Reference.
[https://developer.apple.com/documentation/swift](https://developer.apple.com/documentation/swift)
