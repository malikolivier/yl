#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "interpreter.hh"
#include "parser.hh"

using namespace std;

int interactiveMode()
{
	string codeInput;
	Scope scope = Scope::generateGlobalScope();
	do {
		stringstream codeStream;
		cout << "> ";
		getline(cin, codeInput);
		codeStream << codeInput;
		Ast program(codeStream);
		Var ret = scope.evaluate(program, false);
		cout << ret << endl;
	} while (true);
	return 0;
}

string CLI_FLAG_INLINE_CODE("-e");

stringstream getCode(int argc, char** argv)
{
	stringstream code;
	if (argc > 1 && argv[0] == CLI_FLAG_INLINE_CODE) {
		code << argv[1];
	} else {
		ifstream file(argv[0], ifstream::in);
		if (!file.good()) {
			string err;
			err += "Could not open file '";
			err += argv[0];
			err += "'!";
			throw err;
		}
		code << file.rdbuf();
	}
	return code;
}

int runProgram(int argc, char** argv)
{
	stringstream code = getCode(argc, argv);
	Ast program(code);
	Var ret = evaluate(program);
	return ret.toInt();
}

int main(int argc, char** argv)
{
	int ret = 0;
	if (argc < 2) {
		ret = interactiveMode();
	} else {
		try {
			ret = runProgram(argc - 1, &argv[1]);
		} catch (string e) {
			cout << e << endl;
			ret = 1;
		}
	}
	return ret;
}
