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
	ScopeContainer scope;
	do {
		stringstream codeStream;
		cout << "> ";
		getline(cin, codeInput);
		codeStream << codeInput;
		Ast program(codeStream);
		try {
			Var ret = scope.evaluate(program, false);
			cout << ret << endl;
		} catch (const char* e) {
			cerr << "ERROR: " << e << endl;
		}
	} while (true);
	return 0;
}

string CLI_FLAG_INLINE_CODE("-e");

stringstream getCodeAndSetArgv(int argc, char** argv)
{
	stringstream code;
	if (argc > 1 && argv[0] == CLI_FLAG_INLINE_CODE) {
		code << argv[1];
		ylSetArgv(argc - 2, &argv[2]);
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
		ylSetArgv(argc - 1, &argv[1]);
	}
	return code;
}

int runProgram(int argc, char** argv)
{
	stringstream code = getCodeAndSetArgv(argc, argv);
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
			cerr << "ERROR: " << e << endl;
			ret = 1;
		} catch (const char* e) {
			cerr << "ERROR: " << e << endl;
			ret = 1;
		}
	}
	return ret;
}
