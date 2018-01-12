#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
using namespace std;

void usage()
{
	cout << "yl [-e \"Inline code\"] [file]" << endl;
}

int interactiveMode()
{
	return 0;
}

string CLI_FLAG_INLINE_CODE("-e");

stringstream getCode(int argc, char** argv)
{
	stringstream code;
	if (argc > 1 && argv[0] == CLI_FLAG_INLINE_CODE) {
		code << argv[1];
	} else {
		ifstream file(argv[0]);
		code << file.rdbuf();
		file.close();
	}
	return code;
}

int runProgram(int argc, char** argv)
{
	stringstream code = getCode(argc, argv);
	// TODO
	cout << code.rdbuf() << endl;
	return 0;
}

int main(int argc, char** argv)
{
	int ret = 0;
	if (argc < 2) {
		ret = interactiveMode();
	} else {
		ret = runProgram(argc - 1, &argv[1]);
	}
	return ret;
}


