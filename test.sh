#!/usr/bin/env bash

set -o errexit

if [ "$1" = "node" ]; then
	echo -n "Node	"
	INTERPRETER="node node/yl.js"
elif [ "$1" = "python" ]; then
	echo -n "Python3	"
	INTERPRETER="python3 python/yl.py"
elif [ "$1" = "c" ]; then
	echo -n "C	"
	INTERPRETER="c/yl"
else
	# Run all languages!
	$0 node
	$0 python
	$0 c
	exit
fi

runtest () {
	CODE="$1"
	EXPECTED="$2"
	OUTPUT=$($INTERPRETER -e "$CODE")
	if [ "$OUTPUT" = "$EXPECTED" ]; then
		echo -n "."
		return 0
	else
		echo
		echo "[ERROR] Expected '$CODE' to output '$EXPECTED', but got '$OUTPUT'"
		return 1
	fi
}

runtest '' ''
runtest "(print 1)" "1"
runtest "(print (! ()))" "1"
runtest "(print (! 1))" "()"
runtest "(print (= 0 1))" "()"
runtest "(print (= 0 0))" "1"
runtest "(print (< 0 1))" "1"
runtest "(print (<= 0 1))" "1"
runtest "(print (<= ab ac))" "1"
runtest "(print (> 0 1))" "()"
runtest "(print (>= 0 0))" "1"
runtest "(print (+ 1 1))" "2"
runtest '(let x 1) (print x)' "1"
runtest '(def echo (n) n) (print (echo "ds"))' 'ds'
runtest '(def do_print (x) (print x)) (do_print 1)' '1'
runtest '(let ret 1) (if ret ( (print "TRUE") ))' 'TRUE'
runtest '(let ret ()) (if ret ( (print "TRUE") ))' ''
runtest '(let ret ()) (if ret ( (print "TRUE") ) ( (print "FALSE") ))' 'FALSE'
runtest "(print (if () (1)))" "()"
runtest '(loop x (0 1 2) ( (print x) ))' '0
1
2'
runtest '(loop x (range 3) ( (print x) ))' '0
1
2'
runtest '(loop x (range 1 3) ( (print x) ))' '1
2'
runtest '(def noop ()) (print (noop))' '()'
runtest '(def print_let () (let x 1) (print x)) (print_let)' '1'

echo
