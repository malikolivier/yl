package com.boussejra.yl;

import com.boussejra.yl.Program;
import com.boussejra.yl.interpreter.Var;
import com.boussejra.yl.parser.ParseException;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class Main {
    public static void main(String[] args) {
        if (args.length == 0) {
            interactiveMode();
        } else {
            try {
                Program program = getProgram(args);
                Var ret = program.run();
                System.exit(ret.toInt());
            } catch (IOException e) {
                System.err.println(e);
                System.exit(1);
            } catch (ParseException e) {
                System.err.println(e);
                System.exit(1);
            }
        }
    }

    private static Program getProgram(String[] args) throws ParseException, IOException {
        if (args.length > 1 && args[0].equals("-e")) {
            String[] ylArgs = Arrays.copyOfRange(args, 2, args.length);
            return new Program(args[1], ylArgs);
        } else {
            byte[] encoded = Files.readAllBytes(Paths.get(args[0]));
            String code = new String(encoded, StandardCharsets.UTF_8);;

            String[] ylArgs = Arrays.copyOfRange(args, 1, args.length);
            return new Program(code, ylArgs);
        }
    }

    private static void interactiveMode() {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            do {
                System.out.print("> ");
                String code = reader.readLine();
                try {
                    Program program = new Program(code, new String[0]);
                    Var ret = program.run();
                    System.out.println(ret);
                } catch(ParseException e) {
                    System.err.println(e);
                }
            } while (true);
        } catch(IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
