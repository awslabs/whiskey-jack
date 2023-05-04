/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.code;

import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.DiagramEditor.util.*;
import aws.jag.exl.Expression;
import java.io.*;
import java.nio.file.*;
import java.util.*;

public class CodeTarget implements Closeable {
    public static final Path genDir = Path.of("/tmp/wjg");
    private final Writer out;
    private int indent = 0;
    private boolean bol = true; // beginning of line
    private Path destination;
    public CodeTarget(Domain d) {
        out = output(d);
    }
    private Writer output(Domain d) {
        try {
            destination = genDir.resolve(d.getName() + "." + extension());
            return Files.newBufferedWriter(destination, StandardOpenOption.CREATE);
        } catch(IOException ex) {
            return new PrintWriter(System.out);
        }
    }
    public Path getPath() {
        return destination;
    }
    public String extension() {
        return "txt";
    }
    public CodeTarget append(CharSequence s) {
        var limit = s.length();
        for(var i = 0; i < limit; i++)
            append(s.charAt(i));
        return this;
    }
    public CodeTarget append(char s) {
        try {
            if(s == '\n') bol = true;
            else if(bol) {
                for(int i = indent; --i >= 0;)
                    out.append("    ");
                bol = false;
            }
            out.append(s);
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget appendln(CharSequence s) {
        return append(s).ln();
    }
    public CodeTarget append(Object s) {
        return append(String.valueOf(s));
    }
    public CodeTarget append(Expression e) {
        return append(String.valueOf(e));
    }
    public CodeTarget ln() {
        return append("\n");
    }
    public CodeTarget comment(CharSequence... s) {
        if(s != null && s.length > 0) {
            append("/*\n");
            for(var c: s)
                append(" * ").append(c).append('\n');
            append(" */\n");
        }
        return this;
    }
    public CodeTarget indent() {
        indent++;
        return this;
    }
    public CodeTarget outdent() {
        if(indent > 0) indent--;
        return this;
    }
    public int getIndent() {
        return indent;
    }
    public CodeTarget setIndent(int i) {
        indent = i;
        return this;
    }
    @Override
    @SuppressWarnings({"PMD.AvoidCatchingThrowable", "UseSpecificCatch"})
    public void close() {
        Utils.close(out);
        try {
            new Exec().withExec("/usr/bin/open", destination.toString()).background(n ->
                    System.out.println("Exec->" + n));
        } catch(Throwable ex) {
            ex.printStackTrace(System.out);
        }
    }
    public CodeTarget comment(Collection<String> s) {
        if(s != null && !s.isEmpty()) {
            append("/*\n");
            s.forEach(c -> append(" * ").append(c).append('\n'));
            append(" */\n");
        }
        return this;
    }
    static {
        try {
            Files.createDirectories(genDir);
        } catch(IOException ex) {
        }
    }
}
