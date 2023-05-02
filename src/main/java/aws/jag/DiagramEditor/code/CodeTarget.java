/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
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
    Path destination;
    public CodeTarget(Domain d) {
        out = output(d);
    }
    private Writer output(Domain d) {
        try {
            destination = genDir.resolve(d.getName() + ".java");
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
        try {
            out.append(s);
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget append(char s) {
        try {
            out.append(s);
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget append(Object s) {
        try {
            out.append(String.valueOf(s));
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget append(Expression e) {
        try {
            out.append(String.valueOf(e));
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget nl() {
        try {
            out.append("\n");
        } catch(IOException ex) {
        }
        return this;
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
