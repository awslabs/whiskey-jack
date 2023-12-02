/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.exl.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.util.*;

public abstract class CodeTarget implements Closeable {
    protected final OuterBuildController context;
    private Writer out;
    private int indent = 0;
    private boolean bol = true; // beginning of line
    private Path destination;
    private Path codeRootDirectory;
    protected Domain domain;
    public CodeTarget(OuterBuildController c) {
        assert c != null;
        context = c;
    }
    public void start(Domain d) {
        domain = d;
        try {
            codeRootDirectory = context.getCodePartDirectory(d, "code");
            var codeDirectory = codeRootDirectory.resolve(getCodeDirectoryName());
            try {
                Files.createDirectories(codeDirectory);
            } catch(IOException xyzzy) {
                /* ignore */ }
            destination = codeDirectory.resolve(getDestinationName() + "." + getDestinationExtension());
            System.out.println("codePart " + codeRootDirectory + "\n\tgen " + destination);
            out = Files.newBufferedWriter(destination,
                    StandardCharsets.UTF_8, StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING);
        } catch(IOException ex) {
            context.error(ex);
            out = new PrintWriter(System.out);
        }
    }
    public Path getPath() {
        return destination;
    }
    public Path getCodeRootDirectory() {
        return codeRootDirectory;
    }
    public String getDestinationName() {
        return domain.getName();
    }
    public Domain getDomain() { return domain; } 
    public String getDestinationExtension() {
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
                for(var i = indent; --i >= 0;)
                    out.append("    ");
                bol = false;
            }
            out.append(s);
        } catch(IOException ex) {
        }
        return this;
    }
    public CodeTarget nl() {
        if(!bol) append('\n');
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
    public CodeTarget appendDoubleQuoted(CharSequence s) {
        append('"');
        appendQuoted(s);
        append('"');
        return this;
    }
    public CodeTarget appendSingleQuoted(CharSequence s) {
        append('\'');
        appendQuoted(s);
        append('\'');
        return this;
    }
    public CodeTarget appendQuoted(CharSequence s) {
        var limit = s.length();
        for(var i = 0; i < limit; i++) {
            var c = s.charAt(i);
            switch(c) {
                case '\n' -> {
                    append("\\n");
                    continue;
                }
                case '\t' -> {
                    append("\\t");
                    continue;
                }
                case '\r' -> {
                    append("\\r");
                    continue;
                }
                case '\b' -> {
                    append("\\b");
                    continue;
                }
                case '\f' -> {
                    append("\\f");
                    continue;
                }
                case '\0' -> {
                    append("\\0");
                    continue;
                }
                case '\'', '"' -> append('\\');
            }
            append(c);
        }
        return this;
    }
    public CodeTarget ln() {
        return append("\n");
    }
    public CodeTarget comment(String... s) {
        comment(List.of(s));
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
    public CodeTarget openBrace() {
        append('{').nl().indent();
        return this;
    }
    public CodeTarget closeBrace() {
        outdent().nl().append('}').nl();
        return this;
    }
    public int getIndent() {
        return indent;
    }
    public CodeTarget setIndent(int i) {
        indent = i;
        return this;
    }
    public Writer getWriter() { return out; }
    @Override
    @SuppressWarnings({"PMD.AvoidCatchingThrowable", "UseSpecificCatch"})
    public void close() {
        Utils.close(out);
        try {
            new Exec().withExec("/usr/bin/open", destination.toString()).background(n ->
            {
                if(n != 0)
                    System.out.println("Result open failed->" + n);
            });
        } catch(Throwable ex) {
            ex.printStackTrace(System.out);
        }
    }
    public CodeTarget comment(Collection<String> s) {
        if(s != null && !s.isEmpty()) {
            if(preComment != null) append(preComment).append('\n');
            s.forEach(c -> {
                if(onComment != null) append(onComment);
                append(c).append('\n');
            });
            if(postComment != null) append(postComment).append('\n');
        }
        return this;
    }
    public abstract String getCodeDirectoryName();
    protected String preComment = "/*";
    protected String onComment = " * ";
    protected String postComment = " */";
}
