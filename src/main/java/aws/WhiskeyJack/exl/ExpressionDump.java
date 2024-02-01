/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.exl.DomainCode.FunctionInfo;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.nio.file.*;

public abstract class ExpressionDump implements Closeable {
    private static final int tabsize = 4;
    private Appendable out;
    private int col;
    private int commentDepth = 0;
    public ExpressionDump to(Appendable o) {
        out = o;
        return this;
    }
    public ExpressionDump to(Path o) throws IOException {
        return to(Files.newBufferedWriter(o));
    }
    abstract public void append(Expression e);
    abstract public void append(FunctionInfo e);
    public ExpressionDump append(DomainCode dc) {
        append("\n// code for " + dc.domain + "\n");
        if(!dc.declarations.isEmpty()) {
            append("\n// Declarations\n");
            for(var e: dc.declarations) if(dc.used(e)) {
//                System.out.println("DCL " + e);
                append(e);
                append('\n');
            }
            else {
//                System.out.println("DCL unused "+e);
                toCol(0).startComment().append(" unused ");
                append(e);
                endComment();
            }
        }
        if(!dc.setup.isEmpty()) {
            append("\n// Setup\n");
            append(Expression.of(Vocabulary.BLOCK, dc.setup));
        }
        if(!dc.functionInfo.isEmpty()) {
            append("\n// Functions\n");
            for(var e: dc.functionInfo.values())
                toCol(0).append(e);
        }
        return this;
    }
    protected ExpressionDump append(char c) {
        try {
            out.append(c);
        } catch(IOException ex) {
        }
        col = c == '\n' ? 0 : col + 1;
        return this;
    }
    protected ExpressionDump append(CharSequence s) {
        var limit = s.length();
        for(var i = 0; i < limit; i++)
            append(s.charAt(i));
        return this;
    }
    protected ExpressionDump appendQuoted(CharSequence s) {
        try {
            Utils.deepToStringQuoted(s, out, Integer.MAX_VALUE);
        } catch(IOException ex) {
        }
        return this;
    }
    protected ExpressionDump toTab(int n) {
        toCol(n * tabsize);
        return this;
    }
    protected ExpressionDump toCol(int n) {
        if(n < col) append('\n');
        while(col < n) append(' ');
        return this;
    }
    protected String commentStartString() { return "/* "; }
    protected String commentEndString() { return " */"; }
    protected ExpressionDump startComment() {
        if(++commentDepth==1) append(commentStartString());
        return this;
    }
    protected ExpressionDump endComment() {
        if(--commentDepth==0) append(commentEndString());
        return this;
    }
    @Override
    public void close() {
        if(out instanceof Closeable c) 
        try {
            c.close();
        } catch(IOException ex) {
        }
    }
}
