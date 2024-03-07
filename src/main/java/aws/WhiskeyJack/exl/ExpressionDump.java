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
        fileHeader(dc);
        if(!dc.declarations.isEmpty()) {
            comment("Declarations");
            for(var e: dc.declarations)
                if(dc.used(e)) declareGlobal(e);
        }
        if(!dc.setup.isEmpty()) {
            comment("Setup");
            appendSetup(Expression.of(Vocabulary.BLOCK, dc.setup));
        }
        if(!dc.functionInfo.isEmpty()) {
            comment("Functions");
            for(var e: dc.functionInfo.values())
                toCol(0).append(e);
        }
        fileEnder(dc);
        return this;
    }
    public ExpressionDump append(char c) {
        try {
            out.append(c);
            if(c == '\n') {
                col = 0;
                if(commentDepth > 0) out.append(commentContinuer());
            } else col++;
        } catch(IOException ex) {
        }
        return this;
    }
    public void appendSetup(Expression e) {
        append(e);
    }
    public ExpressionDump append(CharSequence s) {
        var limit = s.length();
        for(var i = 0; i < limit; i++)
            append(s.charAt(i));
        return this;
    }
    public ExpressionDump appendQuoted(CharSequence s) {
        try {
            Utils.deepToStringQuoted(s, out, Integer.MAX_VALUE);
        } catch(IOException ex) {
        }
        return this;
    }
    public void fileHeader(DomainCode dc) {
        comment("Code for domain " + dc.getDomain());
    }
    public void fileEnder(DomainCode dc) {
    }
    public void declareGlobal(Expression e) {
        append(e);
        append('\n');
    }
    public ExpressionDump toTab(int n) {
        toCol(n * tabsize);
        return this;
    }
    public ExpressionDump toCol(int n) {
        if(n < col) append('\n');
        while(col < n) append(' ');
        return this;
    }
    public String commentStartString() {
        return "/* ";
    }
    public String commentEndString() {
        return " */";
    }
    public String commentContinuer() {
        return " * ";
    }
    public ExpressionDump startComment() {
        if(++commentDepth == 1) append(commentStartString());
        return this;
    }
    public ExpressionDump endComment() {
        if(--commentDepth == 0) append(commentEndString());
        return this;
    }
    public ExpressionDump comment(String s) {
        toCol(0);
        startComment();
        append(s);
        endComment();
        toCol(0);
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
