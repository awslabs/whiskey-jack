/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.code.toJava;

import aws.jag.DiagramEditor.code.CodeTarget;
import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.exl.*;
import java.util.*;

public class JavaTarget extends CodeTarget {
    public JavaTarget(Domain d) {
        super(d);
    }
    @Override
    public String extension() {
        return "java";
    }
    @Override
    public JavaTarget append(Expression e) {
        append1[e.getOperator().getType()].append(e);
        return this;
    }
    private final CodeWriter[] append1 = new CodeWriter[200];
    {
        Arrays.fill(append1, (CodeWriter) e -> {
            append('(');
            boolean first = true;
            for(var a: e.asArray()) {
                if(first) first = false;
                else append(e.getOperator().getBody());
                append(a);
            }
            append(')');
        });
        append1[Token.identifierType] = e -> append(e.getOperator().getBody());
        append1[Token.numberType] = e -> append(e.getOperator().getNum());
        append1[Token.stringType] = e -> {
            appendDoubleQuoted(e.getOperator().getBody());
        };
        append1[Vocabulary.INVOKE.getType()] = e->{
            append(e.arg(0));
            append('(');
            for(var i = 1; i<e.length(); i++) {
                if(i>1) append(", ");
                append(e.arg(i));
            }
            append(')');
        };
        append1[Vocabulary.DOT.getType()] = e->{
            append(e.arg(0));
            append('.');
            for(var i = 1; i<e.length(); i++) {
                if(i>1) append(".");
                append(e.arg(i));
            }
        };
        append1[Vocabulary.BLOCK.getType()] = e->{
            nl().append("{\n").indent();
            for(var i = 0; i<e.length(); i++)
                append(e.arg(i)).append(";\n");
            outdent().append("}\n");
        };
        append1[Vocabulary.RETURN.getType()] = e->{
            append("return");
            if(e.length()>0) {
                append(' ');
                append(e.arg(0));
            }
        };
        append1[Vocabulary.NEW.getType()] = e->{
            append("new");
            if(e.length()>0) {
                append(' ');
                append(e.arg(0));
            }
        };
        append1[Vocabulary.NULL.getType()] = e->{
            append("null");
        };
        append1[Vocabulary.TRUE.getType()] = e->{
            append("true");
        };
        append1[Vocabulary.FALSE.getType()] = e->{
            append("false");
        };
    }

    private interface CodeWriter {
        public void append(Expression e);
    }
}
