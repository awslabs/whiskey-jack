/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.code.toJava;

import aws.jag.DiagramEditor.code.CodeTarget;
import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.exl.*;
import java.util.*;

public class JavaTarget extends CodeTarget {
    public JavaTarget(Domain d) { super(d); }
    @Override
    public String extension() { return "java"; }
    @Override
    public JavaTarget append(Expression e) {
        append1[e.kind().getType()].append(e);
        return this;
    }
    private final CodeWriter[] append1 = new CodeWriter[Tokenizer.typeTableSize()];
    {
        Arrays.fill(append1, (CodeWriter) e -> {
            append('(');
            boolean first = true;
            for(var a: e.asArray()) {
                if(first) first = false;
                else append(e.kind().getBody());
                append(a);
            }
            append(')');
        });
        append1[Token.identifierType] = e -> append(e.kind().getBody());
        append1[Token.numberType] = e -> append(e.kind().getNum());
        append1[Token.stringType] = e -> {
            append(e.kind().getBody());
        };
    }
    private interface CodeWriter {
        public void append(Expression e);
    }
}
