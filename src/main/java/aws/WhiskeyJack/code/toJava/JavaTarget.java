/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.toJava;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.exl.*;
import java.util.*;

public class JavaTarget extends CodeTarget {
    private final String packageName;
    public JavaTarget(OuterBuildController context, String pn) {
        super(context);
        assert context!=null;
        packageName = pn;
    }
    @Override
    public String getDestinationExtension() {
        return "java";
    }
    @Override
    public JavaTarget append(Expression e) {
        append1[e.getOperator().getType()].append(e);
        return this;
    }
    public JavaTarget append(Expression e, Expression context) {
        var paren = Vocabulary.getPriority(e.getOperator()) 
                    < Vocabulary.getPriority(context.getOperator());
        if(paren) append('(');
        append(e);
        if(paren) append(')');
        return this;
    }
    private final CodeWriter[] append1 = new CodeWriter[200];
    {
        Arrays.fill(append1, (CodeWriter) e -> {
            var first = true;
            for(var a: e.asArray()) {
                if(first) first = false;
                else append(e.getOperator().getBody());
                append(a, e);
            }
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
            append(e.arg(0), e);
            for(var i = 1; i<e.length(); i++) {
                append(".");
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
    @Override
    public String getCodeDirectoryName() {
        return "src/main/java/"+packageName.replace('.', '/');
    }

    private interface CodeWriter {
        public void append(Expression e);
    }
}
