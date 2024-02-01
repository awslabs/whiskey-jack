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
        append1[e.getOperator().getKind()].append(e);
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
        append1[Token.identifierKind] = e -> append(e.getOperator().getBody());
        append1[Token.numberKind] = e -> append(e.getOperator().getNum());
        append1[Token.stringKind] = e -> {
            appendDoubleQuoted(e.getOperator().getBody());
        };
        append1[Vocabulary.INVOKE.getKind()] = e->{
            append(e.get(0));
            append('(');
            for(var i = 1; i<e.size(); i++) {
                if(i>1) append(", ");
                append(e.get(i));
            }
            append(')');
        };
        append1[Vocabulary.DOT.getKind()] = e->{
            append(e.get(0), e);
            for(var i = 1; i<e.size(); i++) {
                append(".");
                append(e.get(i));
            }
        };
        append1[Vocabulary.BLOCK.getKind()] = e->{
            nl().append("{\n").indent();
            for(var i = 0; i<e.size(); i++)
                append(e.get(i)).append(";\n");
            outdent().append("}\n");
        };
        append1[Vocabulary.RETURN.getKind()] = e->{
            append("return");
            if(e.size()>0) {
                append(' ');
                append(e.get(0));
            }
        };
        append1[Vocabulary.NEW.getKind()] = e->{
            append("new");
            if(e.size()>0) {
                append(' ');
                append(e.get(0));
            }
        };
        append1[Vocabulary.NULL.getKind()] = e->{
            append("null");
        };
        append1[Vocabulary.TRUE.getKind()] = e->{
            append("true");
        };
        append1[Vocabulary.FALSE.getKind()] = e->{
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
