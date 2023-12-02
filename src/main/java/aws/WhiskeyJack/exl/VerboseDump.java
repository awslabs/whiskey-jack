/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

public class VerboseDump extends ExpressionDump {
    @Override
    public void append(Expression e) {
        append(e, 0, 0);
    }
    @Override
    public void append(DomainCode.FunctionInfo e) {
        append(e.returnType.getName())
            .append(' ')
            .append(e.name)
            .append('(');
        var first = true;
        for(var arg: e.args) {
            if(first) first = false;
            else append(',');
            append(arg, 0, 0);
        }
        append(") {\n");
        appendStatement(e.body, 1);
        toCol(0);
        append("}\n");
    }
    private void appendStatement(Expression e, int indent) {
        if(e != null) {
            toTab(indent);
            append(e, indent, 0);
        }
    }
    private void append(Expression e, int indent, int outerprior) {
        if(e != null) {
            var op = e.getOperator();
            if(e.isLeaf())
                if(op.isString()) appendQuoted(op.getBody());
                else append(op.getBody());
            else if(op == Vocabulary.BLOCK) {
                append('{');
                for(var arg: e.asArray())
                    appendStatement(arg, indent + 1);
                append('}');
            } else if(op == Vocabulary.DECLARE) {
                append("var ").append(e.getType().toString());
                for(var arg: e.asArray()) {
                    append(" ");
                    append(arg, indent + 1, 0);
                }
            } else if(op == Vocabulary.INVOKE) {
                var args = e.asArray();
                var len = args.length;
                append(args[0],indent+1,0);
                append('(');
                for(var i=1; i<len; i++) {
                    if(i>1) append(',');
                    append(args[i], indent + 1, 0);
                }
                append(')');
            } else {
                var prior = getPriority(op);
                var args = e.asArray();
                if(prior == 0) {
                    append(op.getBody());
                    append('(');
                    for(var arg: args) {
                        append(' ');
                        append(arg, indent + 1, prior);
                    }
                    append(')');
                } else if(args.length == 1) {
                    append(op.getBody());
                    append(args[0], indent + 1, prior);
                } else {
                    var paren = prior < outerprior;
                    if(paren) append('(');
                    var first = true;
                    for(var arg: args) {
                        if(first) first = false;
                        else append(op.getBody());
                        append(arg, indent + 1, prior);
                    }
                    if(paren) append(')');
                }
            }
        }
    }
    protected int[] getPriorityTable() {
        return Vocabulary.getStandardPriorityTable();
    }
    protected final int getPriority(Token t) {
        return getPriorityTable()[t.getKind()];
    }

}
