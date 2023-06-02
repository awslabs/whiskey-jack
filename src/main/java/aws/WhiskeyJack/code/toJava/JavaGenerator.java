/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.toJava;

import aws.WhiskeyJack.util.Utils;
import aws.WhiskeyJack.nodegraph.Port;
import aws.WhiskeyJack.nodegraph.Node;
import aws.WhiskeyJack.nodegraph.Domain;
import aws.WhiskeyJack.nodegraph.Type;
import aws.WhiskeyJack.code.CodeTarget;
import aws.WhiskeyJack.code.DomainGenerationController;
import aws.WhiskeyJack.code.Matches;
import aws.WhiskeyJack.exl.Tokenizer;
import aws.WhiskeyJack.exl.Expression;
import aws.WhiskeyJack.exl.Vocabulary;
import aws.WhiskeyJack.exl.Token;
import aws.WhiskeyJack.exl.Parser;
import java.io.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;

@Matches("*/java")
public class JavaGenerator implements DomainGenerationController {
    String error = null;
    CodeTarget out;
    Collection<Object> messages;
    @Override
    public void prescan(Collection<Object> m) {
        messages = m;
    }
    @Override
    public String generate(List<Node> nodes, CodeTarget out) {
        this.out = out;
        // TODO aws.WhiskeyJack.code.toJava.JavaGenerator.generate Not implemented
        System.out.println("  Whoo! " + this);
        var m = Pattern.compile("^([^.]*)\\..*").matcher(out.getPath().getFileName().toString());
        var cn = m.group(m.matches() ? 1 : 0);
        out.append("package ade.gen;\n\npublic class ")
                .append(cn)
                .append(" {\n")
                .indent();
        nodes.forEach(n -> n.sidecar(NodeGenerationInfo.class).setParent(this));
        nodes.forEach(n -> {
            Collection<String> header = new ArrayList<>();
            var info = n.sidecar(NodeGenerationInfo.class);
            header.add("Node " + n.getName() + " (" + n.getUid() + ")");
            n.forEachPort((Consumer<Port>) (p -> {
                header.add((p.isInputSide() ? "in  " : "out ") + p.getType()
                           + '\t' + p.getName()
                           + '\t' + p.getValue()
                           + '\t' + p.isConnected());
            }));
            messages.add(n.getUid());
            out.ln().comment(header);
            var code = n.getStringProp("code", null);
            var state = n.getStringProp("state", null);
            var callback = n.getStringProp("callback", null);
            var sti = out.getIndent();
            if(state != null)
                forAllBaseExpressionsIn(parse(state),
                        ist0 -> {
                    var ist = info.rewritePorts(ist0);
                    var t = ist.getOperator();
                    if(t == Vocabulary.DECLAREASSIGN || t == Vocabulary.DECLAREASSIGNFINAL) {
                        flushPendingInit();
                        var name = ist.arg(0);
                        var exp = ist.arg(1);
                        out.append("private ");
                        if(t == Vocabulary.DECLAREASSIGNFINAL)
                            out.append("final ");
                        out.append(typeFrom(exp))
                                .append(' ')
                                .append(name)
                                .append(" = ")
                                .append(exp)
                                .append(';')
                                .nl();
                    } else pendingInit.add(ist);
                });
            if(callback != null)
                forAllBaseExpressionsIn(parse(callback),
                        ist -> pendingInit.add(info.rewritePorts(ist)));
            flushPendingInit();
            out.append("void ").append(n.getUid()).append("(");
            var first = true;
            for(var inarg:info.inputs) {
                if(first) first = false;
                else out.append(", ");
                out.append(typeFrom(inarg.getType()))
                        .append(' ').append(inarg.getName());
            }
            out.append(") {\n").indent();
            for(var outarg:info.outputs) {
                out.append(typeFrom(outarg.getType()))
                        .append(' ').append(outarg.getName()).nl();
            }
            if(!Utils.isEmpty(code)) {
                var parsed = info.rewritePorts(parse(code));
                out.comment("Parsed " + code + " as " + parsed)
                        .append(parsed)
                        .append(";\n");
            }
            out.setIndent(sti).append("}\n");
        });
        out.setIndent(0).append("}\n");
        return error;
    }
    private Expression parse(String s) {
        try {
            if(s.indexOf(';') >= 0)
                s = "{" + s + ";}";
            return new Parser(new Tokenizer(s)).expression();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
            var msg = List.of("Error parsing", s, ex.toString());
            out.comment(msg);
            if(messages != null) messages.addAll(msg);
            return clean(Expression.of(Token.string(s)));
        }
    }
    static Expression clean(Expression e) {
        return e==null || e.getOperator()!=Vocabulary.BLOCK || e.length()!=1
                ? e
                : clean(e.arg(0));
    }
    private final Collection<Expression> pendingInit = new ArrayList<>();
    private void flushPendingInit() {
        if(!pendingInit.isEmpty()) {
            out.nl().append(Expression.of(Vocabulary.BLOCK, pendingInit)).nl();
            pendingInit.clear();
        }
    }
    private static String typeFrom(Expression e) {
        if(e == null) return "void";
        var op = e.getOperator();
        if(op.isNumber()) return "double";
        if(op.isString()) return "string";
        if(isBoolean == null) initOpProperties();
        if(isBoolean[op.getType()]) return "boolean";
        if(op == Vocabulary.NEW) {
            // (new (invoke T args))
            var ne = e.arg(0);
            if(ne.getOperator() == Vocabulary.INVOKE)
                return ne.arg(0).toString(); // TODO: pretty hacky
        }
        return "void";
    }
    private static String typeFrom(Type e) {
        if(e==Type.number) return "double";
        if(e==Type.string) return "String";
        if(e==Type.bool) return "boolean";
        if(e==Type.object) return "Object";
        if(e==Type.tuple) return "Map";
        return "void";
    }
    private static void forAllBaseExpressionsIn(Expression e, Consumer<Expression> func) {
        if(e != null)
            if(e.getOperator() == Vocabulary.BLOCK)
                for(var e2: e.asArray())
                    forAllBaseExpressionsIn(e2, func);
            else func.accept(e);
    }
    private static void initOpProperties() {
        {
            var p = new boolean[Token.typeTableSize()];
            Arrays.fill(p, false);
            p[Vocabulary.NOT.getType()] = true;
            p[Vocabulary.OR.getType()] = true;
            p[Vocabulary.OROR.getType()] = true;
            p[Vocabulary.INSTANCEOF.getType()] = true;
            p[Vocabulary.LT.getType()] = true;
            p[Vocabulary.LE.getType()] = true;
            p[Vocabulary.GT.getType()] = true;
            p[Vocabulary.GE.getType()] = true;
            p[Vocabulary.EQ.getType()] = true;
            p[Vocabulary.NE.getType()] = true;
            p[Vocabulary.ANDAND.getType()] = true;
            p[Vocabulary.AND.getType()] = true;
            p[Vocabulary.OR.getType()] = true;
            p[Vocabulary.OROR.getType()] = true;
            p[Vocabulary.TRUE.getType()] = true;
            p[Vocabulary.FALSE.getType()] = true;
            isBoolean = p;
        }
    }
    private static boolean[] isBoolean;
    @Override
    public CodeTarget makeOutput(Domain d) {
        return new JavaTarget(d);
    }
}
