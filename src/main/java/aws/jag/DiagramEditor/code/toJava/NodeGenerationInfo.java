/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.code.toJava;

import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.exl.*;
import java.io.*;
import java.util.*;
import java.util.function.*;


public class NodeGenerationInfo {
    private JavaGenerator gen = null;
    private final Node node;
    private final Map<String, Expression> portValues = new HashMap<>();
    Collection<Port> inputs = new ArrayList<>();
    Collection<Port> outputs = new ArrayList<>();
    
    public NodeGenerationInfo(Node n) {
        node = n;
        node.forEachPort(new Consumer<Port>() {
            @Override
            public void accept(Port p) {
                var t = p.getType();
                var k = p.getName();
                if(p.isConnected()) {
                    (p.isInputSide() ? inputs : outputs).add(p);
                } else {
                    var nv = p.getValue();
                    var v = switch(nv) {
                        case String str ->
                            t == Type.string
                            ? Expression.of(Token.string(str))
                            : parse(str);
                        case Expression ev ->
                            ev;
                        case Number num ->
                            Expression.of(Token.number(num));
                        case null ->
                            Expression.of(t == Type.string ? emptyString : Vocabulary.NULL);
                        default ->
                            Expression.of(Token.string(String.valueOf(nv)));
                    };
                    System.out.println("rewrite " + k + " to " + v);
                    portValues.put(k, v);
                }
            }
        });
    }
    public NodeGenerationInfo setParent(JavaGenerator g) {
        gen = g;
        return this;
    }
    Expression rewritePorts(Expression e) {
        return e.rewrite(exNode -> {
            var op = exNode.getOperator();
            if(op.isIdentifier()) {
                System.out.println("Found Identifier " + exNode);
                var foundValue = portValues.get(op.getBody());
                if(foundValue != null) {
                    System.out.println("  with value " + foundValue);
                    return foundValue;
                }
            }
            return exNode;
        });
    }
    private Expression parse(String s) {
        try {
            if(s.indexOf(';') >= 0)
                s = "{" + s + ";}";
            return new Parser(new Tokenizer(s)).expression();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
            List<String> msg = List.of("Error parsing", s, ex.toString());
            gen.out.comment(msg);
            if(gen.messages != null) gen.messages.addAll(msg);
            return JavaGenerator.clean(Expression.of(Token.string(s)));
        }
    }

    static String uniqueName(Port p) {
        if(p.isInputSide() && p.nArcs()==1)
            p = p.getArc(0).otherEnd(p);
        return p==null ? "NULLPORT" : p.within.getUid()+"_"+p.getName();
    }
    static String accessor(String prefix, Port p) {
        var nm = uniqueName(p);
        return prefix+Character.toUpperCase(nm.charAt(0))+nm.substring(1);
    }
    static String getter(Port p) {
        return accessor(p.getType()==Type.bool ? "is" : "get", p);
    }
    static String setter(Port p) {
        return accessor("set", p);
    }
    private static final Token emptyString = Token.string("");
}
