/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.nodegraph.Node;
import java.io.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

/**
 * The code for a whole domain, as a collection of abstract expressions, with no
 * relationship to any particular programming language. This converts a
 * collection of nodes into a collection of expressions that can be used to
 * perform the computation specified by the nodes.
 *
 * An important thing to understand is the dichotomy between input and output
 * ports. (Port.isInput()). For the purpose of graphical rendering, input ports
 * are drawn on the left side of a node, and output ports are drawn on the right
 * side of a node. From a behavioral/algebraic point of view, the outputs are
 * the clients, the inputs are the servers. caller/callee, requestor/requestee,
 * there are lots of variations of the terminology. The output side of one node
 * makes requests to the input side of another node.
 *
 * This is <b>not</b> the same as the direction of data flow. It is just control
 * flow. For example, when a request is made to a database, it can be to either
 * store or retrieve data.
 */
public class DomainCode {
    Class<VerboseDump> dumper = VerboseDump.class;
    final Map<String, NodeCode> codes = new HashMap<>();
    /* Every domain ends up with three pools of code fragments: */
    final Map<String, FunctionInfo> functionInfo = new HashMap<>();
    final List<Expression> declarations = new ArrayList<>(); // globals
    final List<Expression> setup = new ArrayList<>(); // initialization
    final Map<Port, PortInfo> globals = new HashMap<>();
    Domain domain = Domain.err;
    public DomainCode(Collection<Node> nodes) {
        nodes.forEach(n -> codes.put(n.getUid(), new NodeCode(n)));
        codes.values().forEach(nc -> nc.addCode());
    }
    static final private ExpressionEvolutionCollector out = new ExpressionEvolutionCollector();
    public void optimize() {
        out.dump(this);
        functionInfo.values().forEach(fi -> fi.inlineFunctions());
        out.dump(this);
    }

    class NodeCode {
        private final Map<String, PortInfo> portInfo = new HashMap<>();
        private final List<PortInfo> inputs = new ArrayList<>();
        private final List<PortInfo> outputs = new ArrayList<>();
        private final Expression code;
        private final Map<String, Expression> portExpressions = new HashMap<>();

        final Node node;
        NodeCode(Node n) {
            node = n;
            domain = n.getDomain();
            n.forEachPort((Consumer<Port>) p -> {
                var pi = new PortInfo(p);
                portInfo.put(p.getName(), pi);
                    globals.put(p, pi);
                    addSetup(declare(p.getType(), ide(pi.uname)));
                if(pi.isInput()) {
                    inputs.add(pi);
                }
                if(pi.isOutput()) outputs.add(pi);
            });
            portInfo.values().forEach(pi -> pi.addRewrites(portExpressions));
            code = codeProp("code");
            addSetup(codeProp("state"));
            addSetup(codeProp("callback"));
        }
        final Expression codeProp(String k) {
            var s = node.getStringProp(k, null);
            if(s != null)
                System.out.println(node.getName() + "." + k + " = " + s);
            return s == null ? null : rewritePorts(parse(s));
        }
        void addCode() {
            var computeName = "compute_" + node.getUid();
            inputs.forEach(in -> {
                var t = in.getType();
                var arg = ide(in.port.getName());
                var gvar = ide(in.uname);
                functionInfo.put(in.fname,
                    new FunctionInfo(in.fname, node, t,
                        ife(NE(gvar, arg),
                            block(
                                assign(gvar, arg),
                                invoke(ide(computeName)))),
                        arg));
            });
            outputs.forEach(out -> {
                var t = out.getType();
                var arg = ide(out.port.getName());
                var gvar = ide(out.uname);
                var body = new ArrayList<Expression>();
                body.add(assign(gvar, arg));
                var callsrc = arg.isLeaf() ? arg : gvar;
                for(var arc:out.port.allArcs()) {
                    var in = arc.otherEnd(out.port);
                    var ininfo = globals.get(in);
                    if(ininfo!=null)
                    body.add(invoke(ide(ininfo.fname),callsrc));
                    else System.out.println("  Missing info for "+in);
                }
                functionInfo.put(out.fname,
                    new FunctionInfo(out.fname, node, t,
                        block(body), arg));
            });
            functionInfo.put(computeName,
                new FunctionInfo(computeName, node,
                    code));
        }
        Expression rewritePorts(Expression e) {
            return e.rewrite((Expression exNode) -> {
                var op = exNode.getOperator();
                if(op.isIdentifier()) {
                    System.out.println("Found Identifier " + exNode);
                    var foundValue = portExpressions.get(op.getBody());
                    if(foundValue != null) {
                        System.out.println("  with value " + foundValue);
                        return foundValue;
                    }
                }
                return exNode;
            });
        }
        private void addSetup(Expression e) {
            if(e != null)
                if(e.getOperator() == Vocabulary.BLOCK)
                    e.forEach(f -> addSetup(f));
                else if(e.getOperator() == Vocabulary.DECLARE) {
                    var type = e.getType();
                    var initialValue = (Expression) null;
                    var name = (Expression) null;
                    var isFinal = false;
                    for(var arg: e.asArray()) {
                        var op = arg.getOperator();
                        if(arg == Vocabulary.finalMarker) isFinal = true;
                        else if(name == null) name = arg;
                        else initialValue = arg;
                    }
                    if(type == Type.unknown && initialValue != null)
                        type = initialValue.getType();
                    if(name != null) {
                        declarations.add(declare(type, name, null, isFinal));
                        if(initialValue != null)
                            setup.add(assign(name, initialValue));
                    } else System.out.println("Malformed declaratikon: " + e);
                } else setup.add(e);
        }
    }
    static boolean isEmpty(Expression e) {
        return e == null || e.length() == 0 && e.getOperator() == Vocabulary.BLOCK;
    }
    static Expression exp(Token t, Expression... a) {
        return Expression.of(t, a);
    }
    static Expression exp(Token t, Stream<Expression> a) {
        return Expression.of(t, a);
    }
    static Expression exp(Token t, Collection a) {
        return Expression.of(t, a);
    }
    static Expression block(Expression... a) {
        var args = new ArrayList<Expression>();
        for(var f:a) collectBlock(args,f);
        return exp(Vocabulary.BLOCK, args);
    }
    static Expression block(Collection<Expression> a) {
        var args = new ArrayList<Expression>();
        a.forEach(f->collectBlock(args,f));
        return exp(Vocabulary.BLOCK, args);
    }
    static Expression invoke(Expression... a) {
        return exp(Vocabulary.INVOKE, a);
    }
    static Expression declare(Type t, Expression name, Expression init, boolean finale) {
        return exp(Vocabulary.DECLARE, name, init, finale ? Vocabulary.finalMarker : null).setType(t);
    }
    static Expression declare(Type t, Expression name) {
        return exp(Vocabulary.DECLARE, name).setType(t);
    }
    static Expression assign(Expression name, Expression value) {
        return exp(Vocabulary.ASSIGN, name, value);
    }
    static Expression NE(Expression a, Expression b) {
        return exp(Vocabulary.NE, a, b);
    }
    static Expression ide(String s) {
        return exp(Token.identifier(s));
    }
    static Expression ife(Expression cond, Expression then, Expression els) {
        return exp(Vocabulary.IF, cond, then, els);
    }
    static Expression ife(Expression cond, Expression then) {
        return exp(Vocabulary.IF, cond, then);
    }
    private static void collectBlock(Collection<Expression> target, Expression e) {
        if(!isEmpty(e))
            if(e.getOperator() == Vocabulary.BLOCK)
                e.forEach(f -> collectBlock(target, f));
            else target.add(e);
    }

    public class PortInfo {
        final Expression value;
        final Port port;
        final String uname;
        final String fname;
        final boolean constant;
        PortInfo(Port p) {
            port = p;
            uname = p.within.getUid() + "_" + p.getName();
            fname = (p.isInputSide() ? "set" : "sendFrom") + capitalize(uname);
            var t = p.getType();
            var k = p.getName();
            if(p.isConnected()) {
                constant = false;
                value = ide(fname);
            } else {
                constant = true;
                var nv = p.getValue();
                System.out.println("Constant port " + p.getFullName() + " = " + nv);
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
                value = v;
            }
        }
        public void addRewrites(Map<String, Expression> m) {
            m.put(port.getName(), value);
        }
        boolean isInput() {
            return !constant && port.isInputSide();
        }
        boolean isOutput() {
            return !constant && port.isOutputSide();
        }
        public Type getType() {
            return port.getType();
        }
    }
    public static String capitalize(String s) {
        // should probably be moved to somewhere like Utils
        if(s == null || s.length() == 0) return s;
        var c0 = s.charAt(0);
        return Character.isLowerCase(c0)
            ? Character.toUpperCase(c0) + s.substring(1)
            : s;
    }

    public class FunctionInfo {
        final String name;
        final Node node;
        Expression body;
        Expression[] args;
        Type returnType;
        FunctionInfo(String nm, Node n, Type t, Expression b, Expression... a) {
            name = nm;
            node = n;
            returnType = t;
            body = b;
            args = a;
        }
        FunctionInfo(String nm, Node n, Expression b, Expression... a) {
            this(nm, n, Type.voidType, b, a);
        }
        boolean inlineFunctions() {
            if(body == null) return false;
            var nb = body.rewrite((Expression n) -> {
                if(n.getOperator() != Vocabulary.INVOKE || n.length() != 1)
                    return n;
                var target = n.arg(0).getOperator();
                if(!target.isIdentifier()) return n;
                var name = target.getBody();
                var fi = functionInfo.get(name);
                System.out.println("Thinking of inlining " + name + " " + fi);
                return fi != null ? fi.body : n;
            });
            if(nb == body) return false;
            body = nb;
            return true;
        }
    }

    private Expression parse(String s) {
        try {
            if(s.indexOf(';') >= 0)
                s = "{" + s + ";}";
            return new Parser(new Tokenizer(s)).expression().clean();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
            error("Error parsing " + s + ": " + ex.toString());
            return Expression.of(Token.string(s)).clean();
        }
    }
    private void error(String s) {
        if(errors == null) errors = new ArrayList<>();
        errors.add(s);
    }
    private List<String> errors;
    private static final Token emptyString = Token.string("");
}
