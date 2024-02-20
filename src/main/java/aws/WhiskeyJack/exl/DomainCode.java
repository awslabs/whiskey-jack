/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import static aws.WhiskeyJack.exl.DomainCode.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.nodegraph.Node;
import java.io.*;
import java.util.*;
import java.util.function.*;
import static aws.WhiskeyJack.exl.Expression.*;
import java.util.concurrent.atomic.*;

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
 *
 * One of the subtleties is the assumption that all variable names are globally
 * unique. Even local variables.
 */
public class DomainCode {
    final Map<String, NodeCode> codes = new HashMap<>();
    /* Every domain ends up with three pools of code fragments: */
    final Map<String, FunctionInfo> functionInfo = new HashMap<>();
    Expression declarations = block(); // globals
    Expression setup = block(); // initialization
    final Map<Port, PortInfo> globals = new HashMap<>();
    final Map<String, AtomicInteger> uniqueNameMap = new HashMap<>();
    final Set<String> usedVars = new HashSet<>();
    boolean usageValid = false;
    Domain domain = Domain.err;
    public DomainCode(Collection<Node> nodes) {
        nodes.forEach(n -> codes.put(n.getUid(), new NodeCode(n)));
        codes.values().forEach(nc -> nc.addCode());
    }
    static final private ExpressionEvolutionCollector out = new ExpressionEvolutionCollector();
    private boolean changed;
    public void optimize() {
        out.dump(this);
        int laps = 0;
        do {
            laps++;
            System.out.println("Lap " + laps);
            changed = false;
            functionInfo.values().forEach(fi -> fi.inlineBodyFunctions());
            declarations = nonnull(inlineFunctions(declarations));
            setup = nonnull(inlineFunctions(setup));
            if(changed) out.dump(this);
        } while(changed && laps < 5);
        markUsedFunctions();
        markUsedVariables();
        usageValid = true;
        eliminateDeadGlobalAssignments();
        out.dump(this);
    }

    public void markUsedFunctions() {
        var toMark = new LinkedList<FunctionInfo>();
        functionInfo.values().forEach(fi -> fi.used = false);
        setup.forEach(s -> markUsed(s, toMark));
        FunctionInfo fi;
        while((fi = toMark.poll()) != null)
            markUsed(fi.body, toMark);
    }
    private void markUsed(Expression e, Queue<FunctionInfo> work) {
        if(e != null)
            e.visit(ce -> {
                if(ce.getOperator() == Vocabulary.INVOKE && !ce.isEmpty()) {
                    var func = ce.getFirst();
                    var funcOp = func.getOperator();
                    if(funcOp.isIdentifier()) {
                        var funcName = funcOp.getBody();
                        System.out.println("  Marking " + funcName);
                        var fi = functionInfo.get(funcName);
                        if(fi != null && !fi.used) {
                            fi.used = true;
                            work.add(fi);
                        }
                    }
                }
            });
    }
    private void markUsedVariables() {
        functionInfo.values().forEach(f -> {
            if(f.used)
                if(f.body != null)
                    markUsedVariables(f.body);
        });
        markUsedVariables(setup);
        markUsedVariables(declarations);
    }
    private void markUsedVariables(Expression e) {
        var skip = (Expression) null;
        var op = e.getOperator();
        if((op == Vocabulary.ASSIGN || op == Vocabulary.DECLARE) && !e.isEmpty()) {
            var lhs = e.getFirst();
            if(lhs.getOperator().isIdentifier()) skip = lhs;
        }
        if(op.isIdentifier()) {
            if(usedVars.add(op.getBody()))
                System.out.println("    Mark used "+op.getBody());
        }
        for(var a: e)
            if(a != skip) markUsedVariables(a);
    }
    public boolean used(String name) { return !usageValid || usedVars.contains(name); }
    public boolean used(Expression e) {
        if(e==null) return false;
        if(!usageValid) return true;
        var op = e.getOperator();
        if(op.isIdentifier()) return used(op.getBody());
        if(op==Vocabulary.DECLARE && !e.isEmpty())
            return used(e.getFirst());
        return true;
    }
    private void eliminateDeadGlobalAssignments() {
        functionInfo.values().forEach(fi -> fi.body = eliminateDeadGlobalAssignments(fi.body));
        setup = nonnull(eliminateDeadGlobalAssignments(setup));
    }
    private Expression eliminateDeadGlobalAssignments(Expression E) {
        return E==null ? null : E.rewrite(e->{
            var op = e.getOperator();
            if(op==Vocabulary.ASSIGN && !used(e.getFirst())) {
                assert(e.size()==2);
                return e.getLast().reduceToSideEffects();
            }
            return e;
        });
    }
    private Expression nonnull(Expression e) {
        return e==null ? block() : e;
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
                System.out.println("Adding declaration <" + txt(pi.varExpr) + ">");
                addSetup(declare(p.getType(), pi.varExpr));
                if(pi.isInput())
                    inputs.add(pi);
                if(pi.isOutput()) outputs.add(pi);
            });
            portInfo.values().forEach(pi -> pi.addRewrites(portExpressions));
            code = codeProp("code");
            addSetup(codeProp("state"));
            addSetup(codeProp("callback"));
        }
        final Expression codeProp(String k) {
            var s = node.getStringProp(k, null);
            if(s == null) return null;
            System.out.println(node.getName() + "." + k + " = " + s);
            var r0 = parse(s);
            var ret = s == null ? null : rewritePorts(r0);
            System.out.println("Port rewrite "+txt(r0)+" => "+txt(ret));
            portExpressions.forEach((var,value)->System.out.println("\t"+var+" => "+txt(value)));
            return ret;
        }
        void addCode() {
            var computeName = "compute_" + node.getUid();
            inputs.forEach(in -> {
                var t = in.getType();
                var arg = ide(in.port.getName());
                functionInfo.put(in.funcName,
                    new FunctionInfo(in.funcName, t,
                        ife(NE(in.varExpr, arg),
                            block(
                                assign(in.varExpr, arg),
                                invoke(ide(computeName)))),
                        arg));
            });
            outputs.forEach(out -> {
                var t = out.getType();
                var arg = ide(out.port.getName());
                var body = new ArrayList<Expression>();
                body.add(assign(out.varExpr, arg));
                var callsrc = arg.isLeaf() ? arg : out.varExpr;
                for(var arc: out.port.allArcs()) {
                    var in = arc.otherEnd(out.port);
                    var ininfo = globals.get(in);
                    if(ininfo != null)
                        body.add(invoke(ininfo.funcExpr, callsrc));
                    else System.out.println("  Missing info for " + in);
                }
                functionInfo.put(out.funcName,
                    new FunctionInfo(out.funcName, t,
                        block(body), arg));
            });
            functionInfo.put(computeName,
                new FunctionInfo(computeName, code));
        }
        Expression rewritePorts(Expression e) {
            return e.rewrite(new Rewriter() { // can't be λ because of 'this' ref
                @Override
                public Expression rewrite(Expression exNode) {
                    var op = exNode.getOperator();
                    if(op.isIdentifier()) {
                        System.out.println("Found Identifier " + exNode);
                        var foundValue = portExpressions.get(op.getBody());
                        if(foundValue != null) {
                            System.out.println("  with value " + foundValue);
                            return foundValue.rewrite(this);
                        }
                    }
                    return exNode;
                }
            });
        }
        private void addSetup(Expression e) {
            if(e != null)
                if(e.getOperator() == Vocabulary.BLOCK)
                    e.forEach(f -> addSetup(f));
                else if(DeclarationInfo.of(e) instanceof DeclarationInfo decl)
                    if(decl.name != null) {
                        declarations.add(declare(decl.type, decl.name, null, decl.isFinal));
                        if(decl.initialValue != null)
                            setup.add(assign(decl.name, decl.initialValue));
                    } else
                        System.out.println("Malformed declaration: " + decl.name + " " + txt(e));
                else setup.add(e);
        }
    }
    static Expression block(Expression... a) {
        var ret = of(Vocabulary.BLOCK);
        for(var f: a)
            ret.add(f);
        return ret;
    }
    static Expression block(Collection<Expression> a) {
        var ret = of(Vocabulary.BLOCK);
        for(var f: a)
            ret.add(f);
        return ret;
    }
    static Expression invoke(Expression... a) {
        return of(Vocabulary.INVOKE, a);
    }
    static Expression comment(String comment) {
        var ret = of(Vocabulary.COMMENT).add1(of(Token.string(comment)));
        System.out.println("COMMENT "+ret+"\n\t"+comment);
        return ret;
    }
    static Expression declare(Type t, Expression name, Expression init, boolean finale) {
        if(init != null && (t == null || t == Type.any)) t = init.getType();
        if(t == null) t = Type.any;
        return of(Vocabulary.DECLARE, name, init, finale ? Vocabulary.finalMarker : null).setType(t);
    }
    static Expression declare(Type t, Expression name) {
        return declare(t, name, null, false);
    }
    static Expression assign(Expression name, Expression value) {
        return of(Vocabulary.ASSIGN, name, value);
    }
    static Expression NE(Expression a, Expression b) {
        return of(Vocabulary.NE, a, b);
    }
    static Expression ide(String s) {
        return of(Token.identifier(s));
    }
    static Expression ife(Expression cond, Expression then, Expression els) {
        return of(Vocabulary.IF, cond, then, els);
    }
    static Expression ife(Expression cond, Expression then) {
        return ife(cond, then, null);
    }
    public String uniqueName(String s) {
        var ci = uniqueNameMap.computeIfAbsent(s, n -> new AtomicInteger(0));
        var ctr = ci.addAndGet(1);
        return s + '_' + ctr;
    }

    public static class DeclarationInfo {
        Expression name;
        Expression initialValue;
        boolean isFinal;
        Type type = Type.any;
        public static DeclarationInfo of(Expression e) {
            return e.getOperator() != Vocabulary.DECLARE ? null
                : new DeclarationInfo(e);
        }
        private DeclarationInfo(Expression e) {
            type = e.getType();
            for(var arg: e)
                if(arg == Vocabulary.finalMarker) isFinal = true;
                else if(name == null) name = arg;
                else initialValue = arg;
        }
    }

    private class PortInfo {
        final Expression value;
        final Port port;
        final Expression varExpr;
        final String funcName;
        final Expression funcExpr;
        final boolean constant;
        PortInfo(Port p) {
            port = p;
            var varName = uniqueName(p.getName());
            varExpr = ide(varName);
            funcName = capitalize(varName);
            funcExpr = ide(funcName);
            var t = p.getType();
            var k = p.getName();
            if(p.isConnected()) {
                constant = false;
                value = port.isInputSide() ? varExpr : funcExpr;//??ide(funcName);
            } else {
                constant = true;
                var nv = p.getValue();
                System.out.println("Constant port " + p.getFullName() + " = " + nv);
                var v = switch(nv) {
                    case String str ->
                        t == Type.string
                        ? of(Token.string(str))
                        : parse(str);
                    case Expression ev ->
                        ev;
                    case Number num ->
                        of(Token.number(num));
                    case null ->
                        of(t == Type.string ? emptyString : Vocabulary.NULL);
                    default ->
                        of(Token.string(String.valueOf(nv)));
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
        Expression body;
        Expression[] args;
        Type returnType;
        private boolean used = true;
        FunctionInfo(String nm, Type t, Expression b, Expression... a) {
            name = nm;
            returnType = t;
            body = b;
            args = a;
        }
        FunctionInfo(String nm, Expression b, Expression... a) {
            this(nm, Type.voidType, b, a);
        }
        void inlineBodyFunctions() {
            var nb = inlineFunctions(body);
            if(nb != body) {
                changed = true;
                body = nb;
            }
        }
        boolean isUsed() { return used; }
        @Override
        public String toString() {
            return name + "(...)";
        }
    }
    private Expression inlineFunctions(Expression body) {
        if(body == null) return null;
        return body.rewrite((Expression n) -> {
            if(n.getOperator() != Vocabulary.INVOKE)
                return n;
            System.out.println("Looking at func " + n);
            var target = n.get(0).getOperator();
            if(!target.isIdentifier()) return n;
            var name = target.getBody();
            var fi = functionInfo.get(name);
            if(fi == null) return n;
            if(isEmpty(fi.body)) {
                System.out.println("\tEmpty body");
                return null;
            }
            var nargs = fi.args.length;
            if(nargs != n.size() - 1) return n; // arg length mismatch
            /* we have an invocation where none of the arguments can have side effects */
            var argSubstitutions = new HashMap<String, Expression>();
            var newBody = block();
            for(var i = 0; i < nargs; i++) {
                var formalParameter = fi.args[i];
                var formalParameterName = formalParameter.getOperator().getBody();
                var actualParameter = n.get(i + 1);
                if(actualParameter.complex()) {
                    var localCaptureName = uniqueName(formalParameterName);
                    var localCaptureExpression = ide(localCaptureName);
                    newBody.add(declare(null, localCaptureExpression, actualParameter, true));
                    argSubstitutions.put(formalParameterName, localCaptureExpression);
                } else
                    if(!formalParameter.equals(actualParameter))
                        argSubstitutions.put(formalParameterName, actualParameter);
                    else System.out.println("rename to self blocked: "+formalParameter);
            }
            System.out.println("rename map:");
            argSubstitutions.forEach((k, v) ->
                System.out.println("\t" + k + ":=" + txt(v)));
            newBody.add(fi.body.rename(argSubstitutions));
            var ret = clean(newBody);
            System.out.println("   Result is " + txt(ret));
            return ret;
        });
    }
    public static String txt(Expression e) {
        var sb = new StringBuilder();
        var ed = new VerboseDump().to(sb);
        ed.append(e);
        return sb.toString().replaceAll("\n *|  +", " ");
    }

    private Expression parse(String s) {
        try {
            if(s.indexOf(';') >= 0)
                s = "{" + s + ";}";
            return new Parser(new Tokenizer(s)).expression().clean();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
            error("Error parsing " + s + ": " + ex.toString());
            return of(Token.string(s)).clean();
        }
    }
    private void error(String s) {
        if(errors == null) errors = new ArrayList<>();
        errors.add(s);
    }
    private List<String> errors;
    private static final Token emptyString = Token.string("");
}
